rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(openxlsx)
library(dplyr)
library(tidyverse)

## prepares raw data and creates dataset for visualization and analyses

setwd("~/Dropbox")
setwd("climate-smart-landscapes-saved")

climatesurvey <- read.csv("rawData/climatedata.csv",
                     stringsAsFactors=FALSE)

## some useful functions for cleaning/exporting data
source("../climate-smart-landscapes/dataPrepAndVis/src/misc.R")

## we'll put the cleaned data and products in these tables
dirClean <- "cleanedData"
dirSummary <- "summaryData"
dirTables <- "tables"
dirFigs <- "figs"

## ***********************************************************
## drop columns
## ***********************************************************

dim(climatesurvey)

## drop the extra columns
climatesurvey$StartDate <- NULL
climatesurvey$EndDate <- NULL
climatesurvey$Status <- NULL
climatesurvey$Progress <- NULL
climatesurvey$Duration..in.seconds. <- NULL
climatesurvey$IPAddress <- NULL
climatesurvey$RecipientLastName <- NULL
climatesurvey$RecipientFirstName <- NULL
climatesurvey$RecipientEmail <- NULL
climatesurvey$ExternalReference <- NULL
climatesurvey$LocationLatitude <- NULL
climatesurvey$LocationLongitude <- NULL
climatesurvey$DistributionChannel <- NULL
climatesurvey$UserLanguage <- NULL
climatesurvey$Q23. <- NULL
climatesurvey$Q1_12_TEXT <- NULL
climatesurvey$Q6_12_TEXT <- NULL
climatesurvey$Q16_8_TEXT <- NULL
climatesurvey$Q18_6_TEXT <- NULL


dim(climatesurvey)

## drop the extra rows
climatesurvey <- climatesurvey[-c(1,2), ]

## drop all surveys that were not completed
## climatesurvey <- climatesurvey[climatesurvey$Finished != FALSE, ]
## dim(climatesurvey)
## dropped about 52 surveys

## For response ID, make sure string of numbers is read as characters
climatesurvey$ResponseId <- as.character(climatesurvey$ResponseId)

## ***********************************************************
## export
## ***********************************************************

## add NAs in blank cells
climatesurvey <- climatesurvey %>% mutate_all(na_if,"")

## export
write.csv(climatesurvey, file=file.path(dirClean,
                                     "climatesurvey_cleaned.csv"), row.names=FALSE)

## ***********************************************************
## summary info, with NAs from unfinished surveys
## ***********************************************************

## Number of respondents
numRespondents <- tally(climatesurvey, sort = FALSE, name = NULL)
## 269 respondents at this time

## List of unique counties with respondents
## I NEED TO DROP THE NAs STILLL AHHHH
listCounties <- unique(na.omit(climatesurvey$Q3))

## Count of unique counties with respondents
numCounties <- length(unique(na.omit(climatesurvey$Q3)))
## 32 counties at this time

## Count of responents who didn't finish the survey
numIncompleteSurveys <- length(climatesurvey[climatesurvey$Finished != FALSE, ])
## 51 surveys not complete

## export
summarydata <- list('numRespondents' = numRespondents, 
                    'listCounties' = listCounties, 
                    'numCounties' = numCounties,
                    'numIncompleteSurveys' = numIncompleteSurveys)

write.xlsx(summarydata, file=file.path(dirSummary,
                                      "climatesurvey_summary.xlsx"), rowNames=FALSE)

## *******************************************************************************
## TABLES, including NAs in each table
## *******************************************************************************


## Q1. Please select the roles that best describes you
## this is ugly because allowed participated to "select all that apply."

Q1participantRole <- climatesurvey %>%
  separate_rows(Q1, sep = ',\\s*') %>%
  group_by(Q1) %>% 
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q2. How long have you been in this industry
Q2Experience <- climatesurvey %>%
  group_by(Q2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Number of respondents per county, ranked (REMOVED CUZ ITS BELOW ALSO)
## RespondentsPerCounty <- count(climatesurvey, climatesurvey$Q3, wt = NULL, sort = TRUE, name = NULL)

## Q3. Number and Percent of respondents per county, ranked
Q3percRespondentsPerCounty <- climatesurvey %>%
  group_by(Q3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q4. Do you hold a management role
Q4mgmtRole <- climatesurvey %>%
  group_by(Q4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q5. Does your company market at climate friendly
Q5marketClimate <- climatesurvey %>%
  group_by(Q5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q6. Where do you receive information about climate change
Q6climateInfo <- climatesurvey %>%
  separate_rows(Q6, sep = ',\\s*') %>%
  group_by(Q6) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q7. Do you believe human activity is the primary cause of climate change
Q7humansClimateChange <- climatesurvey %>%
  group_by(Q7) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q8. Assuming that climate change is happening, do you believe it is caused by
Q8whyClimateChange <- climatesurvey %>%
  group_by(Q8) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q10 Do you believe that climate change is relevant to your business operations, clients, and properties you serve in Florida?
Q10ClimateChangeRelevancy <- climatesurvey %>%
  group_by(Q10) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q12.Please read the following statements related to climate change and indicate whether you believe the statement is true or false
Q12Fact1 <- climatesurvey %>%
  group_by(Q12_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact2 <- climatesurvey %>%
  group_by(Q12_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact3 <- climatesurvey %>%
  group_by(Q12_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact4 <- climatesurvey %>%
  group_by(Q12_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact5 <- climatesurvey %>%
  group_by(Q12_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q14. What is the biggest obstacle to implementing climate-friendly practices? Please rank in order of importance, with 1=biggest obstacle and 5=smallest obstacle.

Q14obstacleRankCost <- climatesurvey %>%
  group_by(Q14_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankKnwldge <- climatesurvey %>%
  group_by(Q14_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankTools <- climatesurvey %>%
  group_by(Q14_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankInterest <- climatesurvey %>%
  group_by(Q14_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankPriority <- climatesurvey %>%
  group_by(Q14_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )


## Q17. Highest level education
Q17Education <- climatesurvey %>%
  group_by(Q17) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q18. Which professional memberships do you currently hold?
Q18Associations <- climatesurvey %>%
  separate_rows(Q18, sep = ',\\s*') %>%
  group_by(Q18) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q19. Do you have a pesticide license from the Florida Department of Agriculture andConsumer Sciences (FDACS)?
Q19Licensed <- climatesurvey %>%
  group_by(Q19) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q20. What is your sex
Q20Sex <- climatesurvey %>%
  group_by(Q20) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q21. Are you Hispanic or Latino?
Q21Hispanic <- climatesurvey %>%
  group_by(Q21) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q22. Which category(ies) best describes you?
Q22RaceEthnicity <- climatesurvey %>%
  separate_rows(Q22, sep = ',\\s*') %>%
  group_by(Q22) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## export tables

tabledata <- list('Q1participantRole' = Q1participantRole, 
                  'Q2Experience' = Q2Experience, 
                  'Q3percRespondentsPerCounty' = Q3percRespondentsPerCounty,
                  'Q4mgmtRole' = Q4mgmtRole,
                  'Q5marketClimate' = Q5marketClimate,
                  'Q6climateInfo' = Q6climateInfo,
                  'Q7humansClimateChange' = Q7humansClimateChange,
                  'Q8whyClimateChange' = Q8whyClimateChange,
                  'Q10ClimateChangeRelevancy' = Q10ClimateChangeRelevancy,
                  'Q12Fact1' = Q12Fact1,
                  'Q12Fact2' = Q12Fact2,
                  'Q12Fact3' = Q12Fact3,
                  'Q12Fact4' = Q12Fact4,
                  'Q12Fact5' = Q12Fact5,
                  'Q14obstacleRankCost' = Q14obstacleRankCost,
                  'Q14obstacleRankKnwldge' = Q14obstacleRankKnwldge,
                  'Q14obstacleRankTools' = Q14obstacleRankTools,
                  'Q14obstacleRankInterest' = Q14obstacleRankInterest,
                  'Q14obstacleRankPriority' = Q14obstacleRankPriority,
                  'Q17Education' = Q17Education,
                  'Q18Associations' = Q18Associations,
                  'Q19Licensed' = Q19Licensed,
                  'Q20Sex' = Q20Sex,
                  'Q21Hispanic' = Q21Hispanic,
                  'Q22RaceEthnicity' = Q22RaceEthnicity)
                    

write.xlsx(tabledata, file=file.path(dirTables,
                                       "tables.xlsx"), rowNames=FALSE)


## when participants select multiple that apply...
## i still need to figure out how to table summarize that better

## still need to make tables with the NAs as well

## **************************************************************************************
## FIGURE - Q9 Do you think that green industry should be doing more less (NAs removed)
## **************************************************************************************

## **************************************************************************************
## FIGURE - Q11 Please evaluate the following statements (attitudes) (NAs removed)
## **************************************************************************************

## **************************************************************************************
## FIGURE - Q13 Please evaluate the following statements (incentives) (NAs removed)
## **************************************************************************************


## **************************************************************************************
## TABLES, using data with dropped NAs
## **************************************************************************************


## Q1. Please select the roles that best describes you
## this is ugly because allowed participated to "select all that apply."
Q1participantRole <- climatesurvey %>%
  separate_rows(Q1, sep = ',\\s*') %>%
  filter(!is.na(Q1)) %>%
  group_by(Q1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q2. How long have you been in this industry
Q2Experience <- climatesurvey %>%
  filter(!is.na(Q2)) %>%
  group_by(Q2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q3. Number and Percent of respondents per county, ranked
Q3percRespondentsPerCounty <- climatesurvey %>%
  filter(!is.na(Q3)) %>%
  group_by(Q3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q4. Do you hold a management role
Q4mgmtRole <- climatesurvey %>%
  filter(!is.na(Q4)) %>%
  group_by(Q4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q5. Does your company market at climate friendly
Q5marketClimate <- climatesurvey %>%
  filter(!is.na(Q5)) %>%
  group_by(Q5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )


## Q6. Where do you receive information about climate change
Q6climateInfo <- climatesurvey %>%
  separate_rows(Q6, sep = ',\\s*') %>%
  filter(!is.na(Q6)) %>%
  group_by(Q6) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q7. Do you believe human activity is the primary cause of climate change
Q7humansClimateChange <- climatesurvey %>%
  filter(!is.na(Q7)) %>%
  group_by(Q7) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q8. Assuming that climate change is happening, do you believe it is caused by
Q8whyClimateChange <- climatesurvey %>%
  filter(!is.na(Q8)) %>%
  group_by(Q8) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q10 Do you believe that climate change is relevant to your business operations, clients, and properties you serve in Florida?
Q10ClimateChangeRelevancy <- climatesurvey %>%
  filter(!is.na(Q10)) %>%
  group_by(Q10) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q12.Please read the following statements related to climate change and indicate whether you believe the statement is true or false
Q12Fact1 <- climatesurvey %>%
  filter(!is.na(Q12_1)) %>%
  group_by(Q12_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact2 <- climatesurvey %>%
  filter(!is.na(Q12_2)) %>%
  group_by(Q12_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact3 <- climatesurvey %>%
  filter(!is.na(Q12_3)) %>%
  group_by(Q12_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact4 <- climatesurvey %>%
  filter(!is.na(Q12_4)) %>%
  group_by(Q12_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact5 <- climatesurvey %>%
  filter(!is.na(Q12_5)) %>%
  group_by(Q12_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q14. What is the biggest obstacle to implementing climate-friendly practices? Please rank in order of importance, with 1=biggest obstacle and 5=smallest obstacle.

Q14obstacleRankCost <- climatesurvey %>%
  group_by(Q14_1) %>%
  filter(!is.na(Q14_1)) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankKnwldge <- climatesurvey %>%
  filter(!is.na(Q14_2)) %>%
  group_by(Q14_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankTools <- climatesurvey %>%
  filter(!is.na(Q14_3)) %>%
  group_by(Q14_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankInterest <- climatesurvey %>%
  filter(!is.na(Q14_4)) %>%
  group_by(Q14_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankPriority <- climatesurvey %>%
  filter(!is.na(Q14_5)) %>%
  group_by(Q14_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )


## Q17. Highest level education
Q17Education <- climatesurvey %>%
  filter(!is.na(Q17)) %>%
  group_by(Q17) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q18. Which professional memberships do you currently hold?
Q18Associations <- climatesurvey %>%
  separate_rows(Q18, sep = ',\\s*') %>%
  filter(!is.na(Q18)) %>%
  group_by(Q18) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q19. Do you have a pesticide license from the Florida Department of Agriculture andConsumer Sciences (FDACS)?
Q19Licensed <- climatesurvey %>%
  filter(!is.na(Q19)) %>%
  group_by(Q19) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q20. What is your sex
Q20Sex <- climatesurvey %>%
  filter(!is.na(Q20)) %>%
  group_by(Q20) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q21. Are you Hispanic or Latino?
Q21Hispanic <- climatesurvey %>%
  filter(!is.na(Q21)) %>%
  group_by(Q21) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q22. Which category(ies) best describes you?
Q22RaceEthnicity <- climatesurvey %>%
  separate_rows(Q22, sep = ',\\s*') %>%
  filter(!is.na(Q22)) %>%
  group_by(Q22) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## export tables

tabledataOmitNA <- list('Q1participantRole' = Q1participantRole, 
                  'Q2Experience' = Q2Experience, 
                  'Q3percRespondentsPerCounty' = Q3percRespondentsPerCounty,
                  'Q4mgmtRole' = Q4mgmtRole,
                  'Q5marketClimate' = Q5marketClimate,
                  'Q6climateInfo' = Q6climateInfo,
                  'Q7humansClimateChange' = Q7humansClimateChange,
                  'Q8whyClimateChange' = Q8whyClimateChange,
                  'Q10ClimateChangeRelevancy' = Q10ClimateChangeRelevancy,
                  'Q12Fact1' = Q12Fact1,
                  'Q12Fact2' = Q12Fact2,
                  'Q12Fact3' = Q12Fact3,
                  'Q12Fact4' = Q12Fact4,
                  'Q12Fact5' = Q12Fact5,
                  'Q14obstacleRankCost' = Q14obstacleRankCost,
                  'Q14obstacleRankKnwldge' = Q14obstacleRankKnwldge,
                  'Q14obstacleRankTools' = Q14obstacleRankTools,
                  'Q14obstacleRankInterest' = Q14obstacleRankInterest,
                  'Q14obstacleRankPriority' = Q14obstacleRankPriority,
                  'Q17Education' = Q17Education,
                  'Q18Associations' = Q18Associations,
                  'Q19Licensed' = Q19Licensed,
                  'Q20Sex' = Q20Sex,
                  'Q21Hispanic' = Q21Hispanic,
                  'Q22RaceEthnicity' = Q22RaceEthnicity)


write.xlsx(tabledataOmitNA, file=file.path(dirTables,
                                     "tablesOmitNAs.xlsx"), rowNames=FALSE)



