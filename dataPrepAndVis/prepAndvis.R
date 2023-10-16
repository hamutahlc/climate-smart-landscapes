rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(openxlsx)
library(tidyverse)
library(ggpubr)
library(scales)
library(wesanderson)

## prepares raw data and creates dataset for visualization and analyses

setwd("~/Dropbox")
setwd("climate-smart-landscapes-saved")

climatesurvey <- read.csv("rawData/climatedata2023.csv",
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
climatesurvey$RecordedDate <- NULL
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

## drop all surveys that were not completed, which remove the NAs
climatesurveydropNAs <- climatesurvey[climatesurvey$Finished != FALSE, ]
## dim(climatesurveydropNA)
## dropped 53 surveys

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
## 323 respondents at this time

## List of unique counties with respondents
## I NEED TO DROP THE NAs STILLL AHHHH
listCounties <- unique(na.omit(climatesurvey$Q3))

## Count of unique counties with respondents
numCounties <- length(unique(na.omit(climatesurvey$Q3)))
## 32 counties at this time

## Count of responents who didn't finish the survey
numIncompleteSurveys <- length(climatesurvey[climatesurvey$Finished != FALSE, ])
## 46 surveys not complete

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

## Q15. What is your preferred format for receiving extension education? Please rank in order of importance, with 1=most preferred and 5=leastpreferred.

Q15formatRankWrittenMaterials <- climatesurvey %>%
  group_by(Q15_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankInPersonLec <- climatesurvey %>%
  group_by(Q15_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankVirtualLec <- climatesurvey %>%
  group_by(Q15_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankWorkshops <- climatesurvey %>%
  group_by(Q15_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankInformal <- climatesurvey %>%
  group_by(Q15_5) %>%
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
                  'Q15formatRankWrittenMaterials' = Q15formatRankWrittenMaterials,
                  'Q15formatRankInPersonLec' = Q15formatRankInPersonLec,
                  'Q15formatRankVirtualLec' = Q15formatRankVirtualLec,
                  'Q15formatRankWorkshops' = Q15formatRankWorkshops,
                  'Q15formatRankInformal' = Q15formatRankInformal,
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


## **************************************************************************************
## FIGURE - Q9 Do you think that green industry should be doing more less (NAs removed)
## **************************************************************************************

# rename responses
climatesurvey$Q9 <- fct_recode(climatesurvey$Q9,
                               "Currently enough" = "Currently doing the right amount")
# reorder
climatesurvey$Q9 <- factor(climatesurvey$Q9,
                           levels = c("Much less", "Less", "Currently enough", "More", "Much more"))


# plot
Q9plot <- climatesurvey %>% 
  drop_na(Q9) %>%
  count(Q9 = factor(Q9)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q9, y = pct, fill = Q9, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("") + 
  ylab("Percentage") +
  theme_pubr() +
  coord_flip()

# clean plot
Q9plot + theme(axis.text.y   = element_text(size=14),
               axis.text.x   = element_text(size=14, angle=45, vjust=0.5),
               axis.title.y  = element_text(size=14),
               axis.title.x  = element_text(size=14),
               panel.background = element_blank(),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.border = element_rect(colour = "black", fill=NA, size=0.75),
               legend.position = "none")

ggsave("figs/Q9ShouldGreenIndustryDoMoreToAddressClimateChange.pdf",
       height=4, width=5)

## **************************************************************************************
## FIGURE - Q11 Please evaluate the following statements (attitudes) (NAs removed)
## **************************************************************************************

# reorder
climatesurvey$Q11_1 <- factor(climatesurvey$Q11_1,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q11_2 <- factor(climatesurvey$Q11_2,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q11_3 <- factor(climatesurvey$Q11_3,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q11_4 <- factor(climatesurvey$Q11_4,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q11_5 <- factor(climatesurvey$Q11_5,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q11_6 <- factor(climatesurvey$Q11_6,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))



### Q11_1
# plot
Q11_1plot <- climatesurvey %>% 
  drop_na(Q11_1) %>%
  count(Q11_1 = factor(Q11_1)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_1, y = pct, fill = Q11_1, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I am concerned\nabout climate change") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_1plot <- Q11_1plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q11_1.pdf",
       height=4, width=12)


### Q11_2
# plot
Q11_2plot <- climatesurvey %>% 
  drop_na(Q11_2) %>%
  count(Q11_2 = factor(Q11_2)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_2, y = pct, fill = Q11_2, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I want to learn more about \n how to slow down climate\nchange through my work") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_2plot <- Q11_2plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q11_2.pdf",
       height=4, width=12)


### Q11_3
# plot
Q11_3plot <- climatesurvey %>% 
  drop_na(Q11_3) %>%
  count(Q11_3 = factor(Q11_3)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_3, y = pct, fill = Q11_3, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I consider the carbon \nfootprint of the equipment \nI use for work") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_3plot <- Q11_3plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q11_3.pdf",
       height=4, width=12)




### Q11_4
# plot
Q11_4plot <- climatesurvey %>% 
  drop_na(Q11_4) %>%
  count(Q11_4 = factor(Q11_4)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_4, y = pct, fill = Q11_4, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("There are strategies and \ntechnologies that my\n company could adopt \n today to reduce \nclimate change") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_4plot <- Q11_4plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())
ggsave("figs/Q11_4.pdf",
       height=4, width=12)



### Q11_5
# plot
Q11_5plot <- climatesurvey %>% 
  drop_na(Q11_5) %>%
  count(Q11_5 = factor(Q11_5)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_5, y = pct, fill = Q11_5, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("Part of my role at work \n should be to prepare \nlandscapes for the impacts \nof climate change") + 
  ylab("Percentage") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_5plot <- Q11_5plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q11_5.pdf",
       height=4, width=12)



### Q11_6
# plot
Q11_6plot <- climatesurvey %>% 
  drop_na(Q11_6) %>%
  count(Q11_6 = factor(Q11_6)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q11_6, y = pct, fill = Q11_6, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I am concerned that \n increasing heat & humidity from \n climate change will result \n in heat-related illness\n in workers") + 
  ylab("Percentage") +
  theme_pubr() +
  coord_flip()

# clean plot
Q11_6plot <- Q11_6plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())


ggsave("figs/Q11_6.pdf",
       height=4, width=12)


# add all plots on single layout

ggarrange(Q11_1plot, Q11_2plot, Q11_3plot, Q11_4plot, Q11_5plot, Q11_6plot,
          common.legend = TRUE, legend = "top",
          align=c("v"),
          ncol = 2, nrow = 3)

ggsave("figs/Q11Attitudes.pdf",
       height=8, width=14)

## **************************************************************************************
## FIGURE - Q13 Please evaluate the following statements (incentives) (NAs removed)
## **************************************************************************************

# reorder
climatesurvey$Q13_1 <- factor(climatesurvey$Q13_1,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q13_2 <- factor(climatesurvey$Q13_2,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q13_3 <- factor(climatesurvey$Q13_3,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q13_4 <- factor(climatesurvey$Q13_4,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))
climatesurvey$Q13_5 <- factor(climatesurvey$Q13_5,
                              levels = c("Strongly disagree", "Somewhat disagree", "Undecided", "Somewhat agree", "Strongly agree"))


### Q13_1
# plot
Q13_1plot <- climatesurvey %>% 
  drop_na(Q13_1) %>%
  count(Q13_1 = factor(Q13_1)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q13_1, y = pct, fill = Q13_1, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("Obtaining a climate-friendly \ncertification is a business \nadvantage") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q13_1plot <- Q13_1plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q13_1.pdf",
       height=4, width=12)


### Q13_2
# plot
Q13_2plot <- climatesurvey %>% 
  drop_na(Q13_2) %>%
  count(Q13_2 = factor(Q13_2)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q13_2, y = pct, fill = Q13_2, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("Marketing my company\nas climate-friendly is \na business advantage") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q13_2plot <- Q13_2plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q13_2.pdf",
       height=4, width=12)


### Q13_3
# plot
Q13_3plot <- climatesurvey %>% 
  drop_na(Q13_3) %>%
  count(Q13_3 = factor(Q13_3)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q13_3, y = pct, fill = Q13_3, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I am interested in\nlearning more about\nhow climate change\npotentially impacts my\nbusiness") + 
  ylab("") +
  theme_pubr() +
  coord_flip()

# clean plot
Q13_3plot <- Q13_3plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_blank(),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q13_3.pdf",
       height=4, width=12)




### Q13_4
# plot
Q13_4plot <- climatesurvey %>% 
  drop_na(Q13_4) %>%
  count(Q13_4 = factor(Q13_4)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q13_4, y = pct, fill = Q13_4, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I trust University extension \nprograms and staff \nto provide unbiased \nclimate-change education") + 
  ylab("Percentage") +
  theme_pubr() +
  coord_flip()

# clean plot
Q13_4plot <- Q13_4plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())
ggsave("figs/Q13_4.pdf",
       height=4, width=12)



### Q13_5
# plot
Q13_5plot <- climatesurvey %>% 
  drop_na(Q13_5) %>%
  count(Q13_5 = factor(Q13_5)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q13_5, y = pct, fill = Q13_5, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(vjust = -1.5,
            hjust = 0, 
            nudge_x = -.5,
            nudge_y = .01,
            size = 4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, .5)) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)))+
  xlab("I will attend\nclimate-change themed\nclasses even if I am\nnot required to for my\nbusiness licenses & operations") + 
  ylab("Percentage") +
  theme_pubr() +
  coord_flip()

# clean plot
Q13_5plot <- Q13_5plot + theme(axis.text.y   = element_blank(),
                               axis.text.x   = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.y  = element_text(size=14, angle=0, vjust=0.5),
                               axis.title.x  = element_text(size=14),
                               #panel.background = element_blank(),
                               #panel.grid.major = element_blank(), 
                               #panel.grid.minor = element_blank(),
                               #axis.line = element_line(colour = "black"),
                               #panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                               legend.position = "none",
                               legend.title=element_blank())

ggsave("figs/Q13_5.pdf",
       height=4, width=12)



# add all plots on single layout

ggarrange(Q13_1plot, Q13_2plot, Q13_3plot, Q13_4plot, Q13_5plot,
          common.legend = TRUE, legend = "top",
          align=c("hv"),
          ncol = 2, nrow = 3)

ggsave("figs/Q13Incentives.pdf",
       height=8, width=14)


## **************************************************************************************
## TABLES, using data with dropped NAs
## **************************************************************************************


## Q1. Please select the roles that best describes you
## this is ugly because allowed participated to "select all that apply."
Q1participantRole <- climatesurveydropNAs %>%
  separate_rows(Q1, sep = ',\\s*') %>%
  filter(!is.na(Q1)) %>%
  group_by(Q1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q2. How long have you been in this industry
Q2Experience <- climatesurveydropNAs %>%
  filter(!is.na(Q2)) %>%
  group_by(Q2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q3. Number and Percent of respondents per county, ranked
Q3percRespondentsPerCounty <- climatesurveydropNAs %>%
  filter(!is.na(Q3)) %>%
  group_by(Q3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q4. Do you hold a management role
Q4mgmtRole <- climatesurveydropNAs %>%
  filter(!is.na(Q4)) %>%
  group_by(Q4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q5. Does your company market at climate friendly
Q5marketClimate <- climatesurveydropNAs %>%
  filter(!is.na(Q5)) %>%
  group_by(Q5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )


## Q6. Where do you receive information about climate change
Q6climateInfo <- climatesurveydropNAs %>%
  separate_rows(Q6, sep = ',\\s*') %>%
  filter(!is.na(Q6)) %>%
  group_by(Q6) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q7. Do you believe human activity is the primary cause of climate change
Q7humansClimateChange <- climatesurveydropNAs %>%
  filter(!is.na(Q7)) %>%
  group_by(Q7) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q8. Assuming that climate change is happening, do you believe it is caused by
Q8whyClimateChange <- climatesurveydropNAs %>%
  filter(!is.na(Q8)) %>%
  group_by(Q8) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q10 Do you believe that climate change is relevant to your business operations, clients, and properties you serve in Florida?
Q10ClimateChangeRelevancy <- climatesurveydropNAs %>%
  filter(!is.na(Q10)) %>%
  group_by(Q10) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q12.Please read the following statements related to climate change and indicate whether you believe the statement is true or false
Q12Fact1 <- climatesurveydropNAs %>%
  filter(!is.na(Q12_1)) %>%
  group_by(Q12_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact2 <- climatesurveydropNAs %>%
  filter(!is.na(Q12_2)) %>%
  group_by(Q12_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact3 <- climatesurveydropNAs %>%
  filter(!is.na(Q12_3)) %>%
  group_by(Q12_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact4 <- climatesurveydropNAs %>%
  filter(!is.na(Q12_4)) %>%
  group_by(Q12_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q12Fact5 <- climatesurveydropNAs %>%
  filter(!is.na(Q12_5)) %>%
  group_by(Q12_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q14. What is the biggest obstacle to implementing climate-friendly practices? Please rank in order of importance, with 1=biggest obstacle and 5=smallest obstacle.

Q14obstacleRankCost <- climatesurveydropNAs %>%
  group_by(Q14_1) %>%
  filter(!is.na(Q14_1)) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankKnwldge <- climatesurveydropNAs %>%
  filter(!is.na(Q14_2)) %>%
  group_by(Q14_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankTools <- climatesurveydropNAs %>%
  filter(!is.na(Q14_3)) %>%
  group_by(Q14_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankInterest <- climatesurveydropNAs %>%
  filter(!is.na(Q14_4)) %>%
  group_by(Q14_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q14obstacleRankPriority <- climatesurveydropNAs %>%
  filter(!is.na(Q14_5)) %>%
  group_by(Q14_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )


## Q15. What is your preferred format for receiving extension education? Please rank in order of importance, with 1=most preferred and 5=leastpreferred.

Q15formatRankWrittenMaterials <- climatesurveydropNAs %>%
  group_by(Q15_1) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankInPersonLec <- climatesurveydropNAs %>%
  group_by(Q15_2) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankVirtualLec <- climatesurveydropNAs %>%
  group_by(Q15_3) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankWorkshops <- climatesurveydropNAs %>%
  group_by(Q15_4) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
Q15formatRankInformal <- climatesurveydropNAs %>%
  group_by(Q15_5) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )
  
## Q17. Highest level education
Q17Education <- climatesurveydropNAs %>%
  filter(!is.na(Q17)) %>%
  group_by(Q17) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q18. Which professional memberships do you currently hold?
Q18Associations <- climatesurveydropNAs %>%
  separate_rows(Q18, sep = ',\\s*') %>%
  filter(!is.na(Q18)) %>%
  group_by(Q18) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q19. Do you have a pesticide license from the Florida Department of Agriculture andConsumer Sciences (FDACS)?
Q19Licensed <- climatesurveydropNAs %>%
  filter(!is.na(Q19)) %>%
  group_by(Q19) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q20. What is your sex
Q20Sex <- climatesurveydropNAs %>%
  filter(!is.na(Q20)) %>%
  group_by(Q20) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q21. Are you Hispanic or Latino?
Q21Hispanic <- climatesurveydropNAs %>%
  filter(!is.na(Q21)) %>%
  group_by(Q21) %>%
  summarise(count = n() ) %>%
  mutate( percent = count / sum(count)*100 )

## Q22. Which category(ies) best describes you?
Q22RaceEthnicity <- climatesurveydropNAs %>%
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
                  'Q15formatRankWrittenMaterials' = Q15formatRankWrittenMaterials,
                  'Q15formatRankInPersonLec' = Q15formatRankInPersonLec,
                  'Q15formatRankVirtualLec' = Q15formatRankVirtualLec,
                  'Q15formatRankWorkshops' = Q15formatRankWorkshops,
                  'Q15formatRankInformal' = Q15formatRankInformal,
                  'Q17Education' = Q17Education,
                  'Q18Associations' = Q18Associations,
                  'Q19Licensed' = Q19Licensed,
                  'Q20Sex' = Q20Sex,
                  'Q21Hispanic' = Q21Hispanic,
                  'Q22RaceEthnicity' = Q22RaceEthnicity)


write.xlsx(tabledataOmitNA, file=file.path(dirTables,
                                     "tablesOmitNAs.xlsx"), rowNames=FALSE)



