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
library(ordinal)
library(tidyr)
library(confintr)


setwd("~/Dropbox")
setwd("climate-smart-landscapes-saved")

clusterdata <- read.csv("rawData/climatesurvey_cleaned_withCluster.csv",
                          stringsAsFactors=FALSE)

clusterdata[is.na(clusterdata)] = NA
unique(clusterdata$Cluster)


#all variables for tests need to be read as factors (categorical data)
clusterdata$Cluster<-as.factor(clusterdata$Cluster)
clusterdata$Q4_Supervisor<-as.factor(clusterdata$Q4_Supervisor)
clusterdata$Q20_Sex <-as.factor(clusterdata$Q20_Sex)
clusterdata$Q22_Race <-as.factor(clusterdata$Q22_Race)
clusterdata$Q17_Education<-as.factor(clusterdata$Q17_Education)
clusterdata$Q21_Ethnicity<-as.factor(clusterdata$Q21_Ethnicity)
clusterdata$Q2_Tenure<-as.factor(clusterdata$Q2_Tenure)

clusterdata$Q19_PesticdeLicense<-as.factor(clusterdata$Q19_PesticdeLicense)
clusterdata$Q18_AnyProfAssoc<-as.factor(clusterdata$Q18_AnyProfAssoc)
clusterdata$Q16_General_info_needed<-as.factor(clusterdata$Q16_General_info_needed)

clusterdata$Q13_4_Engagement<-as.factor(clusterdata$Q13_4_Engagement)
clusterdata$Q13_5_Engagement<-as.factor(clusterdata$Q13_5_Engagement)

clusterdata$Q14_BiggestObstacle<-as.factor(clusterdata$Q14_BiggestObstacle)


#run chi sq tests

#SEX
sex<-chisq.test(clusterdata$Cluster, clusterdata$Q20_Sex, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q20_Sex)
sex

#check expected frequencies
#Compare observed and expected cell counts: which cells have more or less observations than would be expected if H0 were true?
sex$expected
#if more than 20% of cells have expected frequencies < 5, 
#we would need to use Fisher's exact test because applying approximation method is inadequate
#with hybrid=TRUE for tables more than 2x2
#sex<-fisher.test(clusterdata$Cluster, clusterdata$Q20_Sex, hybrid=TRUE)
#OR, compute p-values by Monte Carlo simulation with a chi test, which is what i'll do


#but in this case, expected frequencies are >5

cramersv(clusterdata[c("Cluster", "Q20_Sex")])

#RACE
#if you drop the "Other" category of race, not significant, so not including
#clusterdata[clusterdata$Q22_Race=="Other"] <- NA

clusterdata$Q22_Race <-as.factor(clusterdata$Q22_Race)

race<-chisq.test(clusterdata$Cluster, clusterdata$Q22_Race, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q22_Race)
race

race$expected
#in this case, most expected frequencies are < 5
race<-chisq.test(clusterdata$Cluster, clusterdata$Q22_Race, correct=FALSE, simulate.p.value=TRUE)
table(clusterdata$Cluster, clusterdata$Q22_Race)
race

cramersv(clusterdata[c("Cluster", "Q22_Race")])

#ETHNICITY
ethnicity<-chisq.test(clusterdata$Cluster, clusterdata$Q21_Ethnicity, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q21_Ethnicity)
ethnicity

ethnicity$expected
#in this case, all expected frequencies are >5

#PESTICIDE LICENSE
license<-chisq.test(clusterdata$Cluster, clusterdata$Q19_PesticdeLicense, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q19_PesticdeLicense)
license



#HOLD A PROF ASSOCIATION
prof<-chisq.test(clusterdata$Cluster, clusterdata$Q18_AnyProfAssoc, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q18_AnyProfAssoc)
prof

prof$expected

#EDUCATION
edu<-chisq.test(clusterdata$Cluster, clusterdata$Q17_Education, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q17_Education)
edu

edu$expected

#MANAGER

manager<-chisq.test(clusterdata$Cluster, clusterdata$Q4_Supervisor, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q4_Supervisor)
manager 

#TENURE

tenure<-chisq.test(clusterdata$Cluster, clusterdata$Q2_Tenure, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q2_Tenure)
tenure 

#POSITION
LandscapeArch<-chisq.test(clusterdata$Cluster, clusterdata$Q1_LandscapeArchitect, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q1_LandscapeArchitect)
LandscapeArch

#CLIMATE INFO
Extension<-chisq.test(clusterdata$Cluster, clusterdata$Q6_Extension, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q6_Extension)
Extension

Extension$expected
cramersv(clusterdata[c("Cluster", "Q6_Extension")])

#What Info needed

info<-chisq.test(clusterdata$Cluster, clusterdata$Q16_General_info_needed, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q16_General_info_needed)
info

info$expected
cramersv(clusterdata[c("Cluster", "Q16_General_info_needed")])


#I will attend climate change extension even if not required for my business operations and licensing
attend<-chisq.test(clusterdata$Cluster, clusterdata$Q13_5_Engagement, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q13_5_Engagement)
attend

attend$expected
cramersv(clusterdata[c("Cluster", "Q13_5_Engagement")])

#I trust University Extension program and staff to provide unbiased climate change education
trust<-chisq.test(clusterdata$Cluster, clusterdata$Q13_4_Engagement, correct=FALSE)
trust
cramersv(clusterdata[c("Cluster", "Q13_4_Engagement")])

trust$expected
table(clusterdata$Cluster, clusterdata$Q13_4_Engagement)


# Biggest obstacle
obstacle<-chisq.test(clusterdata$Cluster, clusterdata$Q14_BiggestObstacle, correct=FALSE)
obstacle
cramersv(clusterdata[c("Cluster", "Q14_BiggestObstacle")])

obstacle$expected
table(clusterdata$Cluster, clusterdata$Q14_BiggestObstacle)




# stratified contingency tables.
tab <- xtabs(clusterdata$Cluster ~ clusterdata$Q21_Ethnicity + clusterdata$Q22_Race + clusterdata$Q20_Sex, clusterdata)

tab<-table(clusterdata$Cluster, clusterdata$Q21_Ethnicity, clusterdata$Q22_Race, clusterdata$Q20_Sex)
apply(tab, 3, chisq.test)

#Cochran-Mantel-Haenszel chi-squared test for conditional independence.
mantelhaen.test(tab)



#PLOTTING

#remove NAs from all data

#unique(clusterdata$Cluster)
#clusterdataNA.omit <- na.omit(clusterdata)
#unique(clusterdataNA.omit$Cluster)

#can we just remove rows where NAs were in the cluster column or the Q14_BiggestObstacle column
data <- clusterdata %>% 
            select(Q14_BiggestObstacle, Cluster) %>% 
            drop_na() 

#use wchich dataset you need from the two above

clusters <- c("High Involvement, \nHigh Knolwedge", "Low Involvement, \nHigh Knolwedge",  "Low Involvement, \nModerate Knolwedge")

Q14<- ggplot(data) +
  aes(x = Cluster, fill = Q14_BiggestObstacle) +
  geom_bar() +
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling2"), 
                    labels = c("Cost of tools & practices", "Lack of demand", "Lack of tools & practices", "Lack of knowledge", "Not a priority for me")) +
  coord_flip() +
  ylab("Number of Individuals") +
  theme_pubr() + 
  scale_x_discrete(labels= clusters) +
  theme(legend.direction = "vertical",
        legend.position = "right") +
  guides(fill = guide_legend(title = "Biggest obstacles to implementing \n climate-friendly practices")) 
  
  
ggsave("figs/Q14BiggestObstacle.pdf",
       height=4, width=12)



ggplot(heatmap, aes(x = X2, y = X1, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(colours = pal) 



ggplot(clusterdataNA.omit) +
  aes(x = Cluster, fill = Q22_Race,) +
  geom_bar()

ggplot(clusterdataNA.omit) +
  aes(x = Cluster, fill = Q16_General_info_needed,) +
  geom_bar() 

ggplot(clusterdataNA.omit) +
  aes(x = Cluster, fill = Q15_Format,) +
  geom_bar() 






