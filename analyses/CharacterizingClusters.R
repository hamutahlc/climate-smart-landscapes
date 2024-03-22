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



#run chi sq tests

#SEX
sex<-chisq.test(clusterdata$Cluster, clusterdata$Q20_Sex, correct=FALSE)
table(clusterdata$Cluster, clusterdata$Q20_Sex)
sex

#check expected frequencies
sex$expected
#if more than 20% of cells have expected frequencies < 5, 
#we would need to use Fisher's exact test because applying approximation method is inadequate
#with hybrid=TRUE for tables more than 2x2
#sex<-fisher.test(clusterdata$Cluster, clusterdata$Q20_Sex, hybrid=TRUE)
#OR, compute p-values by Monte Carlo simulation with a chi test, which is what i'll do


#but in this case, expected frequencies are >5

cramersv(clusterdata[c("Cluster", "Q20_Sex")])

#RACE
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

ethnicity$expected

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





# stratified contingency tables.
tab <- xtabs(clusterdata$Cluster ~ clusterdata$Q21_Ethnicity + clusterdata$Q22_Race + clusterdata$Q20_Sex, clusterdata)

tab<-table(clusterdata$Cluster, clusterdata$Q21_Ethnicity, clusterdata$Q22_Race, clusterdata$Q20_Sex)
apply(tab, 3, chisq.test)

#Cochran-Mantel-Haenszel chi-squared test for conditional independence.
mantelhaen.test(tab)



#PLOTTING

#remove NAs from the graphs

unique(clusterdata$Cluster)
clusterdataNA.omit <- na.omit(clusterdata)
unique(clusterdataNA.omit$Cluster)


ggplot(clusterdataNA.omit) +
  aes(x = Cluster, fill = Q22_Race,) +
  geom_bar()

ggplot(clusterdataNA.omit) +
  aes(x = Cluster, fill = Q16_General_info_needed,) +
  geom_bar() 



