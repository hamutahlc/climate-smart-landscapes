rm(list=ls())
library(factoextra)
library(devtools)
library(ggrepel)
library(FactoMineR)


## load data

setwd("~/Dropbox")
setwd("climate-smart-landscapes-saved")

climatesurvey <- read.csv("cleanedData/climatesurvey_cleaned.csv",
                     stringsAsFactors=FALSE)

## some useful functions for cleaning/exporting data
source("../climate-smart-landscapes/dataPrepAndVis/src/misc.R")

## we'll put products in these tables
dirTables <- "tables"
dirFigs <- "figs"

## ***********************************************************
## prepare data
## ***********************************************************

dim(climatesurvey)

#only include questions of interest, drop some questions
climatesurvey$Finished <- NULL
climatesurvey$ResponseId <- NULL


#create matrix of answers

responses<-climatesurvey
responses[responses =="Strongly disagree"]<- -2
responses[responses =="Somewhat disagree"]<- -1
responses[responses =="Undecided"]<- 0
responses[responses =="Somewhat agree"]<- 1
responses[responses =="Strongly agree"]<- 2


q11<-c("Q11_1", "Q11_2", "Q11_3","Q11_4", "Q11_5")
q1<-c("Q1")
as.numeric(q11$Q11_1) <- Q11_1
q11 <- responses[q11]

pca <- PCA(q11[,c(-1)], graph = FALSE)


longData<-melt(responses)
longData<-longData[!is.na(longData$value),]

#visualize responses to likert scale 
likertresponsesplot<-ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=factor(value))) +
  labs(x="Questions", y="Participant Responses") +
  scale_fill_manual(values=c("#F21A00","#E1AF00", "#EBCC2A", "#78B7C5","#3B9AB2"),
                    name="Answers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0, size=8))

ggsave("figs/likertresponsesplot.pdf",
       height=4, width=12)
       
## ***************************************************
## Hierarchical clustering
## ***************************************************

#create similarity matrix
sim.jac <- matrix(0, nrow=nrow(responses), ncol=nrow(responses))
rownames(sim.jac) <- 1:nrow(responses)
colnames(sim.jac) <- 1:nrow(responses)

pairs <- t(combn(1:nrow(responses), 2))

for (i in 1:nrow(pairs)){
  num <- sum(responses[pairs[i,1],]== responses[pairs[i,2],], na.rm=T)
  den <- length(union(which(!is.na(responses[pairs[i,1],])), which(!is.na(responses[pairs[i,2],]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

#hierarchical clustering
sim2dist <- function(mx) as.dist(sqrt(outer(diag(mx), diag(mx), "+") - 2*mx))
dist <- sim2dist(sim.jac)
hc <- hclust(dist, method = "ward.D2")

# colored dendrogram
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")
A2Rplot(hc, k = 9, boxes = F, col.up = "gray50", col.down = c("#ff9900", 
    "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"), show.labels=F, main=NULL)

#evaluate clusters
cut <- cutree(hc, k=7)
f <- function(x) {k <- which(x!=0); mean(x[k])}
d <- function(x) {91-sum(is.na(x))-sum(x=0)}
x <- data.frame(cut,
                climatesurvey[],
                mean=apply(responses, 1, f),
                nonzero=apply(responses, 1, d))



