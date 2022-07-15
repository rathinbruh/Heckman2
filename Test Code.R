
library(tidyverse)
library(fastDummies)
setwd("/Users/rathinbruh/Downloads/Project")
dataR <- read.csv("DataSummary.csv")

#create subdata set 
goodData<-cbind(dataR$R6531900,dataR$R7014300)
#add Data names
colnames(goodData)<-c("ifMoved","ageYoungest")
#get rid of useless data
goodData[,1]<-ifelse(goodData[,1]>-1 ,
                     goodData[,1],NA)
goodData[,2]<-ifelse(goodData[,2]>-1 ,
                     goodData[,2],NA)
goodData<-na.omit(goodData)
# Initial code to get sum estimates of mobility for each age
graphData <- aggregate(ifMoved~ageYoungest,goodData,sum)
lengths <- tapply(goodData[,1], goodData[,2], FUN = length)
graphData[,2] <- graphData[,2]/lengths
#graphing bar plot (format when data is checked)
graphData[,1] <- ifelse(graphData[,1]<17 ,
                        graphData[,1],NA)
graphData<-na.omit(graphData)
view(graphData)
ggplot(graphData, aes(x=ageYoungest, y=ifMoved)) + 
  geom_line( color="#6C5B7B", size=2, alpha=0.9, linetype=1) +
  ggtitle("Mobility Rates")

view(graphData)
