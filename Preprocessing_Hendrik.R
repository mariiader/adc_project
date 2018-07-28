############### Data Challenge ################### 

##### Preprocessing #####


library(foreign)
library(dplyr)
library(MASS)
library(ggplot2)
library(zoo)
library(forecast)
library(corrgram)
library(dynlm)
library(astsa)

#Import as Time Series

setwd("C:/Users/hendr/Documents/Uni_Konstanz/Master/Semester_4/Challenge/gitRepo/adc_project")

feats <- read.csv(
  "./data/dengue_features_train.csv",
  sep=",", stringsAsFactors = F)

labels <- read.csv(
  "./data/dengue_labels_train.csv",
  sep=",", stringsAsFactors = F) 

test <- read.csv(
  "./data/dengue_features_test.csv",
  sep=",", stringsAsFactors = F)

submit <- read.csv(
  "./data/submission_format.csv",
  sep=",", stringsAsFactors = F)

#Concat

train <- merge(feats, labels, by=c("city","year","weekofyear"), all=TRUE)
rm(feats, labels)

#San Juan
trainSj <- filter(train, city=="sj")
trainSj <- trainSj[with(trainSj, order(year,weekofyear)),]
trainSj$ID <- seq(1, nrow(trainSj), 1)
rownames(trainSj) <- seq(length=nrow(trainSj)) 


#Iquitos
trainIq <- filter(train, city=="iq")
trainIq <- trainIq[with(trainIq, order(year,weekofyear)),]
trainIq <- trainIq[1:519,]
trainIq$ID <- seq(1, nrow(trainIq), 1)
rownames(trainIq) <- seq(length=nrow(trainIq))

#Test
testSj <- filter(test, city=="sj")
testIq <- filter(test, city=="iq")
testSj <- testSj[with(testSj, order(year,weekofyear)),]
testIq <- testIq[with(testIq, order(year,weekofyear)),]
testSj$ID <- seq(1, nrow(testSj), 1)
testIq$ID <- seq(1, nrow(testIq), 1)


### Zoo ###
zooSj <- read.zoo(trainSj[,1:25],  index.column = 4, format="%Y-%m-%d")
zooIq <- read.zoo(trainIq[,1:25],  index.column = 4, format="%Y-%m-%d")
zooTsj <- read.zoo(testSj[,1:24],  index.column = 4, format="%Y-%m-%d")
zooTiq <- read.zoo(testIq[,1:24],  index.column = 4, format="%Y-%m-%d")


### Missing Values ###
# Probably different imputations for different values #

#sapply(zooSj,function(x){sum(is.na(x))})
zooSjOut <- na.approx(zooSj[,4:23])
zooSjOut <- merge.zoo(zooSj[,1:3], zooSjOut, zooSj[,24])
names(zooSjOut)[24] <- "total_cases"
dfSjOut <- as.data.frame(zooSjOut)
indx <- sapply(dfSjOut, is.factor)
dfSjOut[indx] <- lapply(dfSjOut[indx], function(x) as.numeric(as.character(x)))
dfSjOut$city <- "sj"


#sapply(zooIq,function(x){sum(is.na(x))})
zooIqOut <- na.approx(zooIq[,4:23])
zooIqOut <- merge.zoo(zooIq[,1:3], zooIqOut, zooIq[,24])
names(zooIqOut)[24] <- "total_cases"
dfIqOut <- as.data.frame(zooIqOut)
indx <- sapply(dfIqOut, is.factor)
dfIqOut[indx] <- lapply(dfIqOut[indx], function(x) as.numeric(as.character(x)))
dfIqOut$city <- "iq"


# Zoo for Test
zooTsjOut <- na.approx(zooTsj[,4:23])
zooTiqOut <- na.approx(zooTiq[,4:23])

zooTsjOut <- merge.zoo(zooTsj[,1:3], zooTsjOut)
zooTiqOut <- merge.zoo(zooTiq[,1:3], zooTiqOut)

dfTsjOut <- as.data.frame(zooTsjOut)
dfTiqOut <- as.data.frame(zooTiqOut)

indx <- sapply(dfTsjOut, is.factor)
dfTsjOut[indx] <- lapply(dfTsjOut[indx], function(x) as.numeric(as.character(x)))
dfTsjOut$city <- "sj"
dfTiqOut[indx] <- lapply(dfTiqOut[indx], function(x) as.numeric(as.character(x)))
dfTiqOut$city <- "iq"

rm(indx, zooIq, zooIqOut, zooSj, zooSjOut, zooTiq, zooTiqOut, zooTsj, zooTsjOut)

