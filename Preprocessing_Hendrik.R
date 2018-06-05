############### Data Challenge ################### 

##### Preprocessing #####


library(foreign)
library(dplyr)
library(MASS)
library(ggplot2)
library(zoo)
library(forecast)
library(corrgram)

#Import as Time Series

feats <- read.csv(
  "./data/dengue_features_train.csv",
  sep=",", stringsAsFactors = F)

labels <- read.csv(
  "./data/dengue_labels_train.csv",
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




### Zoo ###
zooSj <- read.zoo(trainSj[,1:25],  index.column = 4, format="%Y-%m-%d")
zooIq <- read.zoo(trainIq[,1:25],  index.column = 4, format="%Y-%m-%d")


### Missing Values ###
# Probably different imputations for different values #

#sapply(zooSj,function(x){sum(is.na(x))})
zooSjOut <- na.approx(zooSj[,4:23])
zooSjOut <- merge.zoo(zooSj[,1:3], zooSjOut, zooSj[,24])
names(zooSjOut)[24] <- "total_cases"
dfSjOut <- as.data.frame(zooSjOut)


#sapply(zooIq,function(x){sum(is.na(x))})
zooIqOut <- na.approx(zooIq[,4:23])
zooIqOut <- merge.zoo(zooIq[,1:3], zooIqOut, zooIq[,24])
names(zooIqOut)[24] <- "total_cases"


test <- na.StructTS(zooIq$reanalysis_avg_temp_k)

# as ts??? with season
tsSj <- as.ts(zooSjOut, frequency = c(7,52))


plot.zoo(zooSj$ndvi_ne, ylim = c(0,1))
plot.zoo(zooSjOut$ndvi_se, ylim = c(0,1)) #
plot.zoo(zooSjOut$reanalysis_avg_temp_k, ylim = c(295,305))


########### Imputing Weather NAs ##############




#########################

plotComp(trainIqImp, trainIqImp$pred)