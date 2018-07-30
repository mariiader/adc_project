############### Data Challenge ################### 

##### Descriptive Statistics / Explorative Analysis #####

# Lag:http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# http://www.denguevirusnet.com/aedes-aegypti.html
# egg: 2-7 days, larvae: >4 days, pupae: 2 days
# adult: 2-4 weeks
# Can take months if water or warmth are lacking
# Incubation 3-14 days: Period of illness 3-7 days
# Not Contagious
# Correlogramms -> Lags if differ


# Mean with Standard Deviation for all variables
sapply(trainSj, function(x){print(c(mean(x, na.rm=TRUE), sqrt(var(x, na.rm = TRUE))))})


# Mean, Max and Min for the Presentation Plots
meanAir <- statV(trainIq, "reanalysis_air_temp_k", mean)
maxAir <- statV(trainIq, "reanalysis_air_temp_k", max)
minAir <- statV(trainIq, "reanalysis_air_temp_k", min)

meanHum <- statV(trainIq, "reanalysis_specific_humidity_g_per_kg", mean)
maxHum <- statV(trainIq, "reanalysis_specific_humidity_g_per_kg", max)
minHum <- statV(trainIq, "reanalysis_specific_humidity_g_per_kg", min)

meanCase <- statV(trainIq, "total_cases", mean)
maxCase <- statV(trainIq, "total_cases", max)
minCase <- statV(trainIq, "total_cases", min)


# Corrgram

corrgram(dfSjOut, upper.panel=panel.cor, main="San Juan")
corrgram(dfIqOut, upper.panel=panel.cor, main="Iquitos")

lagtest <- corMatrix(dfSjOut)
lagtest <- corMatrix(dfIqOut)

corMatrix <- function(trainSet){
  corvec <- c(cor(trainSet[,2:24])[23,], 0)
  n <- nrow(trainSet)
  for (i in 1:52){
    thisDF <- cbind(dfSjOut[1:(n-i),2:23], trainSet$total_cases[(i+1):n])
    corveci <- c(cor(thisDF)[23,], i)
    corvec <- rbind(corvec, corveci)
  }
  return(corvec)
}

maxvec <- NULL
for (i in 1:23){
  maxvec <- c(maxvec,which(lagtest[,i]==max(lagtest[,i])))
}

pdfhandle <- "./lagSjSpike.pdf"
pdf(file=pdfhandle)
for (i in seq(1,24,3)){
  par(mfrow=c(3,1))
  plot(lagtest[,i]^2, ylab=colnames(lagtest[0,])[i])
  plot(lagtest[,i+1]^2, ylab=colnames(lagtest[0,])[i+1])
  plot(lagtest[,i+2]^2, ylab=colnames(lagtest[0,])[i+2])
}
dev.off()


#Some other summary statistics
summary(train$total_cases)
with(trainSj, plot(total_cases ~ weekofyear))
aggregate(train$total_cases, by=list(train$year), FUN = mean)
with(aggregate(train$total_cases, by=list(train$year), FUN = mean), plot(x ~ Group.1))
with(train, plot(total_cases ~ reanalysis_specific_humidity_g_per_kg))
summary(lm(train$total_cases ~ train$reanalysis_relative_humidity_percent))


# Playing around with lag (-> Functions_Hendrik):

Sj <- lagF(trainSj, c(5:25), 100)
Iq <- lagF(trainIq, c(5:25), 100) 
plot(Sj)
plot(Iq)



#Very Simple modelling:

#Negative binomial with all the variables
summary(mNB <- glm.nb(total_cases ~ year + weekofyear + 
                      ndvi_ne + ndvi_nw + 
                      ndvi_se + ndvi_sw + precipitation_amt_mm + 
                      reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
                      reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
                      reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
                      reanalysis_specific_humidity_g_per_kg +
                      reanalysis_tdtr_k + station_avg_temp_c + station_diur_temp_rng_c + 
                      station_max_temp_c + station_min_temp_c + station_precip_mm , data=trainIq))

#Poisson
mPIq <- glm(total_cases ~ weekofyear + 
                       ndvi_ne  + ndvi_se + ndvi_sw + ndvi_nw +
                       precipitation_amt_mm + 
                       reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
                       reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
                       reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
                       reanalysis_specific_humidity_g_per_kg +
                       reanalysis_tdtr_k + station_avg_temp_c + station_diur_temp_rng_c + 
                       station_max_temp_c + station_min_temp_c + station_precip_mm , 
                  data=dfIqOut,
                  family = "poisson")

mPSj <- glm(total_cases ~ weekofyear + 
                    ndvi_ne  + ndvi_se + ndvi_sw + ndvi_nw +
                    precipitation_amt_mm + 
                    reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
                    reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
                    reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
                    reanalysis_specific_humidity_g_per_kg +
                    reanalysis_tdtr_k + station_avg_temp_c + station_diur_temp_rng_c + 
                    station_max_temp_c + station_min_temp_c + station_precip_mm , 
                  data=dfSjOut,
                  family = "poisson")


trainIq$pred <- predict.glm(mNB, trainIq[,1:24], type = "response")
trainSjImp$pred <- predict.glm(mPSj, trainSjImp[,1:24], type = "response")

mean((trainIq$total_cases-trainIq$pred)^2 , na.rm=T)

plotComp(trainIq, trainIq$pred)
        


# With lag

logtcbasic <- log(dfSjOut$total_cases+0.001) - log(0.001)
dfSjOut$logtc <- logtcbasic

summary(mNBL <- glm.nb(total_cases[12:936] ~ reanalysis_dew_point_temp_k[3:927] + 
                        reanalysis_specific_humidity_g_per_kg[3:927] +
                        station_avg_temp_c[1:925] + 
                        station_min_temp_c[1:925] + 
                        total_cases[11:935], data=dfSjOut))

predNBSjLag <- predict(mNBL, newdata = dfSjOutLag[762:925,], type = "response")



# DynLm

dfSjOutShort <- dfSjOut[500:700,]
dfIqOutShort <- dfSjOut[1:300,]

summary(mDynLag <- dynlm(total_cases ~ L(reanalysis_dew_point_temp_k, 9) + 
                         L(reanalysis_specific_humidity_g_per_kg, 9) +
                         L(station_avg_temp_c, 11) + 
                         L(station_min_temp_c, 11) +
                         #year #+
                          L(reanalysis_precip_amt_kg_per_m2, 2) #+
                         , data=dfSjOutShort))


summary(mDynLagIq <- dynlm(total_cases ~ L(reanalysis_dew_point_temp_k, 11) + 
                           L(reanalysis_specific_humidity_g_per_kg, 11) +
                           L(station_avg_temp_c, 14) + 
                           L(station_min_temp_c, 10) +
                         #year #+
                         L(reanalysis_precip_amt_kg_per_m2, 2) #+
                         # L(total_cases, 1)
                         , data=dfIqOut)) # Short

dfSjOutLag <- cbind(dfSjOut[10:934,14], #precip
                    dfSjOut[3:927, c(11,17)], #dew humid
                    dfSjOut[1:925, c(19,22)],  # avg min
                    dfSjOut[12:936,c(2,3,24)]) # year week total
names(dfSjOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"

dfIqOutLag <- cbind(dfIqOut[11:515,14], #precip
                    dfIqOut[4:508, c(11,17)], #dew humid
                    dfIqOut[1:505, c(19)], # avg
                    dfIqOut[5:509, c(19)], #min
                    dfIqOut[15:519,c(2,3,24)]) # year week total
names(dfIqOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"
names(dfIqOutLag)[4] <- "station_avg_temp_c"
names(dfIqOutLag)[5] <- "station_min_temp_c"

dfSjOutLagShort <- dfSjOutLag[749:761,1:8]
dfSjOutShort$total_cases[13] <- NA

for (i in 762:925) {
  tc <- predict(mDynLag, newdata = dfSjOutLagShort)[13]
  df <- data.frame(c(dfSjOutLag[i,1:7] , tc))
  names(df) <- names(dfSjOutLagShort)
  dfSjOutLagShort <- rbind(dfSjOutLagShort, df)
}

#LSTM

predDynSj <- predict(mDynLag, newdata = dfSjOut[701:936,], type = "response")
predDynIq <- predict(mDynLagIq, newdata = dfIqOutLag[301:519,1:8], type = "response")
predDynIq <- predict(mDynLagIq, newdata = dfIqOut, type = "response")

plotComp(dfSjOut[12:936,24], 
         log(predNBSjLag[1:925]))

plotComp(c(dfSjOut[701:936,24]), 
         predDynSj)

plotComp(c(dfIqOut$total_cases), 
         predDynIq)

plotComp(c(dfSjOut[773:936,24]), 
         dfSjOutLagShort$total_cases[14:177])

mean(abs(dfSjOut[701:936,24]-(predDynSj)))
mean(abs(dfSjOut[772:936,24]-(predNBSjLag[761:925])))
mean(abs(dfSjOut[773:936,24]-dfSjOutLagShort$total_cases[14:177]))
mean(abs(dfSjOut[773:936,24]-rep(15,164)))
mean(abs(dfIqOut$total_cases-predDynIq))