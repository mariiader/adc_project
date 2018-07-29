### Testing ###

predIq <- round(predict.glm(mPIq, dfTsjOut, type = "response"))
predSj <- round(predict.glm(mPSj, dfTiqOut, type = "response"))

plotComp(dfIqOut$total_cases[235:519], predIq)
plotComp(dfSjOut$total_cases[764:936], predSj)

mean(abs(dfIqOut$total_cases[235:519]-predIq))
mean(abs(dfSjOut$total_cases[764:936]-predSj))


submit$total_cases <- c(predSj, predIq)
write.csv(submit, "./SubmissionPoisson.csv", row.names = FALSE)



subSjForecast <- forecast(ArimaSjEx, xreg = dfTsjOut[1:260, c(17,11,19,22)])
subIqForecast <- forecast(ArimaIqEx, xreg = dfTiqOut[1:156, c(17,11,19,22)])

subSj <- floor(subSjForecast[[4]])
subIq <- floor(subIqForecast[[4]])

submit$total_cases <- c(subSj, subIq)

write.csv(submit, "./SubmissionArima2.csv", row.names = FALSE)

#With Lag


dfTsjOutLag <- cbind(rbind(dfSjOut[928:936, c(11,17)], dfTsjOut[1:251, c(11,17)]), 
                     rbind(dfSjOut[926:936, c(19,22)], dfTsjOut[1:249, c(19,22)]))
dfTiqOutLag <- cbind(rbind(dfIqOut[511:519, c(11,17)], dfTiqOut[1:147, c(11,17)]), 
                     rbind(dfIqOut[509:519, c(19,22)], dfTiqOut[1:145, c(19,22)]))

subSjForecastLag <- forecast(ArimaSjExLag, xreg = dfTsjOutLag[1:260,])
subIqForecastLag <- forecast(ArimaIqExLag, xreg = dfTiqOutLag[1:156,])

subSj <- floor(subSjForecastLag[[4]])
subIq <- floor(subIqForecastLag[[4]])

submit$total_cases <- c(subSj, subIq)

write.csv(submit, "./SubmissionArima3.csv", row.names = FALSE)


#dynlm


dymSjTest <- floor(predict(mDynLag, rbind(dfSjOut[900:936,1:23] , dfTsjOut), type="response"))[38:297]
dymSjTest[which(dymSjTest<0)] <- 0
dymIqTest <- floor(predict(mDynLagIq, rbind(dfIqOut[500:519,1:23] , dfTiqOut), type="response"))[21:176]
dymIqTest[which(dymIqTest<0)] <- 0

submit$total_cases <- c(dymSjTest, dymIqTest)

write.csv(submit, "./SubmissionDynLM2.csv", row.names = FALSE)

plotComp(c(dfSjOut$total_cases, rep(0,260)), 
         c(predDyn, dymSjTest))

plotComp(c(dfIqOut$total_cases, rep(0,156)), 
         c(predDynIq, dymIqTest))

plot(c(dfIqOut$reanalysis_dew_point_temp_k, dfTiqOut$reanalysis_dew_point_temp_k))
plot(c(dfIqOut$reanalysis_specific_humidity_g_per_kg, dfTiqOut$reanalysis_specific_humidity_g_per_kg))
plot(c(dfIqOut$station_avg_temp_c, dfTiqOut$station_avg_temp_c))
plot(c(dfIqOut$station_min_temp_c, dfTiqOut$station_min_temp_c))



# Forward negBin

dfTsjOutLag <- cbind(c(dfSjOut[935:936, 14], dfTsjOut[1:258,14]),
                cbind(rbind(dfSjOut[928:936, c(11,17)], dfTsjOut[1:251, c(11,17)]), 
                     rbind(dfSjOut[926:936, c(19,22)], dfTsjOut[1:249, c(19,22)])))
names(dfTsjOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"


dfTiqOutLag <- cbind(c(dfIqOut[516:519, 14], dfTiqOut[1:152,14]),
                     rbind(dfIqOut[511:519, c(11,17)], dfTiqOut[1:147, c(11,17)]), 
                     rbind(dfIqOut[509:519, c(19,22)], dfTiqOut[1:145, c(19,22)]))
names(dfTiqOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"

mNBSj <- glm.nb(total_cases ~ 
                  reanalysis_dew_point_temp_k + #9
                  reanalysis_specific_humidity_g_per_kg + #9
                  station_avg_temp_c +  #11
                  station_min_temp_c + #,  #11
                  total_cases_Lag, 
                data=dfSjOutLag[500:936,]) #500

forwardSj <- dfTsjOutLag
forwardSj$total_cases_Lag <- 0
forwardSj$total_cases_Lag[1:11] <- dfSjOut$total_cases[926:936]
predForw <- NULL
for (i in 1:nrow(forwardSj)){
  thisPred <- predict(mNBSj, forwardSj[i,], type ="response")
  if (i < nrow(forwardSj)-11){
    forwardSj$total_cases_Lag[11+i] <- thisPred
  }
  predForw <- c(predForw, thisPred)
}

mean(abs(predForw-SjApprox$real), na.rm = T)
plotComp(floor(predForw), SjApprox$real)

subSj <- floor(predForw)

submit$total_cases <- c(subSj, rep(0,156))


write.csv(submit, "./SubmissionForwardSjDynIq.csv", row.names = FALSE)

# Now for Iquitos

forwardIq <- dfTiqOutLag
forwardIq$total_cases_Lag[15:nrow(forwardIq)] <- 0
forwardIq$total_cases_Lag[1:14] <- dfIqOut$total_cases[506:519]
predForwIq <- NULL
for (i in 1:nrow(forwardIq)){
  thisPred <- predict(mNBIq, forwardIq[i,], type ="response")
  if (i < nrow(forwardIq)-14){
    forwardIq$total_cases_Lag[14+i] <- thisPred
  }
  predForwIq <- c(predForwIq, thisPred)
}

submit$total_cases <- c(rep(0,260), floor(predForwIq))

write.csv(submit, "./SubmissionForwardSjIq.csv", row.names = FALSE)



##########################


