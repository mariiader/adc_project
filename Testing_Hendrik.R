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