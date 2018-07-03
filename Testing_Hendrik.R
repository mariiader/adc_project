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


predict(mDynLag, dfTsjOutLag, type="response")