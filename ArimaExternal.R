################## Arima with external regressors  ################## 

library('tseries')
library('forecast')
library('qdapTools')
library('fit.models')
library('hydroGOF')

ts_log <- log(trainSj$total_cases)
ts_log[is.infinite(ts_log)] <- 0

ts_log <- data.frame('log' = ts_log, 
                     'time' = as.Date(trainSj$week_start_date[1:936]))

moving_avg <- rollmean(trainSj$total_cases,3)

ts_moving_avg_diff <- trainSj$total_cases[3:936] - moving_avg

ts_diff <-  trainSj$total_cases - shift(trainSj$total_cases, 1)

#model <- arima(trainSj$total_cases, order=c(6,1,0))
#Acf(residuals(model))
#residuals <- resid(model)
#plot(forecast(model))
#plot(residuals)


#arima_predictions_sj <- trainSj$total_cases - residuals
#mse(arima_predictions_sj,trainSj$total_cases )

#df_arima_sj <- data.frame('predictions' = arima_predictions_sj, 
#                          'actuals' = sj_train$total_cases, 
#                          'time'= as.Date(features$week_start_date[1:936]))

tsSj_df <- data.frame('Cases' = trainSj$total_cases, 
                     'Time' = as.Date(trainSj$week_start_date[1:936]))
tsIq_df <- data.frame('Cases' = trainIq$total_cases, 
                      'Time' = as.Date(trainIq$week_start_date[1:519]))
tsSj <- ts(tsSj_df, start=c(1990,18), frequency = 52)
tsIq <- ts(tsIq_df, start=c(2000,26), frequency = 52)



ArimaSjEx <- auto.arima(tsSj[1:936], 
                        stepwise=FALSE, 
                        approximation = FALSE, 
                        seasonal  = TRUE,
                        xreg = dfSjOut[1:936,c(17,11,19,22)]) #650

ArimaIqEx <- auto.arima(tsIq[1:519], 
                        stepwise=FALSE, 
                        approximation = FALSE, 
                        seasonal  = TRUE,
                        xreg = dfIqOut[1:519,c(17,11,19,22)]) #350


#1: SjForecast <- forecast(ArimaSjEx, xreg = dfSjOut[651:936,c(2,3,4,5,6,7,8,9,14,15,18,20,23)])
#1: IqForecast <- forecast(ArimaIqEx, xreg = dfIqOut[351:519,c(2,3,4,5,6,7,8,9,14,15,18,20,23)])

Sj <- SjForecast[[4]]
Iq <- IqForecast[[4]]

plot(c(trainSj$total_cases[1:650],Sj))
plot(c(trainIq$total_cases[1:350],Iq))
plot(trainIq$total_cases)

mean(abs(trainSj$total_cases[651:936]-floor(Sj)))
mean(abs(trainIq$total_cases[351:519]-floor(Iq)))




#with lag
dfSjOutLag <- cbind(dfSjOut[3:927, c(11,17)], dfSjOut[1:925, c(19,22)])
dfIqOutLag <- cbind(dfIqOut[3:510, c(11,17)], dfIqOut[1:508, c(19,22)])


ArimaSjExLag <- auto.arima(tsSj[12:936], 
                        stepwise=FALSE, 
                        approximation = FALSE, 
                        seasonal  = TRUE,
                        xreg = dfSjOutLag)

ArimaIqExLag <- auto.arima(tsIq[12:519], 
                        stepwise=FALSE, 
                        approximation = FALSE, 
                        seasonal  = TRUE,
                        xreg = dfIqOutLag)
