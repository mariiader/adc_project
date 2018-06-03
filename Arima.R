#Arima
library('tseries')
library('forecast')
install.packages('qdapTools')
library('qdapTools')
install.packages('fit.models')
library('fit.models')
library('hydroGOF')
hydroGOF

# check stationarty
dft <- adf.test(sj_train$total_cases)
dft
# not stationary


ts_log <- log(sj_train$total_cases)

ts_log <- data.frame('log' = ts_log, 
                                  'time' = as.Date(features$week_start_date[1:936]))
ggplot(ts_log, aes(x = time, y = log )) +
  geom_line() +
  ggtitle('Log of total cases for SJ')

#plot 3 week rolling mean of the log
moving_avg <- rollmean(ts_log$log,3)
ggplot(data.frame('mvg_avg' = moving_avg, 'time' = as.Date(features$week_start_date[1:934])), aes(x = time, y = mvg_avg)) +
  geom_line() +
  ggtitle('')

ts_log_moving_avg_diff <- ts_log - moving_avg


#differencing

ts_log_diff <-  ts_log$log - shift(ts_log$log, 1)
ggplot(data.frame('ts_log_diff' =ts_log_diff , 'time' = as.Date(features$week_start_date[1:936])), aes(x = time, y = ts_log_diff)) +
  geom_line() +
  ggtitle('')

model <- arima(sj_train$total_cases, order=c(6,1,0))
Acf(residuals(model))
residuals <- resid(model)
plot(forecast(model))
#can't forecast 
plot(residuals)
forecast(model)


arima_predictions_sj <- sj_train$total_cases - residuals
mse(arima_predictions_sj,sj_train$total_cases )

df_arima_sj <- data.frame('predictions' = arima_predictions_sj, 
                          'actuals' = sj_train$total_cases, 
                          'time'= as.Date(features$week_start_date[1:936]))



plot <- melt(df_arima_sj, id.vars = "time")
ggplot(plot, aes(x = time, y = value, color = variable)) +
  geom_line() +
  ggtitle('Dengue predicted Cases vs. Actual Cases (City-San Juan) ')


