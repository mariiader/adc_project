# San Juan


SjApprox <- read.csv("./data/dengue_features_test_Sj_approx.csv", sep=";", stringsAsFactors = F)




#https://ionides.github.io/531w18/midterm_project/project18/midterm_proj.html


sarma45 <- arima(log(dfSjOut$total_cases + 1),order=c(4,0,5), seasonal=list(order=c(1,0,1),period=52), 
                 xreg = dfSjOut[c('station_avg_temp_c', 'reanalysis_sat_precip_amt_mm')])
sarma45