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

corrgram(trainSj, upper.panel=panel.cor, main="San Juan")[,23]
corrgram(trainIq, upper.panel=panel.cor, main="Iquitos")


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
summary(mPIq <- glm(total_cases ~ year + weekofyear + 
                       ndvi_ne + ndvi_nw + 
                       ndvi_se + ndvi_sw + precipitation_amt_mm + 
                       reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
                       reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
                       reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
                       reanalysis_specific_humidity_g_per_kg +
                       reanalysis_tdtr_k + station_avg_temp_c + station_diur_temp_rng_c + 
                       station_max_temp_c + station_min_temp_c + station_precip_mm , 
                  data=trainIqImp,
                  family = "poisson"))

summary(mPSj <- glm(total_cases ~ year + weekofyear + 
                    ndvi_ne + ndvi_nw + 
                    ndvi_se + ndvi_sw + precipitation_amt_mm + 
                    reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
                    reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
                    reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
                    reanalysis_specific_humidity_g_per_kg +
                    reanalysis_tdtr_k + station_avg_temp_c + station_diur_temp_rng_c + 
                    station_max_temp_c + station_min_temp_c + station_precip_mm , 
                  data=trainSjImp,
                  family = "poisson"))


trainIq$pred <- predict.glm(mNB, trainIq[,1:24], type = "response")
trainSjImp$pred <- predict.glm(mPSj, trainSjImp[,1:24], type = "response")

mean((trainIq$total_cases-trainIq$pred)^2 , na.rm=T)

plotComp(trainIq, trainIq$pred)
        
### Testing ###

featTest <- read.csv(
  "C:/Users/hendr/Documents/Uni_Konstanz/Master/Semester_4/Challenge/data/dengue_features_test.csv",
  sep=",", stringsAsFactors = F)

