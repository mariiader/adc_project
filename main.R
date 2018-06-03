
library(rio)
library('MASS')

# Read data
setwd("C:/Users/Masha/Desktop/SS-18/project")

test <- import("dengue_features_test.csv")
features <- import("dengue_features_train.csv")
labels<- import("dengue_labels_train.csv")



# add total cases
features$total_cases <- labels$total_cases
train <- features
sum(is.na(train))

# without city, year, weekofyear, week_start_date
train_cor = train[, 5:25]

#impute missings
nas <- function(x){
  for(i in 1:ncol(x)){
    x[is.na(x[, i]), i] <- mean(x[, i], na.rm = TRUE)
  }
  return(x)
}

train_cor <- nas(train_cor)

#merge
train <- cbind(train[, 1:4], train_cor)


# separtae into cities
sj_train<- subset(train, city == 'sj')
iq_train <- subset(train, city == 'iq')



#Negative Binomial Regression
# training
nb_sj <- glm.nb(total_cases ~ reanalysis_min_air_temp_k  + 
                 station_max_temp_c +
                 reanalysis_dew_point_temp_k +
                 station_avg_temp_c +
                 reanalysis_specific_humidity_g_per_kg,
               data = sj_train)
summary(nb_sj)

prediction_sj <-  predict(nb_sj, sj_train, type = 'response')

mean((sj_train$total_cases-prediction)^2)

prediction_train_sj <- data.frame('prediction' = prediction_sj, 'actual' = sj_train$total_cases,
                                     'time' = as.Date(features$week_start_date[1:936]))
prediction_train_sj <- melt(prediction_train_sj, id.vars = 'time')
ggplot(prediction_train_sj, aes(x = time, y = value, color = variable)) +
  geom_line() +
  ggtitle('Dengue predicted Cases vs. Actual Cases (City-San Juan) ')






