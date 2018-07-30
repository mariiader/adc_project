library(rpart)
library(randomForest)


fitsj <- rpart(total_cases ~ reanalysis_precip_amt_kg_per_m2 + reanalysis_dew_point_temp_k +  
    reanalysis_specific_humidity_g_per_kg + station_avg_temp_c   +   
    station_min_temp_c, 
    method="poisson", data=dfSjOutLag[650:925,], control=rpart.control(minsplit=50, cp=0.001))
#Test: 650

treepredSj <- predict(fitsj, newdata=dfSjOutLag[701:925,])

mean(abs(dfSjOutLag$total_cases[701:925] - treepredSj))
plotComp(c(dfSjOutLag$total_cases[701:925]), 
         treepredSj)

printcp(fitsj) # display the results 
plotcp(fitsj) # visualize cross-validation results 
summary(fitsj) # detailed summary of splits

plot(fitsj, uniform=T, margin=0.2)
text(fitsj, use.n=TRUE, all=TRUE, cex=0.8)

###Test 

test <- predict(fitsj, dfTsjOutLag)
mean(abs(floor(test)-SjApprox$real), na.rm = T)
plotComp(floor(test), SjApprox$real)


### Iq

fitiq <- rpart(total_cases ~ reanalysis_precip_amt_kg_per_m2 + reanalysis_dew_point_temp_k +  
               reanalysis_specific_humidity_g_per_kg + station_avg_temp_c   +   
               station_min_temp_c, 
             method="poisson", data=dfIqOutLag[300:505,], control=rpart.control(minsplit=20, cp=0.001))

treepredIq <- predict(fitiq, newdata=dfIqOutLag[401:505,])

mean(abs(dfIqOutLag$total_cases[401:505] - treepredIq))
plotComp(c(dfIqOutLag$total_cases[401:505]), 
         treepredIq)

testiq <- predict(fitiq, newdata=dfTiqOutLag)

# Out #
submit$total_cases <- c(floor(test), floor(testiq))

write.csv(submit, "./SubmissionTree.csv", row.names = FALSE)






### OTHER ###

fit <- rpart(total_cases ~ station_avg_temp_c, 
             method="poisson", data=dfSjOutLag[500:700,], control=rpart.control(minsplit=50, minbucket=5))

fit <- randomForest(total_cases ~ ., 
                    method="poisson", data=dfSjOutLag[500:700,], control=rpart.control(minsplit=35, minbucket=1))

treepredSj <- predict(fit, newdata=dfSjOutLag[701:925,])
treepredSjSpikes <- predict(fitspike, newdata=dfSjOutLag[701:925,])*100

mean(abs(dfSjOutLag$total_cases[701:925] - treepredSj))
plotComp(c(dfSjOutLag$total_cases[701:925]), 
         treepredSj)

plot(fit, uniform=T)
text(fit, use.n=TRUE, all=TRUE, cex=0.8)
