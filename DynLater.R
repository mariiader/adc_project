# San Juan


SjApprox <- read.csv("./data/dengue_features_test_Sj_approx.csv", sep=";", stringsAsFactors = F)

OutDyn2 <- read.csv("./SubmissionDynLM2.csv", sep=",", stringsAsFactors = F)



#https://ionides.github.io/531w18/midterm_project/project18/midterm_proj.html





# NegBin Lagged For SJ

dfSjOutLag <- cbind(dfSjOut[10:934,14], #precip
                    #dfSjOut[10:934,14], #total lagged
                    dfSjOut[3:927, c(11,17)], #dew humid
                    dfSjOut[1:925, c(19,22,24)],  # avg min
                    dfSjOut[12:936,c(2,3,24)]) # year week total
names(dfSjOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"
names(dfSjOutLag)[6] <- "total_cases_Lag"

mNBSj <- glm.nb(total_cases ~ 
              reanalysis_dew_point_temp_k + #9
              reanalysis_specific_humidity_g_per_kg + #9
              station_avg_temp_c +  #11
              station_min_temp_c + #,  #11
              total_cases_Lag, 
            data=dfSjOutLag[500:700,])

predNB <- predict(mNBSj, dfSjOutLag[701:925,], type ="response")

mean(abs(predNB[12:225]-dfSjOutLag$total_cases[701:914]))
plotComp(dfSjOutLag$total_cases[701:914], predNB[12:225])

forwardSj <- dfSjOutLag[701:925,1:8]
forwardSj$total_cases_Lag[12:nrow(forwardSj)] <- 0
predForw <- NULL
for (i in 1:nrow(forwardSj)){
  thisPred <- predict(mNBSj, forwardSj[i,], type ="response")
  if (i < nrow(forwardSj)-11){
    forwardSj$total_cases_Lag[11+i] <- thisPred
  }
  predForw <- c(predForw, thisPred)
}

mean(abs(predForw-dfSjOutLag$total_cases[701:925]))
plotComp(predForw, dfSjOut$total_cases[701:925])



##### Now Iquitos #####

dfIqOutLag <- cbind(dfIqOut[11:515,14], #precip  
                    dfIqOut[4:508, c(11,17)], #dew humid 11
                    dfIqOut[1:505, c(19,24)], # avg 14
                    dfIqOut[5:509, c(22)], #min 10
                    dfIqOut[15:519,c(2,3,24)]) # year week total 
names(dfIqOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"
names(dfIqOutLag)[4] <- "station_avg_temp_c"
names(dfIqOutLag)[5] <- "total_cases_Lag"
names(dfIqOutLag)[6] <- "station_min_temp_c"

mNBIq <- glm.nb(total_cases ~ 
                  reanalysis_dew_point_temp_k + 
                  reanalysis_specific_humidity_g_per_kg +
                  station_avg_temp_c +  
                  station_min_temp_c +  
                  total_cases_Lag, 
                data=dfIqOutLag[100:519,])

predNBIq <- predict(mNBIq, dfIqOutLag[451:505,], type ="response")

mean(abs(predNBIq[12:55]-dfIqOutLag$total_cases[451:494]))
plotComp(dfIqOutLag$total_cases[451:494], predNBIq[12:55])

forwardIq <- dfIqOutLag[451:505,1:8]
forwardIq$total_cases_Lag[15:nrow(forwardIq)] <- 0
predForwIq <- NULL
for (i in 1:nrow(forwardIq)){
  thisPred <- predict(mNBIq, forwardIq[i,], type ="response")
  if (i < nrow(forwardIq)-14){
    forwardIq$total_cases_Lag[14+i] <- thisPred
  }
  predForwIq <- c(predForwIq, thisPred)
}

mean(abs(predForwIq-dfIqOutLag$total_cases[451:505]))
plotComp(predForwIq, dfIqOut$total_cases[451:505])
