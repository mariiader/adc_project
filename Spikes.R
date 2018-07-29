### Spikes ###

spikes <- which(dfSjOut$total_cases>90)
spikes <- c(spikes,seq(65,73,1), seq(86,88,1), seq(90,98,1), 
            seq(208,225,1), seq(246,260,1),
            seq(775,796,1),seq(801,815,1),
            seq(882,901,1), seq(909,936,1))

ggplot(dfSjOut, aes(color=(ID %in% spikes))) +
  geom_line(aes(group=1, x=seq(1,936,1), y=total_cases))

dfSjOut$ID <- seq(1,936,1)


pdfhandle <- "./DataSj.pdf"
pdf(file=pdfhandle)
for (i in seq(4,24,3)){
  par(mfrow=c(3,1))
    print(ggplot(dfSjOut, aes(color=(ID %in% spikes))) +
            geom_line(aes(group=1, x=seq(1,936,1), y=dfSjOut[,i])) +
            labs(y=names(dfSjOut)[i]))
    print(ggplot(dfSjOut, aes(color=(ID %in% spikes))) +
            geom_line(aes(group=1, x=seq(1,936,1), y=dfSjOut[,i+1])) +
            labs(y=names(dfSjOut)[i+1]))
    print(ggplot(dfSjOut, aes(color=(ID %in% spikes))) +
            geom_line(aes(group=1, x=seq(1,936,1), y=dfSjOut[,i+2])) +
            labs(y=names(dfSjOut)[i+2]))
}
dev.off()


### Corr with Spike occurence ###

dfSjOut$Spike <- 0
dfSjOut$Spike[dfSjOut$total_cases > 90] <- 1

dfSjOut1 <- cbind(dfSjOut[1:935,1:24], dfSjOut[2:936,25] )
dfSjOut2 <- cbind(dfSjOut[1:934,1:24], dfSjOut[3:936,25] )
dfSjOut3 <- cbind(dfSjOut[1:933,1:24], dfSjOut[4:936,25] )
dfSjOut4 <- cbind(dfSjOut[1:932,1:24], dfSjOut[5:936,25] )
dfSjOut5 <- cbind(dfSjOut[1:931,1:24], dfSjOut[6:936,25] )
dfSjOut6 <- cbind(dfSjOut[1:930,1:24], dfSjOut[7:936,25] )
dfSjOut7 <- cbind(dfSjOut[1:929,1:24], dfSjOut[8:936,25] )
dfSjOut8 <- cbind(dfSjOut[1:928,1:24], dfSjOut[9:936,25] )
dfSjOut9 <- cbind(dfSjOut[1:927,1:24], dfSjOut[10:936,25] )
dfSjOut10 <- cbind(dfSjOut[1:926,1:24], dfSjOut[11:936,25] )
dfSjOut11 <- cbind(dfSjOut[1:925,1:24], dfSjOut[12:936,25] )

corrgram(dfSjOut10, upper.panel=panel.cor, main="San Juan")



#####

dfSjOutLag <- cbind(dfSjOut[10:934,14], #precip
                    #dfSjOut[10:934,14], #total lagged
                    dfSjOut[3:927, c(11,17)], #dew humid
                    dfSjOut[1:925, c(19,22,24)],  # avg min
                    dfSjOut[12:936,c(2,3,24,25)]) # year week total
names(dfSjOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"
names(dfSjOutLag)[6] <- "total_cases_Lag"

spikenb <- glm.nb(Spike ~ 
                  reanalysis_dew_point_temp_k + #9
                  reanalysis_specific_humidity_g_per_kg + #9
                  station_avg_temp_c +  #11
                  station_min_temp_c,  #11
                data=dfSjOutLag)


###### IQ ######

spikes <- which(dfIqOut$total_cases>25)
dfIqOut$spikes <- 0
dfIqOut$spikes[spikes] <- 1

dfIqOutLag <- cbind(dfIqOut[11:515,14], #precip
                    dfIqOut[4:508, c(11,17)], #dew humid
                    dfIqOut[1:505, c(19,24)], # avg
                    dfIqOut[5:509, c(22)], #min
                    dfIqOut[15:519,c(2,3,24,25)]) # year week total
names(dfIqOutLag)[1] <- "reanalysis_precip_amt_kg_per_m2"
names(dfIqOutLag)[4] <- "station_avg_temp_c"
names(dfIqOutLag)[5] <- "total_cases_Lag"
names(dfIqOutLag)[6] <- "station_min_temp_c"

high_temp <- which(dfIqOut$station_min_temp_c > 23)
high_prec <- which(dfIqOut$reanalysis_precip_amt_kg_per_m2 > 120)
high_dew <- which(dfIqOut$reanalysis_dew_point_temp_k > 297)
