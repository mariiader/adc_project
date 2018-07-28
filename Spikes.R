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