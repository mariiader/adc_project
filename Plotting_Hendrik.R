# Pred vs. True

plotComp <- function(true, pred){
  thisPlot <- ggplot(data = as.data.frame(cbind(true,pred)), aes(x=seq(1,length(pred),1))) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    geom_line(aes(y=true), color='blue') + geom_line(aes(y=pred), color='red') #+ 
  thisPlot
}

plotSimp(trainSj,"reanalysis_specific_humidity_g_per_kg")
plotSimp(trainIq,"total_cases")

plotSimp <- function(trainSet,variable){
  ticks <- which(trainSet$weekofyear==1)
  ticklabels <- unique(trainSet$year)
  variableV <- trainSet[variable]
  
  thisPlot <- ggplot(data=trainSet, aes(x=ID)) +
     theme(panel.background = element_rect(fill = "white"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) +
      scale_x_continuous(name="Year", expand = c(0, 0), 
                         breaks=ticks, labels=ticklabels[-1]) + 
      scale_y_continuous(name=variable,  expand = c(0, 0)) +
      geom_line(aes(y=variableV), color='red')
  
  thisPlot
}

plotMinMax(meanAir-275.15,minAir-275.15,maxAir-275.15, "Air Temperature")

plotMinMax(meanHum,minHum,maxHum, "Humidity g per kg")

plotMinMax(meanCase,minCase,maxCase, "Cases")

plotMinMax <- function(mean, min, max, ylabel){
  minMaxPlot <- ggplot() + 
    geom_pointrange(mapping=aes(x=seq(1,52,1), y=mean, ymin=min, ymax=max), 
                  size=1, color="blue", fill="white", shape=22) +
    xlab("Week of the Year") + ylab(ylabel)
  minMaxPlot
}



###
