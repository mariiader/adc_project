### Functions ###


# Statistics by week for DF, "Variable_Name", function (like mean)

statV <- function(trainSet, varname, func){
  values <- NULL
  weeks <- length(trainSet$ID)/52
  for (i in 1:52){
    varvec <- filter(trainSet,weekofyear==i)[varname]
    statVar <- sapply(varvec, func, na.rm=TRUE)
    values <- c(values,statVar)
  }
  return(values)
}


# A rough testing function for the lag between cases and data


lagF <- function(trainSet, variables, k){
  output <- NULL
  rowcount <- nrow(trainSet)
  
  for (i in 0:k){
    df <- cbind(trainSet[1:(rowcount-i), variables], trainSet[(1+i):rowcount, "total_cases"])
    df <- df[,1:length(df)-1]
    result <- sum(cor(df, use = "pairwise.complete.obs")[,length(df)]^2)-1
    output <- c(output, result)
  }
  
  return(output)
}
