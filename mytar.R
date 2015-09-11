# p .. AR order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes

# see 1998 Martens et al, Appendix Step 2
myNonLinearityTest <- function(series, p=1, d=1, k=3) {
  
  #h <- max(1, p+1-d)
  
  #index <- order(arrangedSeries)
  #df <- data.frame(series, index)
  
  #arrangedSeries <- series[h:(length(series)-d)]
  
  
  z <- series
  length <- length(z)
  data <- data.frame(z)
  
  for(i in 1:p) {
    z <- series[(1+i):(length+i)]
    data <- data.frame(data,z)
  }
  
  # seriesd: series of threshold values by which the cases of data will be sorted (z_(t-d))
  thresholdSeries <- series[(1+d):(length+d)]
  data <- data.frame(data, thresholdSeries)
  
  # remove NA rows
  data <- (data[1:(length-max(p,d)),])
  
  # rearrange cases by threshold Series
  arrangedData <- data[order(data$thresholdSeries),]
  
  m <- getm(series)
  regime <- arrangedData[1:m,1:(ncol(arrangedData)-1)]
  
  
  return(df)
}

getm <- function(series, stationary=TRUE, verbose=TRUE){
  
  if(stationary==TRUE) m <- round(3*sqrt(length(series)),0)
  else m <- round(5*sqrt(length(series)),0)
  
  if (!verbose) {
    string1 <- as.character(paste("\n"))
    string2 <- as.character(paste("   Stationary: m =",round(3*sqrt(length(series)),0),"\n"))
    string3 <- as.character(paste("   Unit Root:  m =",round(5*sqrt(length(series)),0),"\n\n"))
    cat(string1, string2, string3)
  }
  return(m)
}