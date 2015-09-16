library(vars)

# p .. lag order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes
# m .. regime starting size
# see 1998 Martens et al, Appendix Step 2

myNonLinearityTest <- function(series, p=0, S, k=3) { 
  
  if(p==0) { # auto select lag order p
    p <- round(((VARselect(series)$selection[1]+VARselect(series)$selection[3])/2), digits = 0)
  }
  
  m <- getRegimeSize(series)
  N <- length(series)
  FStatVector <- NULL
  
  z <- series
  data <- data.frame(z)
  
  
  # generate AR dataframe, cut off lost values
  for(i in 1:p) {
    z <- series[(1+i):(length+i)]
    data <- data.frame(data,z)
  }
  data <- data[1:(nrow(data)-p),]
  
  
  # calculate test statistic
  for (d in S) {
    FStat <- NULL
    FStat <- getFStat(data, m, d, p, N)
    FStatVector <- c(FStatVector, FStat)
    
  }
  
  return(FStatVector)
}


# calculate predictive residuals with recursive LS to select optimal threshold lag d
# data is already sorted according to threshold value (z_(t-d))
getFStat <- function(data, m, d, p, N=N) { 
  predictiveResiduals <- NULL
  resid <- NULL
  h <- max(1, p+1-d)
  
  data <- data[order(data[d+1]),] # arrange by threshold d, regressors stat in 2nd column, hence d+1
  
  for (t in m:(nrow(data)-1)) {
    regime <- data[1:t,]
    regime.lm <- lm(z~., data = regime)
    resid <- data[t+1,1]-(sum(data[t+1,2:(p+1)]*regimelm$coefficients[2:(p+1)])+regimelm$coefficients[1])
    predictiveResiduals <- as.numeric(c(predictiveResiduals,resid))
  }
  
  residualDF <- data.frame(predictiveResiduals,data[(m+1):nrow(data),2:(p+1)])
  estimatedResiduals <- summary(lm(predictiveResiduals~., data=residualDF))$residuals
  
  FStat <- ((sum(predictiveResiduals^2)-sum(estimatedResiduals^2))/(p+1))/(sum(estimatedResiduals^2)/(N-d-m-p-h))
  
  return(FStat)
  
}




# calculate m
getRegimeSize <- function(series, stationary=TRUE, verbose=FALSE){
  
  if(stationary==TRUE) regimeSize <- round(3*sqrt(length(series)),0)
  else regimeSize <- round(5*sqrt(length(series)),0)
  
  if (verbose) {
    string1 <- as.character(paste("\n"))
    string2 <- as.character(paste("   Stationary: suggested regime size m =",round(3*sqrt(length(series)),0),"\n"))
    string3 <- as.character(paste("   Unit Root:  suggested regime size m =",round(5*sqrt(length(series)),0),"\n\n"))
    cat(string1, string2, string3)
  }
  return(regimeSize)
}


