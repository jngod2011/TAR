# p .. lag order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes
# m .. regime starting size
# see 1998 Martens et al, Appendix Step 2

myNonLinearityTest2 <- function(series, p=0, S=1, k=3, method="MARTENS") { 
  
  y <- series
  N <- as.numeric(length(y))
  
  # auto select lag order p
  if(p==0) { 
    p <- as.numeric(round(((VARselect(y)$selection[1]+VARselect(y)$selection[3])/2), digits = 0))
  }
  S <- p
  
  # generate AR dataframe
  df.y <- data.frame(y)
  
  for(i in 1:p) {
    y <- series[(1+i):(N+i)]
    df.y <- data.frame(df.y, y)
  }
  df.y <- df.y[1:(N-p),]
  
  # calculate test statistc
  FStatVector <- NULL
  
  for (d in 1:S) {
    # ordere by threshold variable z_(t-d)
    df.z <- df.y[order(df.y[,(d+1)]),]
    FStatVector <- c(FStatVector, getFStat(df.z, d, p, method))
  }
  
  return(FStatVector)
}


# calculate predictive residuals with recursive LS to select optimal threshold lag d
getFStat <- function(df.z, d, p, method) { 
  
  # calculate regime size
  n <- as.numeric(nrow(df.z))
  m <- getRegimeSize(df.z)
  predictiveResiduals <- NULL
  
  if(method=="MARTENS") {
    # recursively calculate predictive residuals (Martens et al)
    for (i in m:n) {
      df.regime <- df.z[1:i,]
      predictiveResiduals <- c(predictiveResiduals, summary(lm(y~., data=df.regime))$residuals[i])
    }
  }
  
  if(method=="TSAY") {
    # forecast residual of next period (Tsay)
    # decrease starting point for i by 1 to m-1 to make predictiveResiduals same length as in MARTENS method
    for (i in (m-1):(n-1)) {
      df.regime <- df.z[1:i,]
      lm.regime <- lm(y~., data = df.regime)
      predResid <- df.z[(i+1),1] - lm.regime$coefficients[1] - sum(lm.regime$coefficients[2:(p+1)] * df.z[(i+1),2:(p+1)])
      predictiveResiduals <- c(predictiveResiduals, predResid)
    }
  }
  
  # regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
  # hence we would have a linear model. if there is explanatory power, we prefer a TAR model
  # now the regression is predictiveResiduals~df.y[60:n,], first m-1 obs. are left out obviously
  df.test <- data.frame(predictiveResiduals, df.z[m:n, 2:(ncol(df.z))])
  estimatedResiduals <- summary(lm(predictiveResiduals~., data=df.test))$residuals
  
  # numerator <- sum(predictiveResiduals^2)-sum(estimatedResiduals^2)/(p+1)
  # denominator <- sum(estimatedResiduals^2)/(n-m-p-p)
  FStat <- ((sum(predictiveResiduals^2)-sum(estimatedResiduals^2)) / (p+1)) / (  sum(estimatedResiduals^2) / (n-d-m-p)  )
  
}


# calculate m
getRegimeSize <- function(df, stationary=TRUE, verbose=FALSE){
  
  if(stationary==TRUE) regimeSize <- round(3*sqrt(nrow(df)),0)
  else regimeSize <- round(5*sqrt(nrow(df)),0)
  
  if (verbose) {
    string1 <- as.character(paste("\n"))
    string2 <- as.character(paste("   Stationary: suggested regime size m =",round(3*sqrt(length(series)),0),"\n"))
    string3 <- as.character(paste("   Unit Root:  suggested regime size m =",round(5*sqrt(length(series)),0),"\n\n"))
    cat(string1, string2, string3)
  }
  return(regimeSize)
}


