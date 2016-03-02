# p .. lag order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes
# m .. regime starting size
# N .. original sample size+
# n .. sample size after losing observations due to AR terms
# see 1998 Martens et al, Appendix Step 2

myNonLinearityTest2 <- function(series, p=0, S=1, k=3, method="MARTENS") { 
  
  y <- as.numeric(series)
  N <- as.numeric(length(y))
  
  # auto select lag order p
  if(p==0) { 
    p <- as.numeric(round(((VARselect(y)$selection[1]+VARselect(y)$selection[3])/2), digits = 0))
  }
  
  # generate AR dataframe
  df.y <- getAR(y)
  
  # calculate test statistc
  FStatVector <- data.frame(NULL)
  
  for (d in 1:S) {
    df.z <- df.y[ order( df.y[,(d+1)] ), ] # order by threshold variable z_(t-d)
    FStatVector <- c(FStatVector, getFStat(df.z, d, p, method))
  }
  
  return(FStatVector)
}

getFStat <- function(df.z, d, p, method) { # calculate predictive residuals and according F-statistic

  # calculate regime size
  m <- getRegimeSize(df.z)
  n <- as.numeric(nrow(df.z))
  predictiveResiduals <- NULL
  eta <- NULL
  
  # recursively calculate predictive residuals (Martens et al)
  if(method=="MARTENS") { 
    for (i in m:n) {
      df.regime <- df.z[1:i,]
      predictiveResiduals <- c(predictiveResiduals, summary(lm(y~., data=df.regime))$residuals[i])
    }
  }
  
  # forecast residual of next period (Tsay)
  # decrease starting point for i by 1 to m-1 to make predictiveResiduals same length as in MARTENS method
  if(method=="TSAY") {
    for (i in (m-1):(n-1)) {
      df.regime <- df.z[1:i,]
      lm.regime <- lm(y~., data = df.regime)
      #predResid <- df.z[(i+1),1] - lm.regime$coefficients[1] - sum(lm.regime$coefficients[-1] * df.z[(i+1),-1])
      predResid <- as.numeric(df.z[(i+1),1] - lm.regime$coefficients[1] - (lm.regime$coefficients[-1] %*% as.numeric(df.z[(i+1),-1])  ))
      predictiveResiduals <- c(predictiveResiduals, predResid)
      
      V <- matrix(data=0, nrow=p, ncol=p)
      V <- solve( getSumOuterProducts(df.regime[,-1]) )
      normalizer <- as.numeric(sqrt(1 + as.numeric(df.z[(i+1), -1]) %*% V %*% as.numeric(df.z[(i+1), -1])))
      eta <- c(eta, predResid/normalizer)
      
      # for(j in 1:i) {
      #   X <- as.numeric(df.z[i, 2:(p+1)])
      #   V <- V+X%o%X # outer product: XX'
      # }
      # normalizer <- sqrt(1 + as.numeric(df.z[(i+1),2:(p-1)])%*%solve(V)%*%as.numeric(df.z[(i+1),2:(p-1)]))
      
      # eta <- c(eta, (predResid/normalizer))
      
      #predictiveResiduals <- eta
    }
  }
  
  # regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
  # hence we would have a linear model. if there is explanatory power, we prefer a TAR model
  # now the regression is predictiveResiduals~df.y[60:n,], first m-1 obs. are left out 
  df.test <- data.frame(predictiveResiduals, df.z[m:n, 2:(ncol(df.z))])
  df.test.eta <- data.frame(eta, df.z[m:n, -1])
  estimatedResiduals <- summary(lm(predictiveResiduals~., data=df.test))$residuals
  estimatedResiduals2 <- summary(lm(eta~., data=df.test))$residuals
  
  # calculate final test statistic
  
  myplot(predictiveResiduals, name="predictiveResiduals")
  #mylines(eta)
  #myplot(estimatedResiduals, name="estimatedResiduals")
  
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

getSumOuterProducts <- function(data) {
  V <- matrix(0, nrow=ncol(data), ncol=ncol(data))
  
  for(i in 1:nrow(data)) {
    X <- as.numeric(data[i,])
    V <- V+(X%o%X)
  }
  return(V)
}
