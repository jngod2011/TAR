# p .. lag order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes
# m .. regime starting size
# N .. original sample size+
# n .. sample size after losing observations due to AR terms
# see 1998 Martens et al, Appendix Step 2

myNonLinearityTest2 <- function(series, p=0, S=1, k=3, method="MARTENS") { 
  
  v.y <- as.numeric(series)
  sc.N <- as.numeric(length(y))
  
  # auto select lag order p
  if(p==0) { 
    p <- as.numeric(round(((VARselect(v.y)$selection[1]+VARselect(v.y)$selection[3])/2), digits = 0))
  }
  
  # generate AR dataframe
  df.y <- getAR(v.y, p)
  
  # calculate test statistc
  v.fStat <- data.frame(NULL)
  
  for (d in 1:S) {
    df.z <- df.y[ order( df.y[,(d+1)] ), ] # order by threshold variable z_(t-d)
    v.fStat <- c(v.fStat, getFStats(df.z, d, p, method))
  }
  
  # get threshold variable with highest F-statistic
  dMax <- which.max(as.numeric(v.fStat))
  
  df.z <- df.y[ order( df.y[,(dMax+1)] ), ]
  df.scatter <- getTStats(df.z, dMax, p)
  names(df.scatter)[1] <- paste("threshold z_(t-",dMax, ")",sep = "")
  
  return(v.fStat)
}


# calculate dataframe with t-Statistics for the predictive residuals to draw in a scatterplot against z_(t-d)
getTStats <- function(df.z, dMax, p, constant=FALSE) {

  # calculate regime size
  sc.m <- getRegimeSize(df.z)
  sc.n <- as.numeric(nrow(df.z))
  v.predictiveResiduals <- NULL
  df.tStats <- data.frame(NULL)
  
  if (constant==TRUE) {
    for (i in (sc.m-1):(sc.n-1)) {
      df.regime <- df.z[1:i,]
      lm.regime <- lm(y~., data = df.regime)
      sc.predResid <- as.numeric(df.z[(i+1),1] - lm.regime$coefficients[1] - (lm.regime$coefficients[-1] %*% as.numeric(df.z[(i+1),-1])  ))
      
      v.tStats <- summary(lm.regime)$coefficients[,3]
      df.tStats <- rbind.data.frame(df.tStats, v.tStats)
    }
  }
  
  else if (constant==FALSE) {
    for (i in (sc.m-1):(sc.n-1)) {
      df.regime <- df.z[1:i,]
      lm.regime <- lm(y~.-1, data = df.regime)
      sc.predResid <- as.numeric(df.z[(i+1),1] - (lm.regime$coefficients %*% as.numeric(df.z[(i+1),-1])  ))
      
      v.tStats <- summary(lm.regime)$coefficients[,3]
      df.tStats <- rbind.data.frame(df.tStats, v.tStats)
    }
  }
  
  names(df.tStats) <- names(v.tStats)
  names(df.tStats) <- gsub("y", "t", names(df.tStats)) # replace y with t in this vector of names
  
  df.tStats <- cbind.data.frame(df.regime[-(1:(m-2)),(dMax+1)], df.tStats) # add threshold variable to df
  
  return(df.tStats)
}

getFStats <- function(df.z, d, p, method) { # calculate predictive residuals and according F-statistic

  # calculate regime size
  sc.m <- getRegimeSize(df.z)
  sc.n <- as.numeric(nrow(df.z))
  v.predictiveResiduals <- NULL
  v.eta <- NULL
  
  # recursively calculate predictive residuals (Martens et al)
  if(method=="MARTENS") { 
    for (i in sc.m:sc.n) {
      df.regime <- df.z[1:i,]
      v.predictiveResiduals <- c(v.predictiveResiduals, summary(lm(y~., data=df.regime))$residuals[i])
    }
  }
  
  # forecast residual of next period (Tsay)
  # decrease starting point for i by 1 to m-1 to make predictiveResiduals same length as in MARTENS method
  if(method=="TSAY") {
    for (i in (sc.m-1):(sc.n-1)) {
      df.regime <- df.z[1:i,]
      lm.regime <- lm(y~., data = df.regime)
      sc.predResid <- as.numeric(df.z[(i+1),1] - lm.regime$coefficients[1] - (lm.regime$coefficients[-1] %*% as.numeric(df.z[(i+1),-1])  ))
      
      mat.V <- matrix(data=0, nrow=p, ncol=p)
      mat.V <- solve( getSumOuterProducts(df.regime[,-1]) )
      sc.normalizer <- as.numeric(sqrt(1 + as.numeric(df.z[(i+1), -1]) %*% mat.V %*% as.numeric(df.z[(i+1), -1])))
      v.eta <- c(v.eta, sc.predResid/sc.normalizer)
      
    }
    v.predictiveResiduals <- v.eta
    
  }
  
  # regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
  # hence we would have a linear model. if there is explanatory power, we prefer a TAR model
  # now the regression is predictiveResiduals~df.y[60:n,], first m-1 obs. are left out 
  df.test <- data.frame(v.predictiveResiduals, df.z[sc.m:sc.n, 2:(ncol(df.z))])
  v.estimatedResiduals <- summary(lm(v.predictiveResiduals~., data=df.test))$residuals
  
  
  # calculate final test statistic
  myplot(v.predictiveResiduals, name="predictiveResiduals")
  
  sc.FStat <- ((sum(predictiveResiduals^2)-sum(estimatedResiduals^2)) / (p+1)) / (  sum(estimatedResiduals^2) / (n-d-m-p)  )
  sc.S0 <- 1/(n-h-m) * sum(v.predictiveResiduals^2)
  sc.S1 <- 1/(n-h-m) * sum(v.estimatedResiduals^2)
  sc.CStat <- (n-h-m- ( (p+1)*p) + 1 ) * ( log(sc.S0) - log(sc.S1) )
  
  if(method=="TSAY") sc.stat <- sc.CStat
  else sc.stat <- sc.FStat
  
  return(sc.stat)
}

# calculate m
getRegimeSize <- function(df, stationary=TRUE, verbose=FALSE){
  if(stationary==TRUE) sc.regimeSize <- round(3*sqrt(nrow(df)),0)
  else sc.regimeSize <- round(5*sqrt(nrow(df)),0)
  
  if (verbose) {
    string1 <- as.character(paste("\n"))
    string2 <- as.character(paste("   Stationary: suggested regime size m =",round(3*sqrt(length(series)),0),"\n"))
    string3 <- as.character(paste("   Unit Root:  suggested regime size m =",round(5*sqrt(length(series)),0),"\n\n"))
    cat(string1, string2, string3)
  }
  return(sc.regimeSize)
}

getSumOuterProducts <- function(data) {
  V <- matrix(0, nrow=ncol(data), ncol=ncol(data))
  
  for(i in 1:nrow(data)) {
    X <- as.numeric(data[i,])
    V <- V+(X%o%X)
  }
  
  return(V)
}
