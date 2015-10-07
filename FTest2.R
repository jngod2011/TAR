FTest <- function(series, p, d) {
  ve.y <- series
  df.y <- getAR(v.y, p)
  sc.m <- getRegimeSize(df.y)
  sc.n <- nrow(df.y)
  df.z <- df.y[ order( df.y[,(d+1)] ), ]
  ve.predResid <- NULL
  ve.tStat <- NULL
  df.tStats <- NULL
  df.scatter <- NULL
  
  for(i in sc.m:sc.n) {
    df.regime <- df.z[1:i,]
    lm.regime <- lm(y~., data=df.regime)
    
    sc.resid <- as.numeric(lm.regime$residuals[i])
    ve.predResid <- c(ve.predResid, sc.resid)
    
    ve.tStat <- summary(lm.regime)$coefficients[,3]
    df.tStats <- rbind.data.frame(df.tStats, ve.tStat)
  }
  
  df.estimatedResiduals <- cbind.data.frame(ve.predResid, df.z[-(1:(sc.m-1)),-1])
  ve.estimResid <- as.numeric(lm(ve.predResid~., data=df.estimatedResiduals)$residuals)
  sc.FStat <- ((sum(ve.predResid^2)-sum(ve.estimResid^2))/(p+1)) / ( sum(ve.estimResid^2 )/(sc.n-p-sc.m-d) )
  
  df.scatter <- cbind.data.frame(df.z[(sc.m:sc.n),(d+1)], df.tStats)
  cat(sc.FStat)
  return(df.scatter)
}