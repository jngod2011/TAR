FTest <- function(series, p, d) {
  ve.y <- series
  df.y <- getAR(v.y, p)
  sc.m <- getRegimeSize(df.y)
  sc.n <- nrow(df.y)
  df.z <- df.y[ order( df.y[,(d+1)] ), ]
  ve.predResid <- NULL
  
  for(i in (sc.m+1):sc.n) {
    df.regime <- df.z[1:(i-1),]
    lm.regime <- lm(y~., data=df.regime)
    sc.resid <- as.numeric(lm.regime$residuals[i-1])
    ve.predResid <- c(ve.predResid, sc.resid)
  }
  
  df.estimatedResiduals <- cbind.data.frame(ve.predResid, df.z[-(1:sc.m),-1])
  ve.estimResid <- as.numeric(lm(ve.predResid~., data=df.estimatedResiduals)$residuals)
  sc.FStat <- ((sum(ve.predResid^2)-sum(ve.estimResid^2))/5) / ( sum(ve.estimResid^2 )/(sc.n-p-sc.m-d) )
  
  cat(sc.FStat)
  return(ve.predResid)
}