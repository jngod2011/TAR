FTest <- function(series, p, d) {
  ve.y <- series
  df.y <- getAR(ve.y, p)
  m <- getRegimeSize(df.y)
  n <- nrow(df.y)
  df.z <- df.y[order(df.y[, (d + 1)] ), ]
  ve.predResid <- NULL
  ve.tStat <- NULL
  df.tStats <- NULL
  df.scatter <- NULL
  
  for(i in m:n) {
    df.regime <- df.z[1:i, ]
    lm.regime <- lm(ve.y ~ ., data=df.regime)
    
    resid <- as.numeric(lm.regime$residuals[i])
    ve.predResid <- c(ve.predResid, resid)
    
    ve.tStat <- summary(lm.regime)$coefficients[, 3]
    df.tStats <- rbind.data.frame(df.tStats, ve.tStat)
  }
  
  df.estimatedResiduals <- cbind.data.frame(ve.predResid, df.z[-(1:(m - 1)), -1])
  ve.estimResid <- as.numeric(lm(ve.predResid ~ ., data = df.estimatedResiduals)$residuals)
  FStat <- ((sum(ve.predResid^2) - sum(ve.estimResid^2)) / (p + 1)) / ( sum(ve.estimResid^2 ) / (n - p - m - d) )
  
  df.scatter <- cbind.data.frame(df.z[(m:n),(d + 1)], df.tStats)
  cat(FStat)
  return(df.scatter)
}