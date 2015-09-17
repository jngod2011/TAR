# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

myplot <- function (x, type="l") {
  plot(x, type=type, col=1)
}

mylines <- function (x, type="l") {
  lines(x, type=type, col=round(runif(1,2,6),0))
}


myar2 <- function(z, lags=0) {
  if(lags==0) {
    lags <- round(((VARselect(z)$selection[1]+VARselect(z)$selection[3])/2), digits = 0)
  }
  N <- length(z)
  df.z <- data.frame(NULL)
  # generate AR dataframe 2nd version
  df.z <- rep(1,(N+p))
  df.z <- NULL
  df.z <- data.frame(c(rep(NA,p),z))
  for(i in p:1) {
   z <- c(rep(NA,(i-1)), series, rep(NA,(p-i+1)))
   df.z <- data.frame(df.z, z)
  }
}





myar <- function(z, lags=0) {
  if(lags==0) {
    lags <- round(((VARselect(z)$selection[1]+VARselect(z)$selection[3])/2), digits = 0)
  }
  series <- z
  length <- length(z)
  data <- data.frame(z)
  
   for(i in 1:lags) {
     z <- series[(1+i):(length+i)]
     data <- data.frame(data,z)
   }
  
  # remove NA rows
  data <- (data[1:(length(z)-lags),])
  AR <- lm(z~., data=data)
  return(AR)
}

mylag <- function(series, lag=1) {
  data <- c(series[(lag+1):length(series)],rep(NA,lag))
  return(data)
}