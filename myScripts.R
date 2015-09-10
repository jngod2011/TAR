# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

library(vars)

myplot <- function (x, type="l") {
  plot(x, type=type, col=1)
}

mylines <- function (x, type="l") {
  lines(x, type=type, col=round(runif(1,2,6),0))
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