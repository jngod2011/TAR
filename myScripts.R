# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

myplot <- function (x, type="l", name="") {
  obs <- paste(name, ", n = ", length(x))
  plot(x, type=type, col=1, xlab=obs)
}

mylines <- function (x, type="l") {
  lines(x, type=type, col=round(runif(1,2,6),0))
}


getAR <- function(series, p=0) {
  N <- length(series)
  y <- series
  if(p==0) {
    p <- round(((VARselect(y)$selection[1]+VARselect(y)$selection[3])/2), digits = 0)
  }
  
  y <- c(y, rep(NA,p))
  data <- data.frame(y)
  
  for(i in 1:p) {
    y <- c(rep(NA,i), series, rep(NA,(p-i)))
    data <- data.frame(data,y)
  }
  # return(data)
  return(data[(p+1):N,])
}


mylag <- function(series, lag=1) {
  data <- c(series[(lag+1):length(series)],rep(NA,lag))
  return(data)
}