# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

mj.plot <- function (ve.series, type="l", name="") {
  obs <- paste(name, ", n = ", length(ve.series))
  plot(ve.series, type = type, col = 1, xlab = obs)
}

mj.lines <- function (ve.series, type="l") {
  lines(ve.series, type = type, col = round(runif(1, 2, 6), 0))
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

getAR <- function(ve.series, p=0) {
  N <- length(ve.series)
  ve.y <- ve.series
  if (p == 0) {
    p <- round(((VARselect(ve.y)$selection[1] + VARselect(ve.y)$selection[3]) / 2), digits = 0)
  }
  
  ve.y <- c(ve.y, rep(NA, p))
  df.data <- data.frame(ve.y)
  
  for(i in 1:p) {
    ve.y <- c(rep(NA, i), ve.series, rep(NA, (p - i)))
    df.data <- data.frame(df.data, ve.y)
  }
  # return(data)
  return(df.data[(p + 1):N, ])
}


lag <- function(ve.series, sc.lag = 1) {
  ve.data <- c(ve.series[(sc.lag + 1):length(ve.series)], rep(NA, sc.lag))
  return(ve.data)
}