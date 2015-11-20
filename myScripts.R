# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

mj.plot <- function (ve.series, type = "l", name = "") {
    dev.new()
    obs <- paste(name, ", n = ", length(ve.series))
    plot(ve.series, type = type, col = 1, xlab = obs)
}

mj.lines <- function (ve.series, type = "l") {
  lines(ve.series, type = type, col = round(runif(1, 2, 6), 0))
}

mj.multiplot <- function (df.data, type = "b", name = "", from = -1, to = -1, col = 1) {
	#pdf(file = "111.pdf", paper = "A4", width=7, height=10)
    dev.new()
    if (from == -1) from <- 2
    if (to == -1) to <- ncol(df.data)
	par(mfrow = c((to - from + 1), 1))
	par(mar=c(3.8, 4.3, 1, 1.5))          # bottom, left, top, right
	for (i in from:to) {
        string = 
		plot(x = df.data[, 1], y = df.data[, i], type = type, pch = 5, cex.axis = 0.8, cex.lab = 0.8, 
				mgp = c(2.6, 0.8, 0), las = 1,     # 1.6 label, 0.6 tick labels, 0 ticks - positions
                xlab = names(df.data)[1], ylab = names(df.data)[i],
                col = col)
	}
	
	par(mfrow = c(1, 1))
	#dev.off()
}


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

getAR <- function(ve.series, p = 0) {
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