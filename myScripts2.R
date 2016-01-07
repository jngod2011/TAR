# 1 black - 2 red - 3 green - 4 blue - 5 cyan - 6 magenta

mj.plot <- function (ve.series, type = "l", name = "", col = 1, ve.points = -1, new = TRUE, interval = 0,
        grid = TRUE) {
    dev.new()
    
    sequence <- seq_len(length(ve.series))
    splitSequence <- sequence[sequence %% interval == 0]
    
    obs <- paste("n = ", length(ve.series), sep = "")
    plot(ve.series, type = type, col = col, xlab = obs, las = 1, 
            mgp = c(2.6, 0.8, 0), las = 1,     # 1.6 label, 0.6 tick labels, 0 ticks - positions
            cex.axis = 0.8, cex.lab = 0.8, cex.main = 1, 
            col.main = 3, main = name)
    
    if (grid) grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
    if (min(ve.points > 0)) for (j in 1:length(ve.points)) points(ve.points[j], ve.series[ve.points[j]], pch = 19, col = 2)
    for (k in 1:length(splitSequence)) abline(v = splitSequence[k], col = "cornflowerblue", lty = "dotted")
    
    if (!new) dev.off()
}

mj.plotList <- function (list.data, type = "l", name = "", col = 0, column = 1, ve.points = -1, ve.verticals = -1, 
        new = TRUE, interval = 0, ylim = -1, grid = TRUE) {
    dev.new()
    
    color1 <- c("violet", "violetred", "violetred1", "violetred2", "violetred3", "violetred4")
    color2 <- c("seagreen", "springgreen", "springgreen1", "springgreen2", "springgreen3", "springgreen4")
    
    sequence <- seq_len(list.data$N - list.data$p + 50)
    splitSequence <- sequence[sequence %% interval == 0]
    
    obs <- paste("n = ", (list.data$N - list.data$p), sep = "")
    
    if (min(ylim) == -1) ylim <- c(min(list.data$list.scatterAll[[1]][, column], na.rm = TRUE), 
    max(list.data$list.scatterAll[[1]][, column], na.rm = TRUE))
    plot(seq(0, (list.data$N - list.data$p)), type = type, col = 0, 
            xlab = obs, las = 1, 
            mgp = c(2.6, 0.8, 0), las = 1,     # 1.6 label, 0.6 tick labels, 0 ticks - positions
            cex.axis = 0.8, cex.lab = 0.8, cex.main = 1, 
            col.main = 1, main = name, ylim = ylim)
    
    if (grid) grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
    if (min(ve.points != -1)) for (j in 1:length(ve.points)) points(ve.points[j], ve.series[ve.points[j]], pch = 19, col = 2)
    if (min(ve.verticals != -1)) for (j in 1:length(ve.points)) abline(ve.verticals[j], ve.series[ve.points[j]], pch = 19, col = 2)
    for (k in 1:length(splitSequence)) abline(v = splitSequence[k], col = "cornflowerblue", lty = "dotted")
    
    for (l in 1:length(list.data$list.scatterAll)) {
        if (l %% 2 == 0) lines(c(rep(NA, (list.data$m - 1)), list.data$list.scatterAll[[l]][, column]), 
                    type = "l", col = color1[floor(l/2 + 1)]) # Purple: Decreasing
        if (l %% 2 == 1) lines(c(rep(NA, (list.data$m - 1)), list.data$list.scatterAll[[l]][, column]), 
                    type = "l", col = color2[floor(l/2 + 1)]) # Green: Increasing
    }
    
    if (!new) dev.off()
}

mj.lines <- function (ve.series, type = "l") {
  lines(ve.series, type = type, col = round(runif(1, 2, 6), 0))
}

mj.multiplot <- function (df.data, type = "b", name = "", from = 2, to = 2, col = 1, ve.points = 1, new = FALSE, 
        interval = 0) {
	#pdf(file = "111.pdf", paper = "A4", width=7, height=10)
    dev.new()
    
    sequence <- seq_len(nrow(df.data))
    splitSequence <- sequence[sequence %% interval == 0]
    
    if (from == -1) from <- 2
    if (to == -1) to <- ncol(df.data)
	par(mfrow = c((to - from + 1), 1))
	par(mar=c(3.8, 4.3, 1, 1.5))          # bottom, left, top, right
	for (i in from:to) {
        string = 
		plot(x = df.data[, 1], y = df.data[, i], type = type, pch = 5, 
                cex.axis = 0.8, cex.lab = 0.8, cex.main = 1, 
				mgp = c(2.6, 0.8, 0), las = 1,     # 1.6 label, 0.6 tick labels, 0 ticks - positions
                xlab = names(df.data)[1], ylab = names(df.data)[i], main = name,
                col = col, col.main = 3)

        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
        for (j in 1:length(ve.points)) points(df.data[ve.points[j], 1], df.data[ve.points[j], i], pch = 19, col = 2)
        for (k in 1:length(splitSequence)) abline(v = df.data[splitSequence[k], 1], col = "cornflowerblue", lty = "dotted")        
	}
	
    par(mfrow = c(1, 1))
	if (!new) dev.off()
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


lag <- function(ve.series, lag = 1) {
    return(c(rep(NA, lag), ve.series[1:(length(ve.series) - lag)]))
}

diff <- function(ve.series, diff = 1) {
    return(ve.series[2:length(ve.series)] - ve.series[1:(length(ve.series)-1)])
}