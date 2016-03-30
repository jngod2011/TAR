
# -----------------------------------      plot all deviations k = 2, k = 3
# ---------------------------- data
a <- getInSampleSize(df.log_US_JP, ratio = 0.75)
df.inSampleJP <- df.log_US_JP[1:a, ]
ve.errorJP <- summary(lm(s ~ ., data = df.inSampleJP))$residuals
list.testLinearityJP <- testLinearity(ve.errorJP, p = -1)
k <- 2
list.thresholdsJP_k2 <- getThresholds(list.testLinearityJP, k = k)
k <- 3
list.thresholdsJP_k3 <- getThresholds(list.testLinearityJP, k = k)

a <- getInSampleSize(df.log_US_RU, ratio = 0.75)
df.inSampleRU <- df.log_US_RU[1:a, ]
ve.errorRU <- summary(lm(s ~ ., data = df.inSampleRU))$residuals
list.testLinearityRU <- testLinearity(ve.errorRU, p = -1)
k <- 2
list.thresholdsRU_k2 <- getThresholds(list.testLinearityRU, k = k)
k <- 3
list.thresholdsRU_k3 <- getThresholds(list.testLinearityRU, k = k)

a <- getInSampleSize(df.log_US_UK, ratio = 0.75)
df.inSampleUK <- df.log_US_UK[1:a, ]
ve.errorUK <- summary(lm(s ~ ., data = df.inSampleUK))$residuals
list.testLinearityUK <- testLinearity(ve.errorUK, p = -1)
k <- 2
list.thresholdsUK_k2 <- getThresholds(list.testLinearityUK, k = k)
k <- 3
list.thresholdsUK_k3 <- getThresholds(list.testLinearityUK, k = k)


par(mfrow = c(3, 2))

plot(ve.errorJP, 
    main = "Deviations USD/JPY, k = 2 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsJP_k2$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
abline(h = list.thresholdsJP_k2$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = 220, y = -0.12, labels = expression('C'["1,AIC"]*' = '*'C'["1,SSR"]), adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Threshold SSR", "Threshold AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# k = 3
plot(ve.errorJP, 
    main = "Deviations USD/JPY, k = 3 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsJP_k3$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
abline(h = list.thresholdsJP_k3$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = c(230, 300), y = c(-0.115, 0.12), 
    labels = c(expression('C'["1,SSR"]), expression('C'["2,SSR"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
text(x = c(254, 300), y = c(-0.0668, 0.05), 
    labels = c(expression('C'["1,AIC"]), expression('C'["2,AIC"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Thresholds SSR", "Thresholds AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# --------------------------------------------- RU ---------------------------------------------------------

plot(ve.errorRU, 
    main = "Deviations USD/RUB, k = 2 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsRU_k2$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
abline(h = list.thresholdsRU_k2$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = c(95, 115), y = c(-0.055, -0.135), 
    labels = c(expression('C'["1,AIC"]), expression('C'["1,SSR"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Threshold SSR", "Threshold AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# k = 3
plot(ve.errorRU, 
    main = "Deviations USD/RUB, k = 3 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsRU_k3$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
abline(h = list.thresholdsRU_k3$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = c(95, 115), y = c(-0.06, -0.14), 
    labels = c(expression('C'["1,AIC"]), expression('C'["1,SSR"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
text(x = 132, y = 0.09, 
    labels = expression('C'["2,AIC"]*' = '*'C'["2,SSR"]), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Thresholds SSR", "Thresholds AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# ----------------------------------------------------- UK ----------------------------------------------------

plot(ve.errorUK, 
    main = "Deviations USD/GBP, k = 2 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsUK_k2$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
abline(h = list.thresholdsUK_k2$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = 190, y = -0.06, labels = c(expression('C'["1,AIC"]*', '*'C'["1,SSR"])), adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Threshold SSR", "Threshold AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# k = 3
plot(ve.errorUK, 
    main = "Deviations USD/GBP, k = 3 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsUK_k3$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
abline(h = list.thresholdsUK_k3$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = c(210, 240), y = c(-0.055, 0.048), 
    labels = c(expression('C'["1,SSR"]), expression('C'["2,SSR"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
text(x = c(175, 245), y = c(-0.0325, 0.013), 
    labels = c(expression('C'["1,AIC"]), expression('C'["2,AIC"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Thresholds SSR", "Thresholds AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

par(mfrow = c(1, 1))










par(mfrow = c(2, 1))
plot(
    rowMeans(list.thresholds$list.allThresholds$df.AIC), 
    main = "Aggregated AIC for all regimes of USD/JPY", 
    ylab = "AIC", 
    xlab = "Regimes",
    cex = 0.5,
    cex.axis = 1,
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
points(
    x = which.min(rowMeans(list.thresholds$list.allThresholds$df.AIC)), 
    y = rowMeans(list.thresholds$list.allThresholds$df.AIC)[which.min(rowMeans(list.thresholds$list.allThresholds$df.AIC))], 
    col = 2, pch = 12, cex = 1.7
)

plot(
    rowMeans(list.thresholds$list.allThresholds$df.SSR), 
    main = "Aggregated SSR for all regimes of USD/JPY", 
    ylab = "SSR", 
    xlab = "Regimes",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
points(
    x = which.min(rowMeans(list.thresholds$list.allThresholds$df.SSR)), 
    y = rowMeans(list.thresholds$list.allThresholds$df.SSR)[which.min(rowMeans(list.thresholds$list.allThresholds$df.SSR))], 
    col = 2, pch = 12, cex = 1.7
)
par(mfrow = c(1, 1))

# Goodness of Fit Plot
gfx.JP <- testLinearity(ve.errorJP[1:getInSampleSize(df.log_US_JP, ratio = 0.75)])
mj.plotList(gfx.JP, column = 2, ylim = c(0.9, 0.965), 
    main = "Goodness of Fit Development in Recursive OLS for USD/JPY",
    ylab = "Goodness of Fit",
    xlab = ""
) 
abline(v = 53, col = "darkgrey") # m
# text for m
text(57, y = 0.9, labels = "m = 53", adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend("topright", # places a legend at the appropriate place 
    cex = 0.95,
    c("Increasing", "Decreasing"), # puts text in the legend
    lty = c(1, 1), # gives the legend appropriate symbols (lines)
    lwd = c(1, 1),
    col = c("violetred", "springgreen") # gives the legend lines the correct color and width
) 

# deviations for JPY for k = 2, k = 3
df.data <- df.log_US_JP
a <- getInSampleSize(df.data, ratio = 0.75)
N <- nrow(df.data)
df.inSample <- df.data[1:a, ]
ve.error <- summary(lm(s ~ ., data = df.inSample))$residuals
list.testLinearityJP <- testLinearity(ve.error, p = p)
k <- 2
list.thresholdsJP_k2 <- getThresholds(list.testLinearityJP, k = k)
k <- 3
list.thresholdsJP_k3 <- getThresholds(list.testLinearityJP, k = k)

par(mfrow = c(2, 1))
plot(ve.error, 
    main = "Deviations USD/JPY, k = 2 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsJP_k2$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
abline(h = list.thresholdsJP_k2$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = 220, y = -0.12, labels = expression('C'["1,AIC"]*' = '*'C'["1,SSR"]), adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Threshold SSR", "Threshold AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 

# k = 3
plot(ve.error, 
    main = "Deviations USD/JPY, k = 3 regimes",
    type = "l",
    lty = 1,
    ylab = expression('z'[t]), 
    xlab = "",
    cex = 0.5,
    cex.axis = 1, 
    cex.main = 1.3,
    mgp = c(2.6, 0.8, 0), 
    las = 1
)
abline(h = 0, col = 1, lty = 1, lwd = 1)
abline(h = list.thresholdsJP_k3$list.thresholds$df.thresholds$SSR, col = "red3", lty = 2, lwd = 2)
abline(h = list.thresholdsJP_k3$list.thresholds$df.thresholds$AIC, col = "royalblue", lty = 6, lwd = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = FALSE)
text(x = c(230, 300), y = c(-0.115, 0.12), 
    labels = c(expression('C'["1,SSR"]), expression('C'["2,SSR"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
text(x = c(254, 300), y = c(-0.0668, 0.05), 
    labels = c(expression('C'["1,AIC"]), expression('C'["2,AIC"])), 
    adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.95, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(x = "bottomright",  # places a legend at the appropriate place 
    cex = 0.95,
    c("Thresholds SSR", "Thresholds AIC"), # puts text in the legend
    lty = c(2, 6), # gives the legend appropriate symbols (lines)
    lwd = c(2, 2),
    col = c("red3", "royalblue") # gives the legend lines the correct color and width
) 
par(mfrow = c(1, 1))
