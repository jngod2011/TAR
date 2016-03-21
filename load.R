# load data
# delete NaNs and empty columns

df.MEI_Master <- data.frame(read.csv("data/02-2016/alldata.csv", header = TRUE, sep = ",", dec = ","))
ve.selectJP <- c(2,3,4,5,6,7,8,9)
ve.selectCH <- c(10,11,12,13,14,15,16,17)
ve.selectUK <- c(18,19,20,21,22,23,24,25)
ve.selectUS <- c(26,27,28,29,30,31,32)
ve.selectEA <- c(33,34,35,36,37,38,39,40)
ve.selectRU <- c(41,42,43,44,45,46,47,48)

df.US_JP <- df.MEI_Master[,c(1,ve.selectUS, ve.selectJP)]
df.US_CH <- df.MEI_Master[,c(1,ve.selectUS, ve.selectCH)]
df.US_UK <- df.MEI_Master[,c(1,ve.selectUS, ve.selectUK)]
df.US_EA <- df.MEI_Master[,c(1,ve.selectUS, ve.selectEA)]
df.US_RU <- df.MEI_Master[,c(1,ve.selectUS, ve.selectRU)]

df.log_US_JP <- data.frame(s=log(df.US_JP[,16]),                        # log exchange rate
        m1=log(df.US_JP[,5])-log(df.US_JP[,12]),     # log m1 index
        m3=log(df.US_JP[,6])-log(df.US_JP[,13]),     # log m3 index
        gdp=log(df.US_JP[,15])-log(df.US_JP[,8]),    # log gdp                
        irlt=df.US_JP[,2]-df.US_JP[,9],              # irlt
        irst=df.US_JP[,3]-df.US_JP[,10],             # irst
        immediate=df.US_JP[,4]-df.US_JP[,11],        # immediate
        shares=log(df.US_JP[,7])-log(df.US_JP[,14])  # log shares index
)

df.log_US_CH <- data.frame(s=log(df.US_CH[,16]),                        # log exchange rate
        m1=log(df.US_CH[,5])-log(df.US_CH[,12]),     # log m1 index
        m3=log(df.US_Ch[,6])-log(df.US_CH[,13]),     # log m3 index
        gdp=log(df.US_CH[,15])-log(df.US_CH[,8]),    # log gdp                
        irlt=df.US_CH[,2]-df.US_CH[,9],              # irlt
        irst=df.US_CH[,3]-df.US_CH[,10],             # irst
        immediate=df.US_CH[,4]-df.US_CH[,11],        # immediate
        shares=log(df.US_CH[,7])-log(df.US_CH[,14])  # log shares index
)

df.log_US_UK <- data.frame(s=log(df.US_UK[,16]),                        # log exchange rate
        m1=log(df.US_UK[,5])-log(df.US_UK[,12]),     # log m1 index
        m3=log(df.US_UK[,6])-log(df.US_UK[,13]),     # log m3 index
        gdp=log(df.US_UK[,15])-log(df.US_UK[,8]),    # log gdp                
        irlt=df.US_UK[,2]-df.US_UK[,9],              # irlt
        irst=df.US_UK[,3]-df.US_UK[,10],             # irst
        immediate=df.US_UK[,4]-df.US_UK[,11],        # immediate
        shares=log(df.US_UK[,7])-log(df.US_UK[,14])  # log shares index
)

df.log_US_EA <- data.frame(s=log(df.US_EA[,16]),                        # log exchange rate
        m1=log(df.US_EA[,5])-log(df.US_EA[,12]),     # log m1 index
        m3=log(df.US_EA[,6])-log(df.US_EA[,13]),     # log m3 index
        gdp=log(df.US_EA[,15])-log(df.US_EA[,8]),    # log gdp                
        irlt=df.US_EA[,2]-df.US_EA[,9],              # irlt
        irst=df.US_EA[,3]-df.US_EA[,10],             # irst
        immediate=df.US_EA[,4]-df.US_EA[,11],        # immediate
        shares=log(df.US_EA[,7])-log(df.US_EA[,14])  # log shares index
)

df.log_US_RU <- data.frame(s=log(df.US_RU[,16]),                        # log exchange rate
        m1=log(df.US_RU[,5])-log(df.US_RU[,12]),     # log m1 index
        m3=log(df.US_RU[,6])-log(df.US_RU[,13]),     # log m3 index
        gdp=log(df.US_RU[,15])-log(df.US_RU[,8]),    # log gdp                
        irlt=df.US_RU[,2]-df.US_RU[,9],              # irlt
        irst=df.US_RU[,3]-df.US_RU[,10],             # irst
        immediate=df.US_RU[,4]-df.US_RU[,11],        # immediate
        shares=log(df.US_RU[,7])-log(df.US_RU[,14])  # log shares index
)

rm(df.US_JP, df.US_CH, df.US_UK, df.US_EA, df.US_RU)
rm(ve.selectUS, ve.selectJP, ve.selectCH, ve.selectUK, ve.selectEA, ve.selectRU)

# EA
df.log_US_EA <- df.log_US_EA[-439,]       # remove NaN rows
df.log_US_EA <- df.log_US_EA[,-8]         # remove shares
rownames(df.log_US_EA) <- seq(1:nrow(df.log_US_EA))

# JP
df.log_US_JP <- df.log_US_JP[-439,]		    # remove NaN rows
df.log_US_JP <- df.log_US_JP[-(1:12),]	  # remove NaN rows 
rownames(df.log_US_JP) <- seq(1:nrow(df.log_US_JP))

# RU
df.log_US_RU <- df.log_US_RU[-(436:439),]	# remove NaN rows
df.log_US_RU <- df.log_US_RU[-(1:198),]   # remove NaN rows
df.log_US_RU <- df.log_US_RU[,-8]
rownames(df.log_US_RU) <- seq(1:nrow(df.log_US_RU))

# UK
df.log_US_UK <- df.log_US_UK[-439,]       # remove NaN rows
df.log_US_UK <- df.log_US_UK[-(1:96),]    # remove NaN rows
rownames(df.log_US_UK) <- seq(1:nrow(df.log_US_UK))

# deviations
ve.errorEA <- summary(lm(s~., data=df.log_US_EA))$residuals
ve.errorJP <- summary(lm(s~., data=df.log_US_JP))$residuals
ve.errorRU <- summary(lm(s~., data=df.log_US_RU))$residuals
ve.errorUK <- summary(lm(s~., data=df.log_US_UK))$residuals

df.data <- df.log_US_JP
df.data <- df.log_US_EA
ratio <- 0.75
h <- 1
Crit <- 2.32
k <- 2


# USD/EUR - linear model
list.analysisEA_k2 <- getAnalysis(df.log_US_EA, k = 2, n = 12, method = "AIC")
df.resultsEA_k2 <- formatTablesLinear(list.analysisEA_k2)
tex.resultsEA_k2 <- xtable(df.resultsEA_k2, label = "test", digits = 6)

list.analysisEA_k3 <- getAnalysis(df.log_US_EA, k = 3, n = 12, method = "AIC")
df.resultsEA_k3 <- formatTablesLinear(list.analysisEA_k3)
tex.resultsEA_k3 <- xtable(df.resultsEA_k3, label = "test", digits = 6)


# USD/JPY
list.analysisJP_k2_AIC <- getAnalysis(df.log_US_JP, k = 2, n = 12, method = "AIC")
list.analysisJP_k2_SSR <- getAnalysis(df.log_US_JP, k = 2, n = 12, method = "SSR")
df.resultsJP_k2 <- formatTablesNonlinear(list.analysisJP_k2_AIC, list.analysisJP_k2_SSR)
tex.resultsJP_k2 <- xtable(df.resultsJP_k2, digits = 6)

list.analysisJP_k3_AIC <- getAnalysis(df.log_US_JP, k = 3, n = 12, method = "AIC")
list.analysisJP_k3_SSR <- getAnalysis(df.log_US_JP, k = 3, n = 12, method = "SSR")
df.resultsJP_k3 <- formatTablesNonlinear(list.analysisJP_k3_AIC, list.analysisJP_k3_SSR)
tex.resultsJP_k3 <- xtable(df.resultsJP_k3, digits = 6)

# USD/RUB
list.analysisRU_k2_AIC <- getAnalysis(df.log_US_RU, k = 2, n = 12, method = "AIC")
list.analysisRU_k2_SSR <- getAnalysis(df.log_US_RU, k = 2, n = 12, method = "SSR")
df.resultsRU_k2 <- formatTablesNonlinear(list.analysisRU_k2_AIC, list.analysisRU_k2_SSR)
tex.resultsRU_k2 <- xtable(df.resultsRU_k2, digits = 6)

list.analysisRU_k3_AIC <- getAnalysis(df.log_US_RU, k = 3, n = 12, method = "AIC")
list.analysisRU_k3_SSR <- getAnalysis(df.log_US_RU, k = 3, n = 12, method = "SSR")
df.resultsRU_k3 <- formatTablesNonlinear(list.analysisRU_k3_AIC, list.analysisRU_k3_SSR)
tex.resultsRU_k3 <- xtable(df.resultsRU_k3, digits = 6)

# USD/GBP
list.analysisUK_k2_AIC <- getAnalysis(df.log_US_UK, k = 2, n = 12, method = "AIC")
list.analysisUK_k2_SSR <- getAnalysis(df.log_US_UK, k = 2, n = 12, method = "SSR")
df.resultsUK_k2 <- formatTablesNonlinear(list.analysisUK_k2_AIC, list.analysisUK_k2_SSR)
tex.resultsUK_k2 <- xtable(df.resultsUK_k2, digits = 6)

list.analysisUK_k3_AIC <- getAnalysis(df.log_US_UK, k = 3, n = 12, method = "AIC")
list.analysisUK_k3_SSR <- getAnalysis(df.log_US_UK, k = 3, n = 12, method = "SSR")
df.resultsUK_k3 <- formatTablesNonlinear(list.analysisUK_k3_AIC, list.analysisUK_k3_SSR)
tex.resultsUK_k3 <- xtable(df.resultsUK_k3, digits = 6)

# GFX , mar=c(3.8, 4.3, 1, 1.5)
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

# Goodness of Fit Plit
gfx.JP <- testLinearity(ve.errorJP[1:getInSampleSize(df.log_US_JP, ratio = 0.75)])
mj.plotList(gfx.JP, column = 2, ylim = c(0.9, 0.965), 
    main = "Goodness of Fit Development in Recursive OLS for USD/JPY",
    ylab = "Goodness of Fit"
) 
abline(v = 53, col = "darkgrey") # m
# text for m
text(57, y = 0.9, labels = "m = 53", adj = NULL,
    pos = 4, offset = 0, vfont = NULL,               # pos can be NULL, then offset is irrelevant. 
    cex = 0.75, col = NULL, font = NULL              # 1 = bottom, 2 = left, 3 = top, 4 = right
)
legend(220, 0.96, # places a legend at the appropriate place 
    cex = 0.75,
    c("Increasing", "Decreasing"), # puts text in the legend
    lty = c(1, 1), # gives the legend appropriate symbols (lines)
    lwd = c(1, 1),
    col = c("violetred", "springgreen") # gives the legend lines the correct color and width
) 




df.tableJP <- formatTablesNonlinear(list.analysisJP_k3_AIC, list.analysisJP_k3_SSR)
xtable(df.tableJP, caption = "Results for USD/JPY with $k = 3$ regimes", label = "tab:5_JP_k3", digits = 6)