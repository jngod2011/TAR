# Everything threshold related
# 
# Author: michael
###############################################################################

# a regime always includes the last index, so the threshold for the regime is LARGER than the value at the last index:
# 1 2 3 4 5 |thr| 6 7 8 |thr| 9 10 11 12 ..
# if df.thresholdIndices give for example 63, 346, this means regime 1 contains observations 1:63, then 64:346, finally 347:last
getThresholds <- function(list.data, ve.thresholdLag = -1, ve.indices = NULL, d = -1, intervalSize = 30, 
        verbose = FALSE, method = 1, increasing = TRUE, minRegimeSize = -1, k = 3) {
    
    # if no grid is specified, grid search over all data, for k regimes
    # for k = 3: [regime][index][regime][regime][index][regime]
    if (is.null(ve.indices)) {
        intervalSize <- floor(((list.data$N - list.data$p - (k - 1)) / 2) / (k - 1))
        for (i in 1:(k - 1)) ve.indices[i] <- ((2 * i) - i) * intervalSize + i
    }
    if (d == -1) d <- list.data$dMax
    if (ve.thresholdLag == -1) ve.thresholdLag <- list.data$list.scatterAll[[(d * 2) - 1]][, 1]
    if (minRegimeSize == -1) minRegimeSize <- list.data$m
    df.AR <- getAR(list.data$ve.series, p = list.data$p)
    df.ordered <- df.AR[order(df.AR[, (d + 1)]), ]
    ve.thresholdLag <- df.ordered[, (d + 1)]
    k <- length(ve.indices) + 1 # number of regimes
    
    df.gridIndices <- foreach(i = 1:length(ve.indices), .combine = cbind.data.frame) %do% {
        getRangeIndices(ve.indices[i], intervalSize)
    }
    
    df.cartesian <- expand.grid(df.gridIndices)
    df.adjCartesian <- getAdjustedCartesian(df.cartesian = df.cartesian, minRegimeSize = minRegimeSize, 
            maxIndex = list.data$N - list.data$p)
    
    nrowCartesian <- nrow(df.cartesian)
    nrowAdjCartesian <- nrow(df.adjCartesian)
    numberRegimes <- ncol(df.cartesian) + 1
    
    df.RSquared <- NULL
    df.AdjRSquared <- NULL
    df.SSR <- NULL
    df.AIC <- NULL
    df.BIC <- NULL    
    
    for (i in 1:nrow(df.adjCartesian)) {
        ve.splits <- df.adjCartesian[i, ]
        
        ve.RSquared <- NULL
        ve.AdjRSquared <- NULL
        ve.SSR <- NULL
        ve.AIC <- NULL
        ve.BIC <- NULL
        
        list.regimeIndices <- getRegimeIndices(ve.splits, (list.data$N - list.data$p))
        for (j in 1:numberRegimes) {
            LM <- lm(ve.y ~ ., data = df.ordered[list.regimeIndices[[j]], ])
            summaryLM <- summary(LM)
            ve.RSquared <- c(ve.RSquared, summaryLM$r.squared) 
            ve.AdjRSquared <- c(ve.RSquared, summaryLM$adj.r.squared)
            ve.SSR <- c(ve.SSR, as.numeric(summaryLM$residuals %*% summaryLM$residuals))
            ve.AIC <- c(ve.AIC, AIC(LM))
            ve.BIC <- c(ve.BIC, BIC(LM))
        }
        df.RSquared <- rbind(df.RSquared, ve.RSquared)
        df.AdjRSquared <- rbind(df.AdjRSquared, ve.AdjRSquared)
        df.SSR <- rbind(df.SSR, ve.SSR)
        df.AIC <- rbind(df.AIC, ve.AIC)
        df.BIC <- rbind(df.BIC, ve.BIC)
    }
    
    bestRSquared <- which.max(as.numeric(rowMeans(df.RSquared)))
    bestAdjRSquared <- which.max(as.numeric(rowMeans(df.AdjRSquared)))
    bestSSR <- which.min(as.numeric(rowMeans(df.SSR)))
    bestAIC <- which.min(as.numeric(rowMeans(df.AIC)))
    bestBIC <- which.min(as.numeric(rowMeans(df.BIC)))
    
    df.thresholds <- cbind.data.frame(
            ve.thresholdLag[as.numeric(df.adjCartesian[bestRSquared, ])],
            ve.thresholdLag[as.numeric(df.adjCartesian[bestAdjRSquared, ])],
            ve.thresholdLag[as.numeric(df.adjCartesian[bestSSR, ])],
            ve.thresholdLag[as.numeric(df.adjCartesian[bestAIC, ])],
            ve.thresholdLag[as.numeric(df.adjCartesian[bestBIC, ])]
    )
    colnames(df.thresholds) <- c("RSquared", "AdjRSquared", "SSR", "AIC", "BIC")
    
    df.thresholdIndices <- cbind.data.frame(
            as.numeric(df.adjCartesian[bestRSquared, ]),
            as.numeric(df.adjCartesian[bestAdjRSquared, ]),
            as.numeric(df.adjCartesian[bestSSR, ]),
            as.numeric(df.adjCartesian[bestAIC, ]),
            as.numeric(df.adjCartesian[bestBIC, ])
    )
    colnames(df.thresholdIndices) <- c("RSquared", "AdjRSquared", "SSR", "AIC", "BIC")
    
    if (verbose) {
        cat("Vector with optimal indices:", as.numeric(result), "(out of", nrow(df.cartesian), "possibilities)\n", sep = " ")    
    }
    
    list.thresholds <- list(df.adjCartesian = df.adjCartesian, df.RSquared = df.RSquared, df.AdjRSquared = df.AdjRSquared, 
            df.SSR = df.SSR, df.AIC = df.AIC, df.BIC = df.BIC, df.thresholds = df.thresholds, df.thresholdIndices = 
                    df.thresholdIndices)
    
    return(list(p = list.data$p, 
                    dMax = list.data$dMax, 
                    N = list.data$N, 
                    m = list.data$m, 
                    n = list.data$N - list.data$m - list.data$p + 1,
                    k = k,
                    ve.series = list.data$ve.series, 
                    ve.FStats = list.data$ve.FStats,
                    ve.thresholdLag = ve.thresholdLag,
                    df.ordered = df.ordered,
                    list.scatterAll = list.data$list.scatterAll, 
                    list.thresholds = list.thresholds))
}


getRangeIndices <- function(candidateIndex, intervalSize = 20) {
    return(ceiling(seq(from = candidateIndex - (intervalSize / 2), to = candidateIndex + (intervalSize / 2))))
}


getMappedIndex <- function(domainIndex, ve.domain, ve.range) {
    return(which(ve.domain[domainIndex] == ve.range))
}


getRegimeIndices <- function(ve.splits, total) {
    ve.splits <- as.numeric(unlist(c(0, ve.splits, total)))
    list.data <- foreach (i = 1:(length(ve.splits) - 1)) %do% {
        seq(from = (ve.splits[i] + 1), to = ve.splits[i + 1])
    }
    return(list.data)
}


getAdjustedCartesian <- function(df.cartesian, minRegimeSize, maxIndex) {
    df.cartesian <- df.cartesian[abs(1 - df.cartesian[, 1]) >= minRegimeSize, ]
    for (i in 1:(ncol(df.cartesian) - 1)) {
        df.cartesian <- df.cartesian[abs(df.cartesian[, i] - df.cartesian[, i + i]) >= minRegimeSize, ]    
    }
    df.cartesian <- df.cartesian[abs(maxIndex - df.cartesian[, ncol(df.cartesian)]) >= minRegimeSize, ] # check for last column
    
    return(df.cartesian)
}



