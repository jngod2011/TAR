# Everything threshold related
# 
# Author: michael
###############################################################################

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

# a regime always includes the last index, so the threshold for the regime is LARGER than the value at the last index:
# 1 2 3 4 5 |thr| 6 7 8 |thr| 9 10 11 12 ..
getThresholds <- function(list.data, ve.thresholdLag = -1, ve.indices, d = -1, intervalSize = 30, verbose = FALSE, 
        method = 1, increasing = TRUE) {
    
    if (d == -1) d <- list.data$dMax
    if (ve.thresholdLag == -1) ve.thresholdLag <- list.data$list.scatterAll[[(d * 2) - 1]][, 1]
    df.AR <- getAR(list.data$ve.series, p = list.data$p)
    df.ordered <- df.AR[order(df.AR[, (d + 1)]), ]
    
    # dataframe with columns of candidates around a potential candidate
    # df.gridIndices <- foreach(i = 1:length(ve.indices), .combine = cbind.data.frame) %do% {
    #     getRangeIndices(getMappedIndex(ve.indices[i], ve.thresholdLag, df.ordered[, (d + 1)]), intervalSize)
    # }
    
    df.gridIndices <- foreach(i = 1:length(ve.indices), .combine = cbind.data.frame) %do% {
        getRangeIndices(ve.indices[i], intervalSize)
    }
    
    df.cartesian <- expand.grid(df.gridIndices)
    nrowCartesian <- nrow(df.cartesian)
    numberRegimes <- ncol(df.cartesian) + 1
    
    df.RSquared <- NULL
    df.AdjRSquared <- NULL
    df.SSR <- NULL
    df.AIC <- NULL
    df.BIC <- NULL
    
    
    for (i in 1:nrow(df.cartesian)) {
        ve.splits <- df.cartesian[i, ]    
        
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
    
    
    if (verbose) {
        cat("Vector with optimal indices:", as.numeric(result), "(out of", nrow(df.cartesian), "possibilities)\n", sep = " ")    
    }
    
    list.thresholds <- list(df.cartesian = df.cartesian, df.RSquared = df.RSquared, df.AdjRSquared = df.AdjRSquared, 
            df.SSR = df.SSR, df.AIC = df.AIC, df.BIC = df.BIC, bestRSquared = bestRSquared, bestAdjRSquared = bestAdjRSquared, 
            bestSSR = bestSSR, bestAIC = bestAIC, bestBIC = bestBIC)
    
    return(list(p = list.data$p, 
                    dMax = list.data$dMax, 
                    N = list.data$N, 
                    m = list.data$m, 
                    n = list.data$N - list.data$m - list.data$p + 1, 
                    ve.series = list.data$ve.series, 
                    ve.FStats = list.data$ve.FStats, 
                    list.scatterAll = list.data$list.scatterAll, 
                    list.thresholds = list.thresholds))
    
}