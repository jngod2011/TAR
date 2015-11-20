# Everything threshold related
# 
# Author: michael
###############################################################################

getRangeIndices <- function(candidateIndex, intervalSize) {
    return(ceiling(seq(from = candidateIndex - (intervalSize / 2), to = candidateIndex + (intervalSize / 2))))
}

getMappedIndex <- function(domain, ve.domain, ve.range) {
    return(which(ve.domain[domain] == ve.range))
}

getRegimeIndices <- function(ve.splits, total) {
    ve.splits <- as.numeric(unlist(c(0, ve.splits, total)))
    list.data <- foreach (i = 1:(length(ve.splits) - 1)) %do% {
        seq(from = (ve.splits[i] + 1), to = ve.splits[i + 1])
    }
    return(list.data)
}

# if decreasing = FALSE, regimes will be split with a < sign
# if decreasing = TRUE, regimes will be split with a > sign
getThresholds <- function(ve.series, ve.thresholdLag, ve.indices, d, intervalSize = 10) {
    
    df.AR <- getAR(ve.series, p = getOptimalLagOrder(ve.series, verbose = FALSE))
    df.ordered <- df.AR[order(df.AR[, (d + 1)]), ]
        
    # dataframe with columns of candidates around a potential candidate
    df.gridIndices <- foreach(i = 1:length(ve.indices), .combine = cbind.data.frame) %do% {
        getRangeIndices(getMappedIndex(ve.indices[i], ve.thresholdLag, df.ordered[, (d + 1)]), intervalSize)
    }
    
    df.cartesian <- expand.grid(df.gridIndices)
    nrowCartesian <- nrow(df.cartesian)
    numberRegimes <- ncol(df.cartesian) + 1
        
    ve.sumRSquared <- foreach(i = 1:nrow(df.cartesian), .combine = 'c') %do% {
        sumRSquared <- 0
        ve.splits <- df.cartesian[i, ]
        list.regimeIndices <- getRegimeIndices(ve.splits, nrowCartesian)
        for (j in 1:numberRegimes) {
            sumRSquared <- sumRSquared + summary(lm(ve.y ~ ., data = df.ordered[list.regimeIndices[[j]], ]))$r.squared
        }
        ve.sumRSquared[i] <- sumRSquared
    }
        
    return(df.cartesian[which.max(ve.sumRSquared), ])
}








