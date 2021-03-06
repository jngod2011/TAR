# Everything threshold related
# 
# Author: michael
###############################################################################

getGridIndices <- function(candidateIndex, intervalSize) {
    return(ceiling(seq(from = candidateIndex - (intervalSize / 2), to = candidateIndex + (intervalSize / 2))))
}

getRange <- function(domain, ve.domain, ve.range) {
    return(which(ve.domain[domain] == ve.range))
}

# if decreasing = FALSE, regimes will be split with a < sign
# if decreasing = TRUE, regimes will be split with a > sign
getThresholds <- function(ve.series, ve.thresholdLag, ve.indices, d, intervalSize = 10, decreasing = FALSE) {
    
    df.AR <- getAR(ve.series, p = getOptimalLagOrder(ve.series, verbose = FALSE))
    df.ordered <- df.AR[order(df.AR[, (d + 1)], decreasing = decreasing), ]
    
    # dataframe with columns of candidates around a potential candidate
    df.gridIndices <- foreach(i = 1:length(ve.indices), .combine = cbind.data.frame) %do% {
        getGridIndices(getRange(ve.indices[i], ve.thresholdLag, df.ordered[, (d + 1)]), intervalSize)
    }
    
    df.cartesian <- expand.grid(df.gridIndices)
    
    
    ve.aggregateRSquared <- foreach(i = 1:ncol(df.cartesian), .combine = 'c') %do% {
        
    }
    
    return(df.cartesian)
    
    
}








