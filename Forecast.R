# TODO: Add comment
# 
# Author: Michi
###############################################################################


getForecast <- function (df.data, list.data, method = "SSR") {
    
    # put mispricing AR data in the original timely order, combine with macro model, reorder by threshold lag, split into regimes
    df.ordered.reordered <- list.data$df.ordered[order(as.numeric(rownames(list.data$df.ordered))), ]
    df.vecm <- data.frame(df.data[-(1:list.data$p), ], df.ordered.reordered[, -1]) # exclude the contemporal mispricing value y_t
    thresholdLagIndex <- ncol(df.data) + list.data$dMax # threshold lag column index
    df.vecmOrdered <- df.vecm[order(df.vecm[, thresholdLagIndex]), ] # sort by this index
        
    list.regimeIndices <- getRegimeIndices(
            ve.splits = list.data$list.thresholds$df.thresholdIndices[, method], 
            total = (list.data$N - list.data$p)
    )
    
    df.vecm[list.regimeIndices[[1]], ]
    
}
