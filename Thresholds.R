# Everything threshold related
# 
# Author: michael
###############################################################################

getCandidates <- function(index, range) {
    return(ceiling(seq(from = index - (range / 2), to = index + (range / 2))))
}



getThresholds <- function(data, candidates) {
        
    if (length(candidates) == 0) {
        return(results)
        
    } else {
        
        
        getThresholds(candidates[-i])
        i <- i - 1
        
    }
    
    
}

# calculate thresholds
getThresholds <- function(ve.series, ve.candidates, p = 0, d = 0, range = 0, ve.thresholdLag, ve.RSquared) {
    if (range == 0) range = 2 * round(log(length(ve.series)), 0)  # range is a logarithmic function of the length of the series
    if (p == 0) p <- getOptimalLagOrder(ve.series)
    df.thresholds <- data.frame(ve.thresholdLag, ve.RSquared)
    
    for (i in 1:length(ve.candidates)) {
        
        candidate <- ve.candidates[i]
        ve.candidates <- 
                
                for (j in (candidate - (range / 2)):(candidate + (range / 2))) {
                    
                }
        
        
    }    
}







