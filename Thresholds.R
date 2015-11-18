# Everything threshold related
# 
# Author: michael
###############################################################################

getCandidates <- function(candidate, range = 10) {
    return(ceiling(seq(from = candidate - (range / 2), to = candidate + (range / 2))))
}



getThresholds <- function(ve.series, ve.thresholdLag, ve.candidates, d) {
    
    df.candidates <- foreach(i = 1:length(ve.candidates), .combine = cbind.data.frame) %do% {
        getCandidates(ve.candidates[i])        
    }
    
    df.cartesian <- expand.grid(df.candidates)
    
    df.AR <- getAR(ve.series, p = getOptimalLagOrder(ve.series, verbose = FALSE))
    
    
}








