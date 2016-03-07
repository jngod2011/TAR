# TODO: Add comment
# 
# Author: Michi
###############################################################################


getAnalysis <- function(df.data, k, h) {
  list.analysis <- NULL    
  list.analysis <- foreach (i = 1:h) %do% {
    getPredictions(df.data, k = k, h = i)
  }
  
  return(list.analysis)
}
