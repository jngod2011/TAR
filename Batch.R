# TODO: Add comment
# 
# Author: Michi
###############################################################################

getAnalysis <- function(df.data, k, n) {
  list.analysis <- NULL
  
  list.analysis <- foreach (i = 1:n) %do% {
    getPredictions(df.data, k = k, h = i)    
  }
  return(list.analysis)  
}

formatTables <- function(list.data) {
  df.table <- list.data[[1]]$df.results
  for (i in 2:length(list.data)) {
    df.table <- rbind.data.frame(df.table, list.data[[i]]$df.results)
  }  
  df.table1 <- df.table[, 1:8]
  df.table2 <- df.table[, 9:16]
  return(list(df.table1 = df.table1, df.table2 = df.table2))
}