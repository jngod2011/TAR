# TODO: Add comment
# 
# Author: Michi
###############################################################################

getAnalysis <- function(df.data, p = -1, k = 3, n = 12, method = "SSR") {
  list.analysis <- NULL  
  list.analysis <- foreach (i = 1:n) %do% {
    getPredictions(df.data, p = p, k = k, h = i, method = method)
  }  
  for(i in 1:n) names(list.analysis)[i] <- paste("pred.h", i, sep = "")  
  return(list.analysis)
}

formatTablesLinear <- function(list.data) {
  df.table <- list.data[[1]]$df.results
  for (i in 2:length(list.data)) {
    df.table <- rbind.data.frame(df.table, list.data[[i]]$df.results)
  }
  df.all <- data.frame(
      df.table[, "RMSE.VECM.norm"],
      df.table[, "RMSE.RWD.norm"],
      df.table[, "MAE.VECM.norm"],
      df.table[, "MAE.RWD.norm"],
      df.table[, "DA.VECM"],
      df.table[, "DA.RWD"],
      df.table[, "DV.VECM"],
      df.table[, "DV.RWD"],
      df.table[, "trade.VECM.pa"],
      df.table[, "trade.RWD.pa"]     
  )
  colnames(df.all) <- c(
      "VECM", "RWD",   
      "VECM", "RWD",
      "VECM", "RWD", 
      "VECM", "RWD",
      "VECM", "RWD" 
  )
  
  return(df.all)
}

formatTablesNonlinear <- function(list.dataAIC, list.dataSSR) {
  df.tableAIC <- list.dataAIC[[1]]$df.results
  df.tableSSR <- list.dataSSR[[1]]$df.results
  
  for (i in 2:length(list.dataAIC)) {
    df.tableAIC <- rbind.data.frame(df.tableAIC, list.dataAIC[[i]]$df.results)
    df.tableSSR <- rbind.data.frame(df.tableSSR, list.dataSSR[[i]]$df.results)
  }
  df.all <- data.frame(
      df.tableAIC[, "RMSE.TVECM"] / df.tableAIC[, "RMSE.RWD"],  # normalize by according RWD 
      df.tableSSR[, "RMSE.TVECM"] / df.tableSSR[, "RMSE.RWD"],
      df.tableAIC[, "RMSE.RWD"] / df.tableAIC[, "RMSE.RWD"],    # just to get a 1 here
      df.tableAIC[, "MAE.TVECM"] / df.tableAIC[, "MAE.RWD"],  # normalize by according RWD 
      df.tableSSR[, "MAE.TVECM"] / df.tableSSR[, "MAE.RWD"],
      df.tableAIC[, "MAE.RWD"] / df.tableAIC[, "MAE.RWD"],    # just to get a 1 here
      df.tableAIC[, "DA.TVECM"],                              # directional accuracy columns 
      df.tableSSR[, "DA.TVECM"],
      df.tableAIC[, "DA.RWD"],
      df.tableAIC[, "DV.TVECM"],                              # directional value columns
      df.tableSSR[, "DV.TVECM"],
      df.tableAIC[, "DV.RWD"],
      df.tableAIC[, "trade.TVECM.pa"],                        # annualized trade columns 
      df.tableSSR[, "trade.TVECM.pa"],
      df.tableAIC[, "trade.RWD.pa"]
  )
  colnames(df.all) <- c(
      "TVECM AIC", "TVECM SSR", "RWD",   
      "TVECM AIC", "TVECM SSR", "RWD",
      "TVECM AIC", "TVECM SSR", "RWD", 
      "TVECM AIC", "TVECM SSR", "RWD",
      "TVECM AIC", "TVECM SSR", "RWD" 
      )
      
  df.table1 <- df.all[,1:15]

  return(df.table1)
}


