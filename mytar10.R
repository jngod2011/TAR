# V6: 
# - return list object now
# - make functions more modular, i.e. less cross-interactions
# - automatically return BOTH decreasing and increasing data for scatterplots
# - removed TSAY method
#
# V7: 
# - remove getSumOuterProducts()
# - added R^2 output to scatterplot 
# - add getThresholds()
#
# V8:
# - reverse threshold lag vector and ve.RSquared if decreasing = TRUE,
#   so that indices will always increase with increasing values of the threshold lag vector.
#   this facilitates the grid search done in Thresholds.R
#
# V9:
# - removed minor index nuisance that was a necessary implementation feature for the TSAY algorithm in getScatter()
#
# V10: 
# - FStats for descending order also (no real changes expected anyways) 
#
#
# p .. lag order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes
# m .. regime starting size
# N .. original sample size
# n .. sample size after losing observations due to AR terms
# see 1998 Martens et al, Appendix Step 2
#


testLinearity <- function(ve.error, p = -1, ve.S = -1, m = -1, constant = FALSE, 
    stationary = TRUE, verbose = FALSE) { 
  
  ve.y <- as.numeric(ve.error)
  N <- as.numeric(length(ve.y))
  
  # auto select lag order p
  if (p == -1) { 
    p <- getOptimalLagOrder(ve.y, verbose)
  }
  
  # restrict threshold order to a maximum value of the AR order
  if (min(ve.S) == -1) {
    ve.S <- seq(from = 1, to = p)
  } else if (max(ve.S) > p) {
    ve.S <- seq(from = 1, to = p)
    cat("S restricted to max value p = ", p, sep = "")
  }
  
  # generate AR dataframe of order p
  df.y <- getAR(ve.y, p)
  
  # get optimal regime size
  if (m == -1) m <- getRegimeSize(df.y, stationary, verbose)
  
  # calculate F-test statistic 
  # will contain 2*p values for ascending and descending sorting
  ve.FStats <- getFStats(df.y = df.y, ve.S = ve.S, p = p, m = m, verbose = verbose)
  
  # get threshold variable with highest F-statistic
  dMax <- ve.S[which(ve.FStats == max(ve.FStats), arr.ind=TRUE)[1]]
  F <- max(ve.FStats[dMax, ])
  df.ordered <- df.y[order(df.y[, (dMax + 1)]), ]
  ve.thresholdLag <- df.ordered[, (dMax + 1)]
  
  # get RSquared and t-statistics, estimates for each coefficient according to the threshold variables    
  list.scatterAll <- NULL
  
  for (i in 1:length(ve.S)) {
    list.scatter <- NULL
    df.scatterIncreasing <- getScatter(df.y, d = ve.S[i], p, constant, stationary, decreasing = FALSE, m)
    df.scatterDecreasing <- getScatter(df.y, d = ve.S[i], p, constant, stationary, decreasing = TRUE, m)
    list.scatterAll <- c(list.scatterAll, list(df.scatterIncreasing, df.scatterDecreasing))
    names(list.scatterAll)[2 * i - 1] <- paste("df.scatterIncreasing", i, sep = "")
    names(list.scatterAll)[2 * i] <- paste("df.scatterDecreasing", i, sep = "")
  }
  
  
  
  return(list(p = p, dMax = dMax, N = N, m = m, n = N - m - p + 1, F = F, ve.thresholdLag = ve.thresholdLag,  
          ve.error = ve.error, ve.FStats = ve.FStats, 
          list.scatterAll = list.scatterAll))
}



# calculate F statistics according to different threshold lags
getFStats <- function(df.y, ve.S, p, m, verbose = FALSE) { 
  
  n <- as.numeric(nrow(df.y))
  ve.FStatsAscending <- NULL
  ve.FStatsDescending <- NULL
  ve.FStats <- NULL
  
  for (i in 1:length(ve.S)) {
    d <- ve.S[i]
    FStatAscending <- NULL
    FStatDescending <- NULL
    ve.predictiveResidualsAscending <- NULL
    ve.predictiveResidualsDescending <- NULL
    #ve.eta <- NULL
    h <- max(1, p + 1 - d)
    df.ascending <- df.y[order(df.y[, (d + 1)]), ] # order ascendingly by threshold variable z_(t-d)
    df.descending <- df.y[order(df.y[, (d + 1)], decreasing = TRUE), ] # order descendingly by threshold variable z_(t-d)
    
    # recursively calculate predictive residuals (Martens et al)
    for (i in m:n) {
      df.regimeAscending <- df.ascending[1:i, ]
      df.regimeDescending <- df.descending[1:i, ]
      ve.predictiveResidualsAscending <- c(ve.predictiveResidualsAscending, summary(lm(ve.y ~ ., data = df.regimeAscending))$residuals[i])
      ve.predictiveResidualsDescending <- c(ve.predictiveResidualsDescending, summary(lm(ve.y ~ ., data = df.regimeDescending))$residuals[i])
    }
    
    
    # regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
    # hence we would have a linear model. if there is explanatory power, we prefer a TAR model
    # now the regression is predictiveResiduals~df.y[60:n, ], first m - 1 obs. are left out 
    df.testAscending <- data.frame(ve.predictiveResidualsAscending, df.ascending[m:n, 2:(ncol(df.ascending))])
    ve.estimatedResidualsAscending <- summary(lm(ve.predictiveResidualsAscending ~ ., data = df.testAscending))$residuals
    df.testDescending <- data.frame(ve.predictiveResidualsDescending, df.descending[m:n, 2:(ncol(df.descending))])
    ve.estimatedResidualsDescending <- summary(lm(ve.predictiveResidualsDescending ~ ., data = df.testDescending))$residuals
    
    # calculate final test statistic
    # mj.plot(ve.predictiveResiduals, name = "predictiveResiduals")
    
    FStatAscending <- ((sum(ve.predictiveResidualsAscending^2) - sum(ve.estimatedResidualsAscending^2)) / (p + 1)) / 
        (sum(ve.estimatedResidualsAscending^2) / (n - d - m - p))
    FStatDescending <- ((sum(ve.predictiveResidualsDescending^2) - sum(ve.estimatedResidualsDescending^2)) / (p + 1)) / 
        (sum(ve.estimatedResidualsDescending^2) / (n - d - m - p))
    
    if (verbose) {
      cat("F-statistic for s = ", d, ": ", FStat, "\n", sep = "")
    }
    
    ve.FStatsAscending <- c(ve.FStatsAscending, FStatAscending)
    ve.FStatsDescending <- c(ve.FStatsDescending, FStatDescending)
  }
  ve.FStats <- cbind(ve.FStatsAscending, ve.FStatsDescending)
  
  return(ve.FStats)
}


# calculate optimal lag order based on AIC, SC
getOptimalLagOrder <- function(ve.series, verbose = FALSE) {
  # p as optimal order by SC: usually the more parsimonious model
  #p <- as.numeric(VARselect(ve.series)$selection[3])
  # p as optimal order by AIC
  p <- as.numeric(VARselect(ve.series)$selection[1])
  
  if (verbose) {
    cat("Optimal lag order: ", p, "\n", sep = "")
  }
  
  return(p)
}


# calculate m
getRegimeSize <- function(df, stationary = TRUE, verbose = FALSE) {
  if (stationary) regimeSize <- round(3 * sqrt(nrow(df)), 0)
  else regimeSize <- round(5 * sqrt(nrow(df)), 0)
  
  if (verbose) {
    string1 <- as.character(paste("\n"))
    string2 <- as.character(paste("Stationary: suggested regime size m =", round(3 * sqrt(nrow(df)), 0), "\n"))
    string3 <- as.character(paste("Unit Root:  suggested regime size m =", round(5 * sqrt(nrow(df)), 0), "\n"))
    cat(string1, string2, string3, string1, sep = "")
  }
  
  return(regimeSize)
}


# calculate dataframe with t-Statistics for the predictive residuals to draw in a scatterplot against z_(t-d)
getScatter <- function(df.y, d, p, constant, stationary, decreasing, m) {
  df.z <- df.y[order(df.y[, (d + 1)], decreasing = decreasing), ]
  n <- as.numeric(nrow(df.z))
  ve.predictiveResiduals <- NULL
  df.tStats <- data.frame(NULL)
  df.estimates <- data.frame(NULL)
  ve.RSquared <- NULL
  
  if (constant) {
    for (i in m:n) {
      df.regime <- df.z[1:i, ]
      lm.regime <- lm(ve.y ~ ., data = df.regime)
      ve.tStats <- summary(lm.regime)$coefficients[, 3]
      df.tStats <- rbind.data.frame(df.tStats, ve.tStats)
      ve.estimates <- summary(lm.regime)$coefficients[, 1]
      df.estimates <- rbind.data.frame(df.estimates, ve.estimates)
      ve.RSquared <- c(ve.RSquared, summary(lm.regime)$r.squared)
    }
    names(ve.tStats)[1] <- "ve.y.0"            # rename (Intercept) to ve.y.0 like the other coefficients
    names(ve.estimates)[1] <- "ve.y.0" 
  } else if (!constant) {
    for (i in m:n) {
      df.regime <- df.z[1:i, ]
      lm.regime <- lm(ve.y ~ .-1, data = df.regime)
      ve.tStats <- summary(lm.regime)$coefficients[, 3]
      df.tStats <- rbind.data.frame(df.tStats, ve.tStats)
      ve.estimates <- summary(lm.regime)$coefficients[, 1]
      df.estimates <- rbind.data.frame(df.estimates, ve.estimates)
      ve.RSquared <- c(ve.RSquared, summary(lm.regime)$r.squared)
    }
  }
  
  names(df.tStats) <- names(ve.tStats)
  names(df.tStats) <- gsub("y", "t", names(df.tStats)) # replace y with t in this vector of names for the t-stats
  names(df.estimates) <- names(ve.estimates)
  names(df.estimates) <- gsub("y", "e", names(df.estimates)) # replace y with e in this vector of names for the estimates
  ve.thresholdLag <- df.z[m:n, (d + 1)]
  
  df.scatter <- cbind.data.frame(ve.thresholdLag, ve.RSquared, df.tStats, df.estimates) # combine threshold variable and dataframes
  
  if (decreasing) {
    # ve.NA <- rep(NA, ncol(df.scatter))
    # for (i in 1:(m - 1)) df.scatter <-  rbind(ve.NA, df.scatter)
    
  } else if (!decreasing) {
    # ve.NA <- rep(NA, ncol(df.scatter))
    # for (i in 1:(m - 1)) df.scatter <- rbind(ve.NA, df.scatter)
  }
  
  return(df.scatter)
}

