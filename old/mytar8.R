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
# p .. lag order 
# d .. particular threshold lag
# S .. max threshold lag
# k .. number of regimes
# m .. regime starting size
# N .. original sample size
# n .. sample size after losing observations due to AR terms
# see 1998 Martens et al, Appendix Step 2

testLinearity <- function(ve.series, p = -1, S = -1, m = -1, constant = FALSE, stationary = TRUE, verbose = FALSE) { 
	
	ve.y <- as.numeric(ve.series)
	N <- as.numeric(length(ve.y))
	
	# auto select lag order p
	if (p == -1) { 
		p <- getOptimalLagOrder(ve.y, verbose)
    }
    
	# restrict threshold order to a maximum value of the AR order
	if (S == -1) {
		S <- p
	} else if (S > p) {
		S <- min(p, S)
		cat("S restricted to max value p = ", p, sep = "")
	}
	
	# generate AR dataframe of order p
	df.y <- getAR(ve.y, p)

    # get optimal regime size
    if (m == -1) m <- getRegimeSize(df.y, stationary, verbose)
        
	# calculate F-test statistic 
	ve.FStats <- getFStats(df.y, S, p, m, verbose)
	
	# get threshold variable with highest F-statistic
	dMax <- which.max(as.numeric(ve.FStats))
	
	# get t-statistics for each coefficient according to the threshold variable yielding the highest F-statistic
	df.scatterDecreasing <- getScatter(df.y, dMax, p, constant, stationary, decreasing = TRUE, m)
	df.scatterIncreasing <- getScatter(df.y, dMax, p, constant, stationary, decreasing = FALSE, m)
    
	return(list(ve.FStats = ve.FStats, p = p, d = dMax, df.scatterDecreasing = df.scatterDecreasing, 
                    df.scatterIncreasing = df.scatterIncreasing))
}



# calculate F statistics according to different threshold lags
getFStats <- function(df.y, S, p, m, verbose = FALSE) { 
	
	n <- as.numeric(nrow(df.y))
    ve.FStats <- NULL
	
    for (d in 1:S) {
        FStat <- NULL
        ve.predictiveResiduals <- NULL
        ve.eta <- NULL
        h <- max(1, p + 1 - d)
        df.z <- df.y[order(df.y[, (d + 1)] ), ] # order by threshold variable z_(t-d)
        
        # recursively calculate predictive residuals (Martens et al)
        for (i in m:n) {
            df.regime <- df.z[1:i, ]
            ve.predictiveResiduals <- c(ve.predictiveResiduals, summary(lm(ve.y ~ ., data = df.regime))$residuals[i])
        }
       
        # regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
        # hence we would have a linear model. if there is explanatory power, we prefer a TAR model
        # now the regression is predictiveResiduals~df.y[60:n, ], first m - 1 obs. are left out 
        df.test <- data.frame(ve.predictiveResiduals, df.z[m:n, 2:(ncol(df.z))])
        ve.estimatedResiduals <- summary(lm(ve.predictiveResiduals ~ ., data = df.test))$residuals
        
        # calculate final test statistic
        # mj.plot(ve.predictiveResiduals, name = "predictiveResiduals")
        
        FStat <- ((sum(ve.predictiveResiduals^2) - sum(ve.estimatedResiduals^2)) / (p + 1)) / 
                (sum(ve.estimatedResiduals^2) / (n - d - m - p))
        
        if (verbose) {
            cat("F-statistic ", d, ": ", FStat, "\n", sep = "")
        }
        
        ve.FStats <- c(ve.FStats, FStat)
    }
    
    return(ve.FStats)
}


# calculate optimal lag order based on AIC, SC
getOptimalLagOrder <- function(ve.series, verbose = FALSE) {
	p <- as.numeric(round(((VARselect(ve.series)$selection[1] + VARselect(ve.series)$selection[3]) / 2), digits = 0))
	
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
getScatter <- function(df.y, dMax, p, constant, stationary, decreasing, m) {
	df.z <- df.y[order(df.y[, (dMax + 1)], decreasing = decreasing), ]
	n <- as.numeric(nrow(df.z))
	ve.predictiveResiduals <- NULL
	df.tStats <- data.frame(NULL)
	df.estimates <- data.frame(NULL)
    ve.RSquared <- NULL
    
	if (constant) {
		for (i in (m - 1):(n - 1)) {
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
		for (i in (m - 1):(n - 1)) {
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
    ve.thresholdLag <- df.regime[-(1:(m - 2)), (dMax + 1)]
    
    
    if (decreasing) {
        ve.thresholdLag <- rev(ve.thresholdLag)
        ve.RSquared <- rev(ve.RSquared)
    }
    
    
    df.scatter <- cbind.data.frame(ve.thresholdLag, ve.RSquared, df.tStats, df.estimates) # combine threshold variable and dataframes
    	
    return(df.scatter)
}

