# p .. lag order 
# d .. particular threshold lag
# S .. max threshold lag
# k .. number of regimes
# m .. regime starting size
# N .. original sample size
# n .. sample size after losing observations due to AR terms
# see 1998 Martens et al, Appendix Step 2

testLinearity <- function(ve.series, p = 0, S = 0, k = 3, constant = FALSE, method = "MARTENS", stationary = TRUE, 
		decreasing = FALSE) { 
	
	ve.y <- as.numeric(ve.series)
	N <- as.numeric(length(ve.y))
	
	# auto select lag order p
	if (p == 0) { 
		p <- getOptimalLagOrder(ve.y)
	}
	
	# restrict threshold order to a maximum value of the AR order
	if (S == 0) {
		S <- p
	} else if (S > p) {
		S <- min(p, S)
		cat("S restricted to max value p = ", p, sep = "")
	}
	
	# generate AR dataframe of order p
	df.y <- getAR(ve.y, p)
	
	# calculate F-test statistic 
	ve.FStats <- data.frame(NULL)
	
	for (d in 1:S) {
		ve.FStats <- c(ve.FStats, getFStat(df.y, d, p, method = method, stationary = stationary))
	}
	
	# get threshold variable with highest F-statistic
	dMax <- which.max(as.numeric(ve.FStats))
	
	# get t-statistics for each coefficient according to the threshold variable yielding the highest F-statistic
	df.scatter <- getScatter(df.y, dMax, p, constant = constant, stationary = stationary, decreasing = decreasing)
	names(df.scatter)[1] <- paste("threshold z_(t-", dMax, ")", sep = "")
	
	return(list(ve.FStats = ve.FStats, df.scatter = df.scatter))
}


# calculate F statistics according to different threshold lags
getFStat <- function(df.y, d, p, method = "TSAY", stationary = TRUE) { 
	
	df.z <- df.y[order(df.y[, (d + 1)] ), ] # order by threshold variable z_(t-d)
	m <- getRegimeSize(df.z, stationary)
	n <- as.numeric(nrow(df.z))
	h <- max(1, p + 1 - d)
	ve.predictiveResiduals <- NULL
	ve.eta <- NULL
	
	# recursively calculate predictive residuals (Martens et al)
	if (method == "MARTENS") { 
		for (i in m:n) {
			df.regime <- df.z[1:i, ]
			ve.predictiveResiduals <- c(ve.predictiveResiduals, summary(lm(ve.y ~ ., data = df.regime))$residuals[i])
		}
	}
	
	# forecast residual of next period (Tsay)
	# decrease starting point for i by 1 to m-1 to make predictiveResiduals same length as in MARTENS method
	if (method == "TSAY") {
		for (i in (m - 1):(n - 1)) {
			df.regime <- df.z[1:i, ]
			lm.regime <- lm(ve.y ~ ., data = df.regime)
			predResid <- as.numeric(df.z[(i + 1), 1] - 
							lm.regime$coefficients[1] - 
							(lm.regime$coefficients[-1] %*% as.numeric(df.z[(i + 1), -1]))
			)
			
			ma.V <- matrix(data = 0, nrow = p, ncol = p)
			ma.V <- solve(getSumOuterProducts(df.regime[, -1]))
			normalizer <- as.numeric(sqrt(1 + as.numeric(df.z[(i + 1), -1]) %*% ma.V %*% as.numeric(df.z[(i + 1), -1])))
			ve.eta <- c(ve.eta, predResid / normalizer)
		}
		
		ve.predictiveResiduals <- ve.eta
	}
	
	# regress predictive residuals on AR terms, check coefficients. H0: no explanatory power of AR regressors
	# hence we would have a linear model. if there is explanatory power, we prefer a TAR model
	# now the regression is predictiveResiduals~df.y[60:n, ], first m - 1 obs. are left out 
	df.test <- data.frame(ve.predictiveResiduals, df.z[m:n, 2:(ncol(df.z))])
	ve.estimatedResiduals <- summary(lm(ve.predictiveResiduals ~ ., data = df.test))$residuals
	
	# calculate final test statistic
	# mj.plot(ve.predictiveResiduals, name = "predictiveResiduals")
	
	FStatMartens <- (
				(sum(ve.predictiveResiduals^2) - sum(ve.estimatedResiduals^2)) / (p + 1)) / 
			(sum(ve.estimatedResiduals^2) / (n - d - m - p)
				)
	SSR0 <- 1 / (n - h - m) * sum(ve.predictiveResiduals^2)
	SSR1 <- 1 / (n - h - m) * sum(ve.estimatedResiduals^2)
	FStatTsay <- (n - h - m - ((p + 1) * p) + 1) * (log(SSR0) - log(SSR1))
	
	if (method == "TSAY") FStat <- FStatTsay
	else FStat <- FStatMartens
	
	return(FStat)
}


# calculate optimal lag order based on AIC, SC
getOptimalLagOrder <- function(ve.series) {
	p <- as.numeric(round(((VARselect(ve.series)$selection[1] + VARselect(ve.series)$selection[3]) / 2), digits = 0))
	
	return(p)
}


# calculate m
getRegimeSize <- function(df, stationary = TRUE, verbose = FALSE) {
	if (stationary == TRUE) regimeSize <- round(3 * sqrt(nrow(df)), 0)
	else regimeSize <- round(5 * sqrt(nrow(df)), 0)
	
	if (verbose) {
		string1 <- as.character(paste("\n"))
		string2 <- as.character(paste("   Stationary: suggested regime size m =", round(3 * sqrt(length(series)), 0), "\n"))
		string3 <- as.character(paste("   Unit Root:  suggested regime size m =", round(5 * sqrt(length(series)), 0), "\n\n"))
		cat(string1, string2, string3)
	}
	
	return(regimeSize)
}


# Sum Outer Products
getSumOuterProducts <- function(data) {
	ma.V <- matrix(0, nrow = ncol(data), ncol = ncol(data))
	
	for(i in 1:nrow(data)) {
		ve.X <- as.numeric(data[i, ])
		ma.V <- ma.V + (ve.X %o% ve.X)
	}
	
	return(ma.V)
}


# calculate dataframe with t-Statistics for the predictive residuals to draw in a scatterplot against z_(t-d)
getScatter <- function(df.y, dMax, p, constant = constant, stationary = TRUE, decreasing = FALSE) {
	df.z <- df.y[order(df.y[, (dMax + 1)], decreasing = decreasing), ]
	m <- getRegimeSize(df.z, stationary)
	n <- as.numeric(nrow(df.z))
	ve.predictiveResiduals <- NULL
	df.tStats <- data.frame(NULL)
	df.estimates <- data.frame(NULL)
    
	if (constant == TRUE) {
		for (i in (m - 1):(n - 1)) {
			df.regime <- df.z[1:i, ]
			lm.regime <- lm(ve.y ~ ., data = df.regime)
			ve.tStats <- summary(lm.regime)$coefficients[, 3]
			df.tStats <- rbind.data.frame(df.tStats, ve.tStats)
            ve.estimates <- summary(lm.regime)$coefficients[, 1]
            df.estimates <- rbind.data.frame(df.estimates, ve.estimates)            
		}
        names(ve.tStats)[1] <- "ve.y.0"            # rename (Intercept) to ve.y.0 like the other coefficients
        names(ve.estimates)[1] <- "ve.y.0" 
	} else if (constant == FALSE) {
		for (i in (m - 1):(n - 1)) {
			df.regime <- df.z[1:i, ]
			lm.regime <- lm(ve.y ~ .-1, data = df.regime)
			ve.tStats <- summary(lm.regime)$coefficients[, 3]
			df.tStats <- rbind.data.frame(df.tStats, ve.tStats)
            ve.estimates <- summary(lm.regime)$coefficients[, 1]
            df.estimates <- rbind.data.frame(df.estimates, ve.estimates)
		}
	}
	
	names(df.tStats) <- names(ve.tStats)
	names(df.tStats) <- gsub("y", "t", names(df.tStats)) # replace y with t in this vector of names for the t-stats
    names(df.estimates) <- names(ve.estimates)
    names(df.estimates) <- gsub("y", "e", names(df.estimates)) # replace y with e in this vector of names for the estimates
	
    df.scatter <- cbind.data.frame(df.regime[-(1:(m - 2)), (dMax + 1)], df.tStats, df.estimates) # combine threshold variable and dataframes
    
	return(df.scatter)
}

