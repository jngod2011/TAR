# TODO: Add comment
# 
# Author: Michi
###############################################################################

getPredictions <- function (df.data, ratio = 0.75, n.ahead = 1) {
    # step 1: split data in in sample/out of sample parts
    a <- getInSampleSize(df.data, ratio = ratio)
    N <- nrow(df.data)
    df.inSample <- df.data[1:a, ]
    # TAR analysis is performed with the in sample error correction term from the original macro model
    # note that TAR analysis is only performed ONCE, with the first in sample data 
    # this is to keep the amount of computations feasible. also the TAR regimes are not expected to change much
    # by adding new observations
    ve.error <- summary(lm(s ~ ., data = df.inSample))$residuals
    list.testLinearity <- testLinearity(ve.error)
    list.thresholds <- getThresholds(list.testLinearity)
    p <- list.thresholds$p
    dMax <- list.thresholds$dMax
    k <- list.thresholds$k
    
    
    ######################## Predictions & Regime splitting #############################
    # value of the exogenous variable (the error correction term) at time t.
    # a lag of it later enters the VAR/VECM to calculate the predictions 
    # ve.r are the actual thresholds as described in the literature, including -Inf and Inf
    ve.r <- c(-Inf, list.thresholds$list.thresholds$df.thresholds$SSR, Inf)
    ve.r <- ve.r[order(ve.r)]
    df.exoPred <- getDumvarPredictions(df.data, inSample = a, dMax = dMax, n.ahead = n.ahead) 
    df.predictions <- NULL
    
    for (i in a:(N - 1)) {
        df.inSample <- NULL
        ve.error <- NULL
                
        df.inSample <- df.data[1:i, ]
        ve.error <- summary(lm(s ~ ., data = df.inSample))$residuals
        
        ve.threshDMax <- getAR(ve.error, p = dMax)[, dMax + 1]
        df.vecmFull <- data.frame(df.data[(p + 1):i, ], ve.threshDMax)
        # determine in which regime to go:
        currentRegime <- as.numeric(table(tail(ve.threshDMax, 1) > ve.r)["TRUE"])
        
        # next: in regime einteilen, vecms schaetzen, prediction                 
        list.regimes <- NULL
        for (j in 1:k) {
            list.regimes[[j]] <- which(df.vecmFull[, "ve.threshDMax"] > ve.r[j] & 
                                       df.vecmFull[, "ve.threshDMax"] <= ve.r[j + 1])
        }
        
        # lagOrder <- VARselect(df.vecmFull[list.regimes[[currentRegime]], ])$selection[1]
        lagOrder <- 2
        mod.VECM <- VECM(df.vecmFull[list.regimes[[currentRegime]], -ncol(df.vecmFull) ], lag = lagOrder,
                exogen = df.vecmFull[list.regimes[[currentRegime]], "ve.threshDMax"])
        
        df.predictions <- rbind.data.frame(df.predictions, 
                predict(mod.VECM, n.ahead = 1, exoPred = matrix(df.exoPred[(i - a + 1), 1])))        
    }
        
    df.eval <- cbind.data.frame(tail(df.data[, "s"], nrow(df.predictions)), df.predictions[, "s"])
    colnames(df.eval)[2] <- "predictions"
    RMSE <- sqrt(mean((df.eval[, 1] - df.eval[, 2]) * (df.eval[, 1] - df.eval[, 2])))
}


getInSampleSize <- function (df.data, ratio) {
    if (ratio > 1 || ratio < 0) stop("\nInvalid ratio\n")
    return(ceiling(nrow(df.data) * ratio))
}

# predictions for exogenous variable in VAR/VECM
getDumvarPredictions <- function (df.data, inSample, dMax, n.ahead) {
    df.exogen <- NULL
    # prediction for threshold lag with d = dMax at time t + n.ahead, calculated at time t, which corresponds to
    # threshold lag d = dMax + n.ahead at time t.
    for (i in (inSample):nrow(df.data)) {
        df.exogen <- c(df.exogen, summary(lm(s ~ ., data = df.data[1:i, ]))$residuals[i - dMax + n.ahead])
    }
    return(as.data.frame(df.exogen))
}
