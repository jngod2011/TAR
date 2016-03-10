# all about predictions
# 
# Author: Michi
###############################################################################

getPredictions <- function (df.data, ratio = 0.75, Crit = 2.32, k = 3, h = 1, method = "SSR") {
  # step 1: split data in in sample/out of sample parts
  nonlinear <- FALSE
  a <- getInSampleSize(df.data, ratio = ratio)
  N <- nrow(df.data)
  df.inSample <- df.data[1:a, ]
  # TAR analysis is performed with the in sample error correction term from the original macro model
  # note that TAR analysis is only performed once, with the first set of in-sample data 
  # this is to keep the amount of computations feasible. also the TAR regimes are not expected to change much
  # by adding new observations
  # note also that F statistics here will differ from the ones gotten from the ve.error vectors in load.R
  # since the ones in load.R are from models with more observations (not split in in/out of sample)
  ve.error <- summary(lm(s ~ ., data = df.inSample))$residuals
  
  list.testLinearity <- testLinearity(ve.error)
  F <- list.testLinearity$F
  p <- list.testLinearity$p
  dMax <- list.testLinearity$dMax
  
  #k <- list.thresholds$k
  list.thresholds <- getThresholds(list.testLinearity, k = k)
  
  if(F > Crit) nonlinear <- TRUE else nonlinear <- FALSE
  
  if(nonlinear) {
    ######################## Predictions & Regime splitting #############################
    # value of the exogenous variable (the error correction term) at time t.
    # a lag of it later enters the VAR/VECM to calculate the predictions 
    # ve.r are the actual thresholds as described in the literature, including -Inf and Inf
    ve.r <- c(-Inf, list.thresholds$list.thresholds$df.thresholds$SSR, Inf)
    if (method == "AIC") ve.r <- c(-Inf, list.thresholds$list.thresholds$df.thresholds$AIC, Inf)
    
    ve.r <- ve.r[order(ve.r)]
    # predictions of exogenous variable needs to contain ALL PREDICTIONS if h > 1 as well. example:
    # consider a = 10 and h = 2. you also need the predictions for h = 1 in order to get the one for h = 2.
    # therefore the length of the df.exoPred vector does NOT correspond to the ultimate amount of predictions
    # we will get. 
    df.predictionsTVECM <- NULL
    df.predictionsRWD <- NULL
    df.eval <- NULL
    
    # TVECM predictions until element N
    for (i in a:(N - h)) {
      df.inSample <- NULL
      ve.error <- NULL
      
      df.inSample <- df.data[1:i, ]
      ve.error <- summary(lm(s ~ ., data = df.inSample))$residuals
      ve.threshDMax <- getAR(ve.error, p = p)[, dMax + 1]
      
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
      # a too large lag number (in the case for russia) can lead to too little observations for single regimes
      # therefore I just go with dMax + 1
      lagOrder <- dMax + 1
      mod.VECM <- VECM(df.vecmFull[list.regimes[[currentRegime]], -ncol(df.vecmFull) ], lag = lagOrder,
          exogen = df.vecmFull[list.regimes[[currentRegime]], "ve.threshDMax"])
      
      df.exoPred <- getDumvarPredictions(df.data, inSample = i, dMax = dMax, p = p, h = h)
      df.predictionsTVECM <- rbind.data.frame(df.predictionsTVECM,
          # predict(mod.VECM, n.ahead = h, exoPred = matrix(df.exoPred[(i - a + 1):(i - a + h), 1]))[h, ])
            predict(mod.VECM, n.ahead = h, exoPred = as.matrix(df.exoPred))[h, ])
    
      # RW /w drift predictions until element N
      df.predictionsRWD <- rbind.data.frame(df.predictionsRWD,
          rwf(df.inSample[, 1], h = h, drift = TRUE)$mean[h])
    }
    
    colnames(df.predictionsTVECM) <- colnames(df.data)
    df.eval <- data.frame(df.data[(a:(nrow(df.data) - h)), "s"],
            tail(df.data[, "s"], nrow(df.predictionsTVECM)),
            df.predictionsTVECM[, "s"], 
            df.predictionsRWD)
    colnames(df.eval) <- c("s_t", "s_t+h", "TVECM", "RWD")
    cat(nrow(df.eval), "\n")
        
    RMSE.TVECM <- getRMSE(df.eval[,"s_t+h"], df.eval[, "TVECM"])
    RMSE.RWD <- getRMSE(df.eval[,"s_t+h"], df.eval[, "RWD"])
    RMSE.TVECM.norm <- 1
    RMSE.RWD.norm <- RMSE.RWD/RMSE.TVECM
    
    MAE.TVECM <- getMAE(df.eval[,"s_t+h"], df.eval[, "TVECM"])
    MAE.RWD <- getMAE(df.eval[,"s_t+h"], df.eval[, "RWD"])
    MAE.TVECM.norm <- 1
    MAE.RWD.norm <- MAE.RWD/MAE.TVECM
    
    DA.TVECM <- getDA(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "TVECM"])
    DA.RWD <- getDA(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])
    
    DV.TVECM <- getDV(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "TVECM"])
    DV.RWD <- getDV(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])
    
    trade.TVECM <- getTrade(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "TVECM"])
    trade.RWD <- getTrade(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])
    
    # trade return per annum
    trade.TVECM.pa <- (trade.TVECM * 12) / (nrow(df.eval) * h)
    trade.RWD.pa <- (trade.RWD * 12) / (nrow(df.eval) * h)
    
    df.results <- data.frame(RMSE.TVECM, RMSE.RWD, RMSE.TVECM.norm, RMSE.RWD.norm, MAE.TVECM, MAE.RWD, 
        MAE.TVECM.norm, MAE.RWD.norm, DA.TVECM, DA.RWD, DV.TVECM, DV.RWD, 
        trade.TVECM, trade.RWD, trade.TVECM.pa, trade.RWD.pa)
  } else if (!nonlinear) {
    df.predictionsVECM <- NULL
    df.predictionsRWD <- NULL
    df.eval <- NULL
    
    for (i in a:(N - h)) {      
      df.inSample <- NULL
      df.inSample <- df.data[1:i, ]
      
      lagOrder <- dMax + 1
      mod.VECM <- VECM(df.inSample, lag = lagOrder)
      df.predictionsVECM <- rbind.data.frame(df.predictionsVECM, predict(mod.VECM, n.ahead = h)[h, ])
      
      # RW /w drift predictions until element N
      df.predictionsRWD <- rbind.data.frame(df.predictionsRWD, 
          rwf(df.inSample[, 1], h = h, drift = TRUE)$mean[h])     
    }
    
    colnames(df.predictionsVECM) <- colnames(df.data)
    df.eval <- data.frame(df.data[(a:(nrow(df.data) - h)), "s"],
        tail(df.data[, "s"], nrow(df.predictionsVECM)),
        df.predictionsVECM[, "s"], 
        df.predictionsRWD)
    colnames(df.eval) <- c("s_t", "s_t+h", "VECM", "RWD")
    
    RMSE.VECM <- getRMSE( df.eval[, "s_t+h"], df.eval[, "VECM"])
    RMSE.RWD <- getRMSE(df.eval[, "s_t+h"], df.eval[, "RWD"])
    RMSE.VECM.norm <- 1
    RMSE.RWD.norm <- RMSE.RWD/RMSE.VECM
    
    MAE.VECM <- getMAE(df.eval[, "s_t+h"], df.eval[, "VECM"])
    MAE.RWD <- getMAE(df.eval[, "s_t+h"], df.eval[, "RWD"])
    MAE.VECM.norm <- 1
    MAE.RWD.norm <- MAE.RWD/MAE.VECM
    
    DA.VECM <- getDA(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "VECM"])
    DA.RWD <- getDA(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])

    DV.VECM <- getDV(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "VECM"])
    DV.RWD <- getDV(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])
    
    trade.VECM <- getTrade(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "VECM"])
    trade.RWD <- getTrade(df.eval[, "s_t"], df.eval[, "s_t+h"], df.eval[, "RWD"])
    
    # trade return per annum
    trade.VECM.pa <- (trade.VECM * 12) / (nrow(df.eval) * h)
    trade.RWD.pa <- (trade.RWD * 12) / (nrow(df.eval) * h)
    
    df.results <- data.frame(RMSE.VECM, RMSE.RWD, RMSE.VECM.norm, RMSE.RWD.norm, MAE.VECM, MAE.RWD, 
        MAE.VECM.norm, MAE.RWD.norm, DA.VECM, DA.RWD, DV.VECM, DV.RWD, 
        trade.VECM, trade.RWD, trade.VECM.pa, trade.RWD.pa) 
  }
  
  return(list(p = p, dMax = dMax, F = F, df.results = df.results))
}


# direction of change correct percentage
getDA <- function(ve.origin, ve.actual, ve.prediction) {
  ve.actualDirection <- NULL
  ve.predictionDirection <- NULL
  
  for (i in 1:length(ve.actual)) {
    ve.actualDirection <- c(ve.actualDirection, ve.actual[i] > ve.origin[i])
    ve.predictionDirection <- c(ve.predictionDirection, ve.prediction[i] > ve.origin[i])
  }
  
  ve.true <- (ve.actualDirection == ve.predictionDirection) * 1
  DA <- mean(ve.true)
  #DA <- table(ve.true)["TRUE"] / length(ve.true)
  
  return(as.numeric(DA))
}


# directional value
getDV <- function(ve.origin, ve.actual, ve.prediction) {
  ve.actualDirection <- NULL
  ve.predictionDirection <- NULL
  
  for (i in 1:length(ve.actual)) {
    ve.actualDirection <- c(ve.actualDirection, ve.actual[i] > ve.origin[i])
    ve.predictionDirection <- c(ve.predictionDirection, ve.prediction[i] > ve.origin[i])
  }
  
  ve.true <- (ve.predictionDirection == ve.actualDirection) * 1
  DV <- sum(ve.true * abs(ve.origin - ve.actual))
  
  return(as.numeric(DV))
}


# trade return: predict the correct direction: add to total return. predict it wrong, subtract
getTrade <- function(ve.origin, ve.actual, ve.prediction) {
  ve.actualDirection <- NULL
  ve.predictionDirection <- NULL
  
  for (i in 1:length(ve.actual)) {
    ve.actualDirection <- c(ve.actualDirection, ve.actual[i] > ve.origin[i])
    ve.predictionDirection <- c(ve.predictionDirection, ve.prediction[i] > ve.origin[i])
  }
  
  # convert logical to binary with 1/-1
  ve.true <- ve.actualDirection == ve.predictionDirection
  ve.trueBin <- ve.true * 1
  ve.trueBin[ve.trueBin == 0] <- -1
  
  ve.change <- abs(ve.actual - ve.origin)
  ve.return <- sum(ve.trueBin * ve.change) 
  
  return(ve.return)
}


getMAE <- function(ve.actual, ve.prediction) {
  return(mean(abs(ve.actual - ve.prediction)))
}

getRMSE <- function(ve.actual, ve.prediction) {
  return(sqrt(mean((ve.actual - ve.prediction) * (ve.actual - ve.prediction))))
}

getInSampleSize <- function (df.data, ratio) {
  if (ratio > 1 || ratio < 0) stop("\nInvalid ratio\n")
  return(ceiling(nrow(df.data) * ratio))
}

# predictions for exogenous variable in VAR/VECM
getDumvarPredictions <- function (df.data, inSample, dMax, p, h) {
  df.exoPred <- NULL
  if(h <= dMax) {
    # prediction for threshold lag with d = dMax at time t + h, calculated at time t, which corresponds to
    # threshold lag d = dMax + h at time t.
    ve.residuals <- summary(lm(s ~ ., data = df.data[1:inSample, ]))$residuals
    df.exoPred <- ve.residuals[(length(ve.residuals) - dMax + 1):(length(ve.residuals) - dMax + h)]
  } else {
    # dMax steps ahead "prediction" of z_{t-d}
    ve.residuals <- summary(lm(s ~ ., data = df.data[1:inSample, ]))$residuals
    df.exoPred <- ve.residuals[(length(ve.residuals) - dMax + 1):length(ve.residuals)]
    # need the remaining h - dMax steps ahead:
    # lm.ar <- ar(ve.residuals, aic = TRUE) # AR Forecast of remaining predictions
    # df.exoPred <- c(df.exoPred, predict(lm.ar, n.ahead = (h - dMax))$pred[h - dMax])
    df.exoPred <- c(df.exoPred, rwf(ve.residuals, h = (h - dMax), drift = TRUE)$mean)     
  }
  return(as.data.frame(df.exoPred))
}
