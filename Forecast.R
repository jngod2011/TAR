# all about predictions
# 
# Author: Michi
###############################################################################

getPredictions <- function (df.data, ratio = 0.75, n.ahead = 1, Crit = 2.32, k = 3) {
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
    ve.r <- ve.r[order(ve.r)]
    df.exoPred <- getDumvarPredictions(df.data, inSample = a, dMax = dMax, n.ahead = n.ahead) 
    df.predictionsTVECM <- NULL
    df.predictionsRW <- NULL
    df.predictionsRWD <- NULL  
    df.eval <- NULL
    
    # TVECM predictions until element N
    for (i in a:(N - n.ahead)) {
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
      
      df.predictionsTVECM <- rbind.data.frame(df.predictionsTVECM,
          predict(mod.VECM, n.ahead = 1, exoPred = matrix(df.exoPred[(i - a + 1), 1])))
      
      # RW /wo drift predictions until element N
      df.predictionsRW <- rbind.data.frame(df.predictionsRW,
          rwf(df.inSample[, 1], h = n.ahead, drift = FALSE)$mean[1])
      
      # RW /w drift predictions until element N
      df.predictionsRWD <- rbind.data.frame(df.predictionsRWD,
          rwf(df.inSample[, 1], h = n.ahead, drift = TRUE)$mean[1])
    }
    
    df.eval <- data.frame(tail(df.data[, "s"], nrow(df.predictionsTVECM)), 
        df.predictionsTVECM[, "s"], 
        df.predictionsRW, 
        df.predictionsRWD)
    colnames(df.eval) <- c("s", "TVECM", "RW", "RWD")
    
    RMSE.TVECM <- getRMSE(df.eval[,"s"], df.eval[, "TVECM"])
    RMSE.RW <- getRMSE(df.eval[,"s"], df.eval[, "RW"])
    RMSE.RWD <- getRMSE(df.eval[,"s"], df.eval[, "RWD"])
    RMSE.TVECM.norm <- 1
    RMSE.RW.norm <- RMSE.RW/RMSE.TVECM
    RMSE.RWD.norm <- RMSE.RWD/RMSE.TVECM
    
    MAE.TVECM <- getMAE(df.eval[,"s"], df.eval[, "TVECM"])
    MAE.RW <- getMAE(df.eval[,"s"], df.eval[, "RW"])
    MAE.RWD <- getMAE(df.eval[,"s"], df.eval[, "RWD"])
    MAE.TVECM.norm <- 1
    MAE.RW.norm <- MAE.RW/MAE.TVECM
    MAE.RWD.norm <- MAE.RWD/MAE.TVECM
    
    DA.TVECM <- getDA(df.eval[,"s"], df.eval[, "TVECM"])
    DA.RW <- getDA(df.eval[,"s"], df.eval[, "RW"])
    DA.RWD <- getDA(df.eval[,"s"], df.eval[, "RWD"])
    
    trade.TVECM <- getTrade(df.eval[,"s"], df.eval[, "TVECM"])
    trade.RW <- getTrade(df.eval[,"s"], df.eval[, "RW"])
    trade.RWD <- getTrade(df.eval[,"s"], df.eval[, "RWD"])
    
    df.results <- data.frame(RMSE.TVECM.norm, RMSE.RW.norm, RMSE.RWD.norm,  MAE.TVECM.norm, MAE.RW.norm, MAE.RWD.norm, DA.TVECM, DA.RW, DA.RWD, 
        trade.TVECM, trade.RW, trade.RWD)
  } else if (!nonlinear) {
    df.predictionsVECM <- NULL
    df.predictionsRW <- NULL
    df.predictionsRWD <- NULL
    df.eval <- NULL
    
    for (i in a:(N - n.ahead)) {
      
      #df.vecmFull <- data.frame(df.data[(p + 1):i, ])
      df.inSample <- NULL
      df.inSample <- df.data[1:i, ]
      
      # lagOrder <- VARselect(df.vecmFull[list.regimes[[currentRegime]], ])$selection[1]
      lagOrder <- dMax + 1
      mod.VECM <- VECM(df.inSample, lag = lagOrder)
      df.predictionsVECM <- rbind.data.frame(df.predictionsVECM, predict(mod.VECM, n.ahead = n.ahead))
      
      # RW /wo drift predictions until element N
      df.predictionsRW <- rbind.data.frame(df.predictionsRW, 
          rwf(df.inSample[, 1], h = n.ahead, drift = FALSE)$mean[1])
      
      # RW /w drift predictions until element N
      df.predictionsRWD <- rbind.data.frame(df.predictionsRWD, 
          rwf(df.inSample[, 1], h = n.ahead, drift = TRUE)$mean[1])     
    }
    
    df.eval <- data.frame(tail(df.data[, "s"], nrow(df.predictionsVECM)), 
        df.predictionsVECM[, "s"], 
        df.predictionsRW, 
        df.predictionsRWD)
    colnames(df.eval) <- c("s", "VECM", "RW", "RWD")
    
    RMSE.VECM <- getRMSE(df.eval[,"s"], df.eval[, "VECM"])
    RMSE.RW <- getRMSE(df.eval[,"s"], df.eval[, "RW"])
    RMSE.RWD <- getRMSE(df.eval[,"s"], df.eval[, "RWD"])
    RMSE.VECM.norm <- 1
    RMSE.RW.norm <- RMSE.RW/RMSE.VECM
    RMSE.RWD.norm <- RMSE.RWD/RMSE.VECM
    
    MAE.VECM <- getMAE(df.eval[,"s"], df.eval[, "VECM"])
    MAE.RW <- getMAE(df.eval[,"s"], df.eval[, "RW"])
    MAE.RWD <- getMAE(df.eval[,"s"], df.eval[, "RWD"])
    MAE.VECM.norm <- 1
    MAE.RW.norm <- MAE.RW/MAE.VECM
    MAE.RWD.norm <- MAE.RWD/MAE.VECM
    
    DA.VECM <- getDA(df.eval[,"s"], df.eval[, "VECM"])
    DA.RW <- getDA(df.eval[,"s"], df.eval[, "RW"])
    DA.RWD <- getDA(df.eval[,"s"], df.eval[, "RWD"])
    
    trade.VECM <- getTrade(df.eval[,"s"], df.eval[, "VECM"])
    trade.RW <- getTrade(df.eval[,"s"], df.eval[, "RW"])
    trade.RWD <- getTrade(df.eval[,"s"], df.eval[, "RWD"])
    
    df.results <- data.frame(RMSE.VECM.norm, RMSE.RW.norm, RMSE.RWD.norm, MAE.VECM.norm, MAE.RW.norm, MAE.RWD.norm, DA.VECM, DA.RW, DA.RWD, 
        trade.VECM, trade.RW, trade.RWD) 
  }
  
  return(list(p = p, dMax = dMax, F = F, df.results = df.results))    
}

# trade return
getTrade <- function(ve.actual, ve.prediction) {
  ve.signal <- NULL
  ve.long <- NULL
  ve.short <- NULL
  for (i in 2:length(ve.actual)) {
    ve.long <- c(ve.long, (ve.prediction[i] > ve.actual[i - 1]) * 1)
    ve.short <- c(ve.short, (ve.prediction[i] < ve.actual[i - 1]) * 1)
  }
  # signal says if in period t, based on forecast t+h, position should be long (+1) or short (-1)
  ve.signal <- ve.long - ve.short
  # ve.actual is in logs already, so the difference gives the %change
  tradeReturn <- sum(ve.signal * diff((ve.actual))) 
  
  return(tradeReturn)
}

# directional value
getDV <- function(ve.actual, ve.prediction) {
  ve.actualDV <- NULL
  ve.predictionDV <- NULL
  
  for (i in 2:length(ve.actual)) {
    ve.predictionDV <- c(ve.predictionDV, ve.prediction[i] > ve.actual[i - 1])
    ve.actualDV <- c(ve.actualDV, ve.actual[i] > ve.actual[i - 1])
  }
  
  ve.correct <- (ve.predictionDOC == ve.actualDOC) * 1
  DV <- sum(ve.correct * abs(diff(ve.actual)))
  
  return(as.numeric(DV))
}

# direction of change correct percentage
getDA <- function(ve.actual, ve.prediction) {
  ve.actualDA <- NULL
  ve.predictionDA <- NULL
  
  for (i in 2:length(ve.actual)) {
    ve.predictionDA <- c(ve.predictionDA, ve.prediction[i] > ve.actual[i - 1])
    ve.actualDA <- c(ve.actualDA, ve.actual[i] > ve.actual[i - 1])
  }
  # length(ve.actual) - 1 because one observation is lost in the process - for loop goes from 2:length() 
  DA <- table(ve.actualDA == ve.predictionDA)["TRUE"] / (length(ve.actual) - 1)
  
  return(as.numeric(DA))
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
getDumvarPredictions <- function (df.data, inSample, dMax, n.ahead) {
  df.exogen <- NULL
  # prediction for threshold lag with d = dMax at time t + n.ahead, calculated at time t, which corresponds to
  # threshold lag d = dMax + n.ahead at time t.
  for (i in (inSample):nrow(df.data)) {
    df.exogen <- c(df.exogen, summary(lm(s ~ ., data = df.data[1:i, ]))$residuals[i - dMax + n.ahead])
  }
  return(as.data.frame(df.exogen))
}
