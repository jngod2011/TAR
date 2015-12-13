# TODO: Add comment
# 
# Author: Michi
###############################################################################


getForecast <- function (list.data, method = "SSR") {
    
    list.regimes <- getRegimeIndices(list.data$list.thresholds$df.thresholdIndices[, "SSR"], 422)
    list.lm <- foreach (i = 1:length(list.regimes)) %do% {
        lm(ve.y ~ ., data = list.data$df.ordered)
    }
}
