# tsayExercise
library(tseries)
data(ice.river)

y1 <- flow.jok[1:(length(flow.jok)-1)]
y2 <- flow.vat[1:(length(flow.vat)-1)]
x <- prec[2:length(prec)]
z <- temp[1:(length(temp)-1)] # temperature
