library(tsDyn)
library(vars)

m1 <- log(MEI$M1IDX_EA)-log(MEI$M1IDX_US)
m3 <- log(MEI$M3IDX_EA)-log(MEI$M3IDX_US)
y <- log(MEI$GDPIDX_US)-log(MEI$GDPIDX_EA)
irlt <- MEI$IRLT_EA-MEI$IRLT_US
e <- log(MEI$CCUS)

df.macro <- data.frame(e, m1, y, irlt)
longRunResiduals <- as.numeric(summary(lm(e~., data=df.macro))$residuals)

rm(m1,m3,y,irlt,e)


#old, probably useless, can't remember properly 
de <- diff(e)

# ecm 
m.ecm <- summary(lm(diff(e)[2:length(diff(e))]~1+modelLongRunResiduals[2:length(modelLongRunResiduals)]+diff(e,lag = 2)))

