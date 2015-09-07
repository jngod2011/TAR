m1 <- log(MEI$M1IDX_EA)-log(MEI$M1IDX_US)
m3 <- log(MEI$M3IDX_EA)-log(MEI$M3IDX_US)
y <- log(MEI$GDPIDX_US)-log(MEI$GDPIDX_EA)
irlt <- MEI$IRLT_EA-MEI$IRLT_US
e <- log(MEI$CCUS)

m.lr <- summary(lm(e~m1+y+irlt))
m.lr.resid <- as.numeric(lm1$residuals[2:length(lm1$residuals)])
de <- diff(e)

# ecm ??????? nicht sicher ob richtig
m.ecm <- summary(lm(diff(e)[2:length(diff(e))]~1+m.lr.resid[2:length(m.lr.resid)]+diff(e,lag = 2)))

