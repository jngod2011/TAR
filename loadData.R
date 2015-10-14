# load data

m1 <- log(df.MEI_EA_US$M1IDX_EA)-log(df.MEI_EA_US$M1IDX_US)
m3 <- log(df.MEI_EA_US$M3IDX_EA)-log(df.MEI_EA_US$M3IDX_US)
y <- log(df.MEI_EA_US$GDPIDX_US)-log(df.MEI_EA_US$GDPIDX_EA)
irlt <- df.MEI_EA_US$IRLT_EA-df.MEI_EA_US$IRLT_US
e <- log(1/df.MEI_EA_US$CCUS)

df.macro <- data.frame(e, m1, y, irlt)
ve.longRunResiduals <- as.numeric(summary(lm(e~., data=df.macro))$residuals)

rm(m1,m3,y,irlt,e)

