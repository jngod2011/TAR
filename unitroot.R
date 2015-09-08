> lm(prod[2:84]~prod[1:83])

Call:
  lm(formula = prod[2:84] ~ prod[1:83])

Coefficients:
  (Intercept)   prod[1:83]  
-5.920        1.015  

# manuell DF mit drift
lm(diff(prod)[2:length(diff(prod))]~prod[1:(length(prod)-2)]+diff(prod)[1:length(diff(prod))-1])

# gleich wie
summary(ur.df(prod, type="drift", lags=1))



# UR Analyse für PYR aus AWM
acf(PYR)
acf(PYR, plot=FALSE)
pacf(PYR)
pacf(PYR, plot=FALSE)

# ADF
summary(ur.df(PYR, type="drift", lags=4))

# ADF für 1st diff
summary(ur.df(diff(PYR, differences = 1), type="drift", lags=4))

# ADF für 2nd diff
summary(ur.df(diff(PYR, differences = 2), type="drift", lags=4))