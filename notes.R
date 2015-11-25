# remove objects with a similar name
rm(list = ls(pattern = "^mj"))

# vector products
a %*% b     # inner: returns scalar
a %o% b     # outer: returns nxn matrix

# modulo operator
45%%9       # for remainder
45%/%9      # for quotient

######## UR Analyse
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


mj.multiplot(list.JP$df.scatterIncreasing, name = "Increasing", ve.points = c(90, 309), new = TRUE, from = 7, to = 10)
ve.bestRegime <- getThresholds(ve.errorJP, list.JP$df.scatterIncreasing[, 1], c(45, 256), 2, 105)
system.time(ve.bestRegime <- getThresholds(ve.errorJP, list.JP$df.scatterIncreasing[, 1], c(45, 256), 2, 105, RSquared = FALSE))

