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

# UR Analyse fuer PYR aus AWM
acf(PYR)
acf(PYR, plot=FALSE)
pacf(PYR)
pacf(PYR, plot=FALSE)

# ADF
summary(ur.df(PYR, type="drift", lags=4))

# ADF fuer 1st diff
summary(ur.df(diff(PYR, differences = 1), type="drift", lags=4))

# ADF fuer 2nd diff
summary(ur.df(diff(PYR, differences = 2), type="drift", lags=4))


mj.multiplot(list.JP$df.scatterIncreasing, name = "Increasing", ve.points = c(90, 309), new = TRUE, from = 7, to = 10)
ve.bestRegime <- getThresholds(ve.errorJP, list.JP$df.scatterIncreasing[, 1], c(45, 256), 2, 105)
system.time(ve.bestRegime <- getThresholds(ve.errorJP, list.JP$df.scatterIncreasing[, 1], c(45, 256), 2, 105, RSquared = FALSE))


mj.plot(list.JP1$df.scatterIncreasing[, 2])
mj.lines(list.JP2$df.scatterIncreasing[, 2])
mj.lines(list.JP3$df.scatterIncreasing[, 2])
mj.lines(list.JP4$df.scatterIncreasing[, 2])



mj.plot(ve.series, col=0)
mj.lines(c(rep(NA, 61), df.scatterIncreasing[, 1]))
mj.lines(c(df.scatterDecreasing[, 1], rep(NA, 61)))

mj.plot(seq(0.9, 1, by = 1/4210), col = 0)
mj.lines(c(rep(NA, 61), df.scatterIncreasing[, 2]))
mj.lines(c(df.scatterDecreasing[, 2], rep(NA, 61)))



# generate list, plot all results
list.JP <- testLinearity(ve.errorJP)
mj.plotList(list.JP, column = 2, ylim = c(0.91, 0.975), name = "RSquared") # RSquared
mj.plotList(list.JP, column = 3, ylim = c(7, 28), name = "t-statistics for y_t-1")       # t-Statistic d = 1
mj.plotList(list.JP, column = 4, ylim = c(-6, -1), name = "t-statistics for y_t-2")        # t-Statistic d = 2
mj.plotList(list.JP, column = 5, ylim = c(0.5, 3), name = "t-statistics for y_t-3")        # t-Statistic d = 3
mj.plotList(list.JP, column = 6, ylim = c(-2.6, 0.3), name = "t-statistics for y_t-4")     # t-Statistic d = 4
list.JP.Thresh <- getThresholds(list.JP, ve.indices = c(145, 290), intervalSize = 10)
rect(xleft = 290-132, xright = 290+132, ytop = 30, ybottom = 0, col = "#88FF8811")
abline(v = 354,col = "cornflowerblue")
abline(v = 290-132,col = "darkgreen")
abline(v = 290+132,col = "darkgreen")


abs(head(df.cartesian[, 1]) - head(df.cartesian[, 2])) > m
#next:
df.cartesianRestricted <- df.cartesian[abs(df.cartesian[, 1] - df.cartesian[, 2]) > m, ]