# p .. AR order 
# d .. particular threshold lag
# S .. set of threshold lags
# k .. number of regimes

# see 1998 Martens et al, Appendix Step 2
myNonLinearityTest <- function(series, p=1, d=1, k=3) {
  
  h <- max(1, p+1-d)
  z <- series[h:(length(series)-d)]
  index <- seq(1:length(z))
  
  df <- data.frame(z, index)
  
  
  return(df)
}