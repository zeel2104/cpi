#rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
#Read 100 items
df<- read.csv("C:/Users/desai/Downloads/cpi.csv")
#df
#gdpts <- ts(df$Goa,start=min(df$Year),end=max(df$Year),frequency = 1)
#tfseries <- ts(df,start=c(1813))
tfseries <- ts(df$Goa,start=min(df$Year),end=max(df$Year),frequency = 1)
plot.ts(tfseries)
#rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
tfseriesforecasts <- HoltWinters(tfseries, beta=FALSE, gamma=FALSE)
tfseriesforecasts
tfseriesforecasts$fitted
plot(tfseriesforecasts)
tfseriesforecasts$SSE
HoltWinters(tfseries, beta=FALSE, gamma=FALSE, l.start=23.56)
library(forecast)

tfseriesforecasts2 <- forecast:::forecast.HoltWinters(tfseriesforecasts, h=2)
tfseriesforecasts2
plot(tfseriesforecasts2)
acf(tfseriesforecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(tfseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(tfseriesforecasts2$residuals)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(tfseriesforecasts2$residuals)
