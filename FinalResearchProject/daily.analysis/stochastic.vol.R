#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
ret <- ComputeCCYReturns2(EURUSD.daily)

acf(coredata(ret$Abs.Return),400)
acf(coredata(ret$True.Range),400)

Box.raw <- apply(coredata(ret),2,Box.test,60)
sapply(Box.raw,"[[","statistic")
#very strong autocorrelation: stronger in range than in abs return

#now standardise data using 30-minute realized volatility
GetCCYData("EURUSD")
rv <- realized.volatility(EURUSD["2004/"],15)

index(ret) <- as.Date(index(ret),tz=tzone(ret))
index(rv) <- as.Date(index(rv),tz=tzone(rv))

std.abs.ret <- standardise(ret$Abs.Return,rv$Rlzd.Vol)
acf(coredata(abs(std.abs.ret)),400)

std.true.range <- standardise(ret$True.Range,rv$Rlzd.Vol)
acf(coredata(abs(std.true.range)),400)

Box.std <- apply(coredata(merge(std.abs.ret,std.true.range)),2,Box.test,60)
sapply(Box.std,"[[","statistic")

#now there is a significant negative autocorrelation at lag 1
#and in the standardised range a few borderline significant correlations in the first 50 

#returns becomes normal after standardising
qqnorm(ret$Return); qqline(ret$Return)
std.ret <- standardise(ret$Return,rv$Rlzd.Vol)
qqnorm(std.ret); qqline(std.ret)

#log ranges are normal even before standardising
qqnorm(log(ret$True.Range)); qqline(log(ret$True.Range))
qqnorm(log(std.true.range)); qqline(log(std.true.range))

#what does the volatility process look like?
plot.zoo(rv$Rlzd.Vol)
acf(coredata(rv$Rlzd.Vol),400)
#strong clustering
#very strong autocorrelation structure
#try fitting an autoregressive model to it
try.lags <- 15
ar.aic <- sapply(1:try.lags,function(l) AIC(arima(rv$Rlzd.Vol,c(l,0,0))))
plot(1:try.lags,ar.aic,type="b")

ar.rv <- arima(rv$Rlzd.Vol,c(5,0,0))
tsdiag(ar.rv)
acf(ar.rv$residuals,60)
fitted.rv <- xts(coredata(rv$Rlzd.Vol)+ar.rv$residuals,
                 index(rv))
plot.default(merge(rv$Rlzd.Vol,fitted.rv))
abline(a=0,b=1)
qqnorm(ar.rv$residuals)
qqline(ar.rv$residuals)
#the fit is not quite right!
#a very high P is required to capture the dependence structure
#even then the residuals are no completely uncorrelated
#there also appears to be heterosked in the residuals
#also the fitted values are conditinally biased: at low values they are too low, 
#at high values they are too high
coef(ar.rv)

histogram(coredata(rv$Rlzd.Vol),nint=30)
qqnorm(log(rv$Rlzd.Vol))
qqline(log(rv$Rlzd.Vol))
#close to log-normally distributed
