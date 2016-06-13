#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
ret <- ComputeCCYReturns2(EURUSD.daily)["2004/2013-04"]
head(ret)

require(vrtest)
kv <- 2:10
Lo.Mac(ret$Return,kv)
Boot.test(coredata(ret$Return),kvec=kv,nboot=50,wild="Normal")
VR.plot(ret$Return,kvec=kv)

#try adjusting returns for heteroskedasticity first
GetCCYData("EURUSD")
rv <- realized.volatility(EURUSD["2004/2013-04"],30)
index(rv) <- as.Date(index(rv),tz="Europe/London")
index(ret) <- as.Date(index(ret),tz="Europe/London")
ret.std <- standardise(ret$Return,rv$Rlzd.Vol)
Lo.Mac(ret.std,kv)
VR.plot(ret.std,kv)

#try testing for constant drift
plot.zoo(cumsum(ret.std))
1-pnorm(mean(ret.std)/(sd(ret.std)/sqrt(length(ret.std))))

#Ljung-Box
Box.test(ret.std,60,"Ljung-Box")

#constant drift mined from the chart
mean(ret.std["2006/2008"])
1.96*sd(ret.std["2006/2008"])/sqrt(length(ret.std["2006/2008"]))

#try fitting an arima model
arima.std <- arima(ret.std,order=c(1,0,1))

print(arima.std)

#Try testing for the variability in rolling means
plot.zoo(cumsum(ret$Return))
plot.zoo(cumsum(ret.std))
#visual evidence of trends in heterosked adjusted returns

require(TTR)

roll.length <- seq(10,200,10)
rollmean.sd <- sapply(roll.length,function(l) sd(EMA(ret.std,n=l)[-(1:200)]))

#Compute bootstrapped thresholds and p-vals
sims <- 1000
sim.sd <- list(length(roll.length))
for (l in 1:length(roll.length)) {
  sim.sd[[l]] <- sapply(1:sims,function(s) 
    sd(EMA(sample(coredata(ret.std),length(ret.std)),n=roll.length[l])[-(1:200)]))
  print(roll.length[l])
}

sim.pval <- sapply(1:length(roll.length), function(l) mean(sim.sd[[l]]>rollmean.sd[l]))
plot(roll.length,sim.pval,type="b")
abline(h=0.05,col="red")

plot(roll.length,rollmean.sd,ylim=c(0,0.08),
     xlab="rolling window (days)",ylab="standard deviation")
lines(roll.length,sapply(sim.sd,quantile,0.95),lty=2,col="red")
lines(roll.length,sapply(sim.sd,quantile,0.5),lty=1,col="red")
grid()

#Some evidence of time-varying non zero drift

#try fitting a local level model to the returns
ret.lev <- StructTS(ret.std,type="level")
tsdiag(ret.lev)
ret.lev$coef
ret.fitted <- as.xts(fitted(ret.lev))
index(ret.fitted) <- index(ret.std)
plot.zoo(ret.std["2005/"])
plot.zoo(ret.fitted["2005/"])
abline(h=0)
#this doesn't appear to work well!
#the local trends are very minor...

#try fitting an arima model to the returns
acf(ret.std,lag.max=400)
ret.arima <- arima(ret.std,order=c(0,1,1))
coef(ret.arima)
print(ret.arima)
tsdiag(ret.arima)
ret.arima.fitted <- xts(coredata(ret.std)-ret.arima$residuals,
                        index(ret.std))
plot.zoo(ret.arima.fitted)
abline(h=0)
plot.zoo(cumsum(ret.std))
