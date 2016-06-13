#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
EURUSD.daily <- EURUSD.daily["2004/2013-04"]
index(EURUSD.daily) <- as.Date(index(EURUSD.daily),tz=tzone(EURUSD.daily))

#Compute raw strategy returns
#In: daily prices
raw.strat.ret <- raw.gap.returns(EURUSD.daily)

#Evaluate
strat.perf.stats(raw.strat.ret)

#Compute vol forecasting parameter accuracy
#And compute E[DD] and Quantile[DD] as a function of post-jump vol forecast
#In: daily prices
risk.model <- select.vol.smoothing.param(EURUSD.daily)
summary(risk.model)
plot(risk.model)
summary(risk.model)$coefficients
#E[DD] = 0.88*sigma + 0.0032*std.jump
#Quantile[DD,0.95] = 1.16*sigma + 0.0043*std.jump + 2*0.0037
#Under GBM, Quantile[DD,0.95] ~ 2*sigma


#Compute E[R] as function of pre-jump vol stdised gap
#In: daily prices, vol smoothing parameter, EMA params
std.gap.response(EURUSD.daily,ema.param=c(10,200))
std.gap.response(EURUSD.daily,ema.param=c(20,100),signal="Jump")

#Compute filtered and leveraged strategy returns
#In: daily prices, vol smoothing param, jump thresholds, DD threshold
strat.returns <- filtered.strat.ret(EURUSD.daily,0.1,c(-0.25,0.25),0.025,risk.model)
strat.perf.stats(strat.returns[[1]])
