#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
EURUSD.daily <- EURUSD.daily["2004/2013-04"]
index(EURUSD.daily) <- as.Date(index(EURUSD.daily),tz=tzone(EURUSD.daily))

#compute weekend gap
augmented.daily <- na.trim(merge(EURUSD.daily,with(EURUSD.daily,lag(merge(High,Low,Close),1))))
weekend <- with(augmented.daily,log(Open/Close.1))[wday(index(augmented.daily))==2]
weekend <- merge(weekend,abs(weekend)*sqrt(pi/2))
index(weekend) <- as.Date(index(weekend),tz=tzone(weekend))
names(weekend) <- c("Return","Abs.Return")

c(mean(weekend[,2]),sd(weekend[,2]))
plot.zoo(weekend[,1])
plot.zoo(cumsum(weekend[,1]))
plot(weekend[,2])
acf(coredata(weekend[,2]),lag.max=60)

#compute supp/resist gap
gap.hilo <- with(augmented.daily,log(merge(Open/High.1,Open/Low.1)))[wday(index(augmented.daily))==2]
plot(gap.hilo[,2])
names(gap.hilo) <- c("High","Low")

plot(gap.hilo[weekend[,1]<0,2])

#compute Monday returns and vol
ret <- ComputeCCYReturns2(EURUSD.daily)["2004/2013-04"]
monday <- with(augmented.daily,log(Close/Open))[wday(index(augmented.daily))==2]
monday <- merge(monday,abs(monday)*sqrt(pi/2))
monday <- merge(monday,ret[wday(index(ret))==2,"Parkinson"])
names(monday) <- c("Return","Abs.Return","Parkinson")
index(monday) <- as.Date(index(monday),tz=tzone(monday))

rbind(apply(monday,2,mean),
      apply(monday,2,sd))
acf(coredata(monday[,2]),lag.max=60)
acf(coredata(monday[,3]),lag.max=60)

#compare weekend vol to monday vol
compare <- na.trim(merge(weekend[,2],monday[,2:3]))
names(compare) <- c("weekend","monday.1","monday.2")

#is weekend vol predictive of monday vol?
cor(compare)[1,3]
plot.default(compare[,c(1,3)])

#is weekend return predictive of monday return?
predictive <- na.trim(merge(weekend[,1],monday[,1]))
cor(predictive)[1,2]
plot.default(predictive)

#simple strat: fade the gap until the close of the day
#no position sizing, no trade filtering, no exit filtering
strat.ret <- -sign(predictive[,1])*predictive[,2]
mean(strat.ret)*sqrt(52)/sd(strat.ret)
mean(-monday[,1])*sqrt(52)/sd(monday[,1])
plot.zoo(cumsum(na.trim(merge(strat.ret,-monday[,1]))),
         plot.type="single",xlab="",ylab="",col=1:2)
plot(cumsum(residuals(lm(strat.ret~monday[,1]))))
#Sharpe is not significantly better than just shorting every Monday

#what about return potential?
EURUSD.monday <- EURUSD.daily[wday(index(EURUSD.daily))==2]
monday.maxret <- with(EURUSD.monday,log(merge(High/Open,Open/Low)))
strat.signal <- -sign(predictive[,1])
strat.signal[strat.signal==-1] <- 2
aa <- merge(monday.maxret,strat.signal)
strat.maxperf <- do.call("rbind",lapply(1:2,function(s) aa[aa[,3]==s,s]))
plot.zoo(strat.maxperf)
plot.zoo(cumsum(strat.maxperf))
#not very informative

#first go at trade filtering based expected return conditional on on size of jump
dim(predictive)
weekend.cut <- cut.default(predictive[,1],breaks=quantile(predictive[,1],seq(0,1,0.1)))
monday.cut.mean <- tapply(predictive[,2],weekend.cut,median)
monday.cut.sd <- tapply(predictive[,2],weekend.cut,function(X) IQR(X)/(1.34*sqrt(length(X))))
plot(monday.cut.mean,type="b")
lines(monday.cut.mean+3*monday.cut.sd,lty=2)
lines(monday.cut.mean-3*monday.cut.sd,lty=2)
abline(h=0)
grid()
#only first and last deciles are significantly more profitable
#than others: a resulting strategy would only take 10 trades a year!
#average profit of 25bp per trade
levs <- levels(weekend.cut)
filtered.strat <- rbind(predictive[weekend.cut==levs[1],2],
                  -predictive[weekend.cut==levs[10],2])
plot.zoo(filtered.strat)
plot.zoo(cumsum(filtered.strat))
strat.period <- as.numeric(end(filtered.strat)-start(filtered.strat))/260
(sum(filtered.strat)/strat.period)
median(filtered.strat[filtered.strat<0])
head(sort(coredata(filtered.strat)),10)
hist(filtered.strat)
#Has an annualised return of 2% and a worst 1-day return of 230bp
#Win pct of 65% and profit factor of about 1.4
#Worst return is -230bp 
#but this is overstated...
#because INTRADAY vol and drawdown is going to be proportional to size of the jump too
#Now compute drawdown of strat
dim(monday.maxret)
head(monday.maxret)
head(predictive)
filtered.dd <- -rbind(monday.maxret[weekend.cut==levs[1],2], #long posn
      monday.maxret[weekend.cut==levs[10],1])                #short posn
plot.zoo(filtered.dd)
mean(filtered.dd)
median(filtered.dd)
hist(filtered.dd)
min(filtered.dd)
(sum(filtered.strat)/strat.period)/-min(filtered.dd)
#max dd is -250bp, correpsonding to the worst 1 day loss
#annual return to max dd ratio of 0.81:
#takes on avg over a year to crawl back from worst drawdown

#relationship between drawdowns and returns
plot.default(merge(filtered.dd,filtered.strat),
             xlab="Drawdown",ylab="Return")
abline(h=0)
#suggests that:
# there is a relationship between drawdown and return
# - after a certain amount of loss it may not be worth
# holding out til the end of the day --> introduce a stop loss
# for example at about 1-1.5%
# the stop loss should be relative to some kind of vol forecast

filtered.drawup <- rbind(monday.maxret[weekend.cut==levs[1],1], #long posn
                          monday.maxret[weekend.cut==levs[10],2])                #short posn
plot.zoo(filtered.drawup)
mean(filtered.drawup)
median(filtered.drawup)
hist(filtered.drawup)
plot.default(merge(filtered.drawup,filtered.strat),
             xlab="Drawup",ylab="Return")
abline(h=0)
abline(a=0,b=1)
#This shows that trades that ultimately lose rarely
#get up more than 50bp through the day


#use vol forecasts in two ways:
#1) improve trade selection: size of standardised gap
#2) forecast intraday risk and use it to size positions - think about a posn sizing
# strategy - either constant vol target / max dd target / or take bigger 
# bets with higher quality trades? how would that work though?
#2a) check if gap size predicts risk over and above previous forecast
vol.forecast <- exp(smooth.vol.forecast(log(ret$Garman.Klass)))
monday.vol.forecast <- vol.forecast[wday(index(vol.forecast))==2,]
std.gap <- weekend[2:nrow(weekend),1]/monday.vol.forecast
weekend.cut <- cut.default(std.gap,breaks=quantile(std.gap,seq(0,1,0.1)))
levs <- levels(weekend.cut)
mon.for.strat.2 <- monday[2:nrow(monday),1]
monday.cut.mean <- tapply(mon.for.strat.2,weekend.cut,median)
monday.cut.sd <- tapply(mon.for.strat.2,weekend.cut,function(X) IQR(X)/(1.34*sqrt(length(X))))
plot(monday.cut.mean,type="b",xaxt="n")
axis(1,c(2,9),levs[c(2,9)])
lines(monday.cut.mean+2*monday.cut.sd,lty=2)
lines(monday.cut.mean-2*monday.cut.sd,lty=2)
abline(h=0)
grid()
#on standardised returns, profitability is also sig
#for second largest decile - so trading 3 out of every 10 weeks
#or 15 times a year on average

#Try a kernel smoother
std.gap.winzd <- std.gap
winz.thresh <- 3*IQR(std.gap)/1.34
std.gap.winzd[abs(std.gap) > winz.thresh] <- winz.thresh*sign(std.gap)[abs(std.gap) > winz.thresh]
lo.gap <- loess(mon.for.strat.2~std.gap.winzd)
gap.grid <- seq(-1,1,length.out=100)
lo.pred <- predict(lo.gap,gap.grid,se=TRUE)
plot(coredata(merge(std.gap.winzd,mon.for.strat.2)),pch="+",
     xlim=c(-1,1),ylim=0.02*c(-1,1))
lines(gap.grid,lo.pred$fit,type="l",col="red")
lines(gap.grid,lo.pred$fit+2*lo.pred$se.fit,lty=2,col="red")
lines(gap.grid,lo.pred$fit-2*lo.pred$se.fit,lty=2,col="red")
abline(h=0); grid()

#+/-0.25 sigma seems like a good rough fit for a tradeable signal
buy.signal <- std.gap < -0.25
sell.signal <- std.gap > 0.25

filtered.strat.2 <- rbind(mon.for.strat.2[buy.signal,1],
                        -mon.for.strat.2[sell.signal,1])
plot.zoo(filtered.strat.2)
plot.zoo(cumsum(filtered.strat.2))
strat.period <- as.numeric(end(filtered.strat.2)-start(filtered.strat.2))/260
(sum(filtered.strat.2)/strat.period)
median(filtered.strat.2[filtered.strat.2<0])
head(sort(coredata(filtered.strat.2)),10)
hist(filtered.strat.2)
#Signal is now generated from standardised gaps and
#The equity curve looks way better now
#A 25% increase in annual return
#There is no longer a negative outlier return
maxret.2 <- monday.maxret[2:nrow(monday.maxret),]
filtered.dd <- -rbind(maxret.2[buy.signal,2], #long posn
                      maxret.2[sell.signal,1])                #short posn
#filtered.dd <- filtered.dd/monday.vol.forecast
plot.zoo(filtered.dd)
mean(filtered.dd)
median(filtered.dd)
hist(filtered.dd)
min(filtered.dd)
(sum(filtered.strat)/strat.period)/-min(filtered.dd)


#see how good the risk forecast and standardised jump size is for: 
#Monday vol
#Monday drawdown
all.dd <- rbind(maxret.2[std.gap>0,2],
                maxret.2[std.gap<=0,1])
risk.measures <- merge(monday.vol.forecast,abs(std.gap),monday[2:nrow(monday),3],all.dd)
risk.measures <- risk.measures[index(filtered.strat.2)]
names(risk.measures) <- c("Forecast","Jump","Parkinson","Drawdown")
cor(risk.measures)

lm.vol <- lm(Parkinson~Forecast+Jump,data=risk.measures)
summary(lm.vol)
lm.dd <- lm(Drawdown~Forecast+Jump,data=risk.measures)
summary(lm.dd)

plot.default(risk.measures[,c(1,4)])
coefs <- summary(lm.dd)$coefficients
abline(a=coefs[1,1],b=coefs[2,1],col="red")
abline(a=coefs[1,1],b=coefs[2,1]+2*coefs[2,2],col="red",lty=2)
abline(a=coefs[1,1],b=coefs[2,1]-2*coefs[2,2],col="red",lty=2)

plot.default(risk.measures[,c(2,4)])
abline(a=coefs[1,1],b=coefs[3,1],col="red")
abline(a=coefs[1,1],b=coefs[3,1]+2*coefs[3,2],col="red",lty=2)
abline(a=coefs[1,1],b=coefs[2,1]-2*coefs[2,2],col="red",lty=2)

#They are both important, vol forecast more so
#Drawdowns are harder to forecast than vol on the day
#A problem is that the jump vol is not log distributed like the GK vol estimate
#Could combine the jump into Friday's vol estimate before smoothing
#Also the drawdown has non-standard distribution-it is basically the dist of the half-range
#Next step: build a forecast model to forecast the risk for each potential trade


#Position sizing strategy:
#a) vol targeting, 
#Monday vol is mostly less than 1 x vol forecast + 0.003 x stdised jump
#R-squared of this regression is about 40%
#b) max dd targeting
#DD is mostly less than 1.15 x vol forecast + 0.0035 x stdised jump
#Bearing in mind the R-squared of this regression is 20%

#Can try re-estimating this for each day of the week:
#Max intra-day drawdown given a vol forecast

#Compute distribution of drawdown for GBM observed every 10 seconds
grid.size <- 24*60*10
maxes <- sapply(1:5000,function(s) max(cumsum(c(rnorm(grid.size,0,sqrt(1/grid.size))))))
hist(maxes,50); quantile(maxes,c(0.95,0.99))
#Theoretically, for GBM, 95% of max intra-day drawdowns are less than 2 sigma

#plot relative contributions of 
plot.zoo(with(risk.measures,merge(Jump*0.0035,Forecast*1.15)),
         plot.type="single",col=1:2)

#Position size to avoid drawdowns greater than a target
#Use parameters from drawdown forecast model
dd.forecast <- with(risk.measures,Forecast*1.15+Jump*0.0035+2*0.0040)
dd.target <- 0.025
posn.size <- dd.target/dd.forecast
plot.zoo(posn.size,type="p",pch="+")
grid()
filtered.strat.3 <- filtered.strat.2*posn.size
plot(filtered.strat.3)
plot.zoo(cumsum(filtered.strat.3),type="s")
(sum(filtered.strat.3)/strat.period)
median(filtered.strat.3[filtered.strat.2<0])
head(sort(coredata(filtered.strat.3)),10)
hist(filtered.strat.3,20)


#now check for: stop loss exit strategy
filtered.dd.3 <- filtered.dd*posn.size
plot.zoo(filtered.dd.3)
mean(filtered.dd.3)
median(filtered.dd.3)
hist(filtered.dd.3,20)
min(filtered.dd.3)
(sum(filtered.strat.3)/strat.period)/-min(filtered.dd.3)

#stop-loss exit calibration
#dd.vs.ret <- merge(filtered.dd.3,filtered.strat.3)
dd.vs.ret <- apply(merge(filtered.dd.3,filtered.strat.3),
                   2,"/",as.vector(dd.forecast))
lm.ddvsret <- lm(Return~Open,data=data.frame(dd.vs.ret))

#op <- par(pty="s")
plot.default(dd.vs.ret,
             xlab="DD")
abline(h=0); abline(a=0,b=1)
abline(coef(lm.ddvsret),col="red")
grid()
#par(op)

std.dd <- filtered.dd/dd.forecast
stoploss.candidates <- seq(min(std.dd),-0.1,length.out=100)#sort(coredata(std.dd[std.dd<mean(std.dd)]))
stoploss.sharpe <- function(s) {
  rets <- filtered.strat.2
  rets[std.dd<=s] <- s*dd.forecast[std.dd<=s]
  return(sum(rets[rets>0])/sum(-rets[rets<0]))
}
plot(stoploss.candidates,sapply(stoploss.candidates,stoploss.sharpe),
     type="s",ylab="")
#A stop loss of 1x dd forecast seems adequate
#It guards against very large adverse moves
#even though we haven't seen any in this sample

filtered.strat.4 <- filtered.strat.3
filtered.strat.4[filtered.dd.3< -dd.target] <- dd.forecast[filtered.dd.3< -dd.target]
plot.zoo(cumsum(filtered.strat.4),type="s")

#limit order entry strategy
#check how far the price moves in the adverse direction
#on those days we get a signal
#and how this compares against returns

plot.default(merge(filtered.strat.2,filtered.dd))
abline(v=0)
abline(h=0)

#doesn't look worth it as most of the winning trdes
#have very small drawdowns

#profit lock check
filtered.drawup.2 <- rbind(maxret.2[buy.signal,1], #long posn
                         maxret.2[sell.signal,2])                #short posn
plot.zoo(filtered.drawup.2)
mean(filtered.drawup.2)
median(filtered.drawup.2)
hist(filtered.drawup.2)

plot.default(apply(merge(filtered.drawup.2,filtered.strat.2),
                   2,"/",as.vector(dd.forecast)),
             xlab="Drawup",ylab="Return")
abline(h=0)
abline(a=0,b=1)
grid()


#Real question about implementing stop loss and take profits is:
#do prices revert or carry on after reaching their 
#intraday extremes?
#if drawdowns carry on, it might be putting in a stop loss
#if drawups revert, it might be worth putting in a profit take
#This is a rather more complex question than the one about 
#limiting strategy drawdowns...

std.du <- filtered.drawup.2/dd.forecast
profit.candidates <- seq(0.1,max(std.du),length.out=100)#sort(coredata(std.dd[std.dd<mean(std.dd)]))
profit.take.sharpe <- function(s) {
  rets <- filtered.strat.2
  rets[std.du>=s] <- s*dd.forecast[std.du>=s]
  return(sum(rets[rets>0])/sum(-rets[rets<0]))
}
plot(profit.candidates,sapply(profit.candidates,profit.take.sharpe),
     type="s",ylab="")
#doesn't appear to be a big advantage in profit taking either

#more advanced option:
#jump size conditional stop loss or profit takes - 
#see whether there is reversion for certain gap sizes
# need a measure of reversion to compare against gap sizes
plot.default(abs(std.gap)[index(filtered.strat.2)],filtered.strat.2-filtered.dd,
             xlab="Std gap signal",ylab="Reversion from trough")
plot.default(abs(std.gap)[index(filtered.strat.2)],filtered.drawup.2-filtered.strat.2,
             xlab="Std gap signal",ylab="Reversion from peak")


#for jumps over 1 DD forecast, the gap doesn't normally
#close by the end of the day
#for gaps less than 1.5 DD forecast it can more than close


# final strategy feature:
# add information about previous support and reistance levels
# and check if this