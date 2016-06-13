#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
EURUSD.daily <- EURUSD.daily["2004/2013-04"]

#compute weekend vol
augmented.daily <- na.trim(merge(EURUSD.daily,with(EURUSD.daily,lag(Close,1))))
weekend <- with(augmented.daily,log(Open/Close.1))[wday(index(augmented.daily))==2]
weekend <- merge(weekend,abs(weekend)*sqrt(pi/2))
index(weekend) <- as.Date(index(weekend),tz=tzone(weekend))
names(weekend) <- c("Return","Abs.Return")

c(mean(weekend[,2]),sd(weekend[,2]))
plot.zoo(weekend[,1])
plot.zoo(cumsum(weekend[,1]))
plot(weekend[,2])
acf(coredata(weekend[,2]),lag.max=60)

#compare to non-weekend close-to-open vol
non.weekend <- with(augmented.daily,abs(log(Open/Close.1))*sqrt(pi/2))[wday(index(augmented.daily))!=2]
c(mean(non.weekend),sd(non.weekend))

#compute Monday vol - abs.return and parkinson
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
f.ratio <- with(compare,merge(weekend^2/(weekend^2+monday.1^2),
                              weekend^2/(weekend^2+monday.2^2)))
names(f.ratio) <- c("monday","monday.1")

#plot density of f
pdf(file=file.path(report.path,"figures","f-histogram.pdf"))
density.grid <- seq(0,1,0.01)
hist(coredata(f.ratio[,2]),xlab="",ylab="",main="",breaks=25,xlim=c(0,1),freq=FALSE)
lines(density.grid,dexp(density.grid,1/mean(f.ratio[,2])),col="red")
dev.off()

pdf(file=file.path(report.path,"figures","f-qq.pdf"))
par(pty="s")
plot(qexp((1:nrow(f.ratio))/(1+nrow(f.ratio)),rate=1/mean(f.ratio[,2])),
     sort(coredata(f.ratio[,2])),
     xlab="Theoretical quantiles",ylab="Sample quantiles")
abline(a=0,b=1)
dev.off()

plot.zoo(f.ratio[,2])
acf(coredata(f.ratio[,2]),lag.max=60)
Box.test(coredata(f.ratio[,2]),lag=60,type="Ljung-Box")

#report vols and f estimates
vol.table <- rbind(cbind(apply(f.ratio,2,mean),
                         apply(f.ratio,2,sd)),
                   c(mean(weekend[,2]),sd(weekend[,2])),
                   c(mean(non.weekend),sd(non.weekend)),
                   cbind(apply(monday,2,mean),
                         apply(monday,2,sd))[c(2,3),]
                   )
colnames(vol.table) <- c("Mean","Std. Dev.")
rownames(vol.table) <- c("$\\widehat{f}_{A}$",
                         "$\\widehat{f}_{P}$",
                         "Monday $j$",
                         "Non-Monday $j$",
                         "Monday $c$",
                         "Monday $\\widehat\\sigma_P$"
                         )

print.xtable(
  xtable(vol.table,digits=4,
         caption=paste("$f$ estimates,",
                       "Monday and non-Monday jump volatility estimates, and",
                       "Monday open-to-close volatility estimates."),
         label="tab:weekend.vol.estim"),
  sanitize.rownames.function=function(x) {x},
  file=file.path(report.path,"tables","weekend.vol.tex")
)


#is weekend vol predictive of monday vol?
cor(compare)[1,3]
plot.default(compare[,c(1,3)])

#is weekend return predictive of monday return?
predictive <- na.trim(merge(weekend[,1],monday[,1]))
cor(predictive)[1,2]
plot.default(predictive)

strat.ret <- -sign(predictive[,1])*predictive[,2]
mean(strat.ret)*sqrt(52)/sd(strat.ret)
mean(-monday[,1])*sqrt(52)/sd(monday[,1])
plot.zoo(cumsum(na.trim(merge(strat.ret,-monday[,1]))),
         plot.type="single",xlab="",ylab="")

remove.worst <- function(strat.ret,s) {
  rets <- sort(coredata(strat.ret))[-(1:s)]
  return(mean(rets)*sqrt(52)/sd(rets))
}
plot(sapply(1:10,function(s) remove.worst(strat.ret,s)),ylab="",type="l")
lines(sapply(1:10,function(s) remove.worst(-monday[,1],s)),ylab="",col="red")
