setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")

#compute realized measures using 30 minute sub-periods
r <- realized.volatility(EURUSD["2004/"],min.bars=30)
head(r)
table(r$Count)

#time series plots
for (nm in c("Rlzd.Vol","Rlzd.Range")) {
  pdf(file=file.path(report.path,"figures",
                     paste("ts-",sub("\\.","-",nm),".pdf",sep="")),height=7,width=16)
  plot.zoo(r[,nm],xlab="",ylab="")
  lines(rollmeanr(r[,nm],60),col="lightblue",lwd=2)
  grid(lty=2,lwd=1,col="gray")
  abline(h=mean(r[,nm]),lty=2,col="red")
  dev.off()
}


#histograms
pdf(file=file.path(report.path,"figures","hist-rlzd-vol.pdf"))
histogram(coredata(r$Rlzd.Vol),nint=30,xlab="",ylab="")
dev.off()
pdf(file=file.path(report.path,"figures","hist-rlzd-range.pdf"))
histogram(coredata(r$Rlzd.Range),nint=30,xlab="",ylab="")
dev.off()

#scatter
op <- par(pty="s")
with(r,plot.default(Rlzd.Vol,Rlzd.Range))
abline(a=0,b=1,col="red",lty=2)

#scatter with absolute daily return
GetCCYData("EURUSD.daily")
daily.ret <- ComputeCCYReturns2(EURUSD.daily["2004/"])
all.ret <- merge(xts(r,as.Date(index(r),tz="Europe/London")),
                 xts(daily.ret,as.Date(index(daily.ret),tz="Europe/London")))
with(all.ret,plot.default(Abs.Return,Rlzd.Vol))
abline(a=0,b=1,col="red",lty=2)

#acfs
l.max <- 400
acf.periods <- c("","2004/2008-06","2009-07/")

for (i in 1:length(acf.periods)) {
  use.ret <- r[acf.periods[i],]  
  acf.obj <- lapply(1:2,function(j) acf(coredata(use.ret[,j]),lag.max=l.max,plot=FALSE))
  pdf(file=file.path(report.path,"figures",paste("acf-rlzd-vol-400-sub",i,".pdf",sep="")),
      width=16)
  plot(1:l.max,acf.obj[[2]]$acf[2:(l.max+1)],type="l",col="navy",
       xlab="",ylab="",ylim=c(-0.1,1))
  lines(1:l.max,acf.obj[[1]]$acf[2:(l.max+1)])
  grid(col="gray")
  abline(h=0,lty=3)
  abline(h=qnorm(c(0.025,0.975),sd=(1/sqrt(nrow(use.ret)-l.max))),lty=2,col="blue")
  dev.off()
}

#scatter against lag 1
plot.default(na.trim(with(r,merge(lag(Rlzd.Vol,1),Rlzd.Vol))))

#ljung-box stats
Box.tests <- apply(coredata(r)[,1:2],2,Box.test,lag=60,type="Ljung-Box")
extract.items <- c("statistic","p.value")
Box.table <- t(sapply(1:2,function(x) sapply(Box.tests,"[[",extract.items[x])))
colnames(Box.table) <- c("$RV_t$","$RR_t$")
rownames(Box.table) <- c("statistic","$p$-value")
print.xtable(xtable(Box.table,
       label="tab:rlzd.box.test",
       caption="Ljung-Box statistics for $RV_t$ and $RR_t$"),
             file=file.path(report.path,"tables","box.test.rlzd.tex"),
             sanitize.text.function=function(x){x})


qqnorm(coredata(log(daily.ret$Hilo.Range)))
qqline(coredata(log(daily.ret$Hilo.Range)))

qqnorm(coredata(log(r$Rlzd.Range)))
qqline(coredata(log(r$Rlzd.Range)))

#Comparison of intraday and daily measures
vol.measure.cor <- xtable(cor(all.ret[,c(5,6,1,2)]))
colnames(vol.measure.cor) <- c("$|R_t|$","$HLR_t$","$RV_t$","$RR_t$")
rownames(vol.measure.cor) <- colnames(vol.measure.cor)
print.xtable(vol.measure.cor,
             file=file.path(report.path,"tables","vol.measure.cor.tex"),
             sanitize.text.function=function(x) {x})

#Compare hilo-range with rlzd vol:
with(all.ret,plot.default(log(Abs.Return),log(Hilo.Range)))
abline(a=0,b=1,col="red",lty=2)
histogram(coredata(with(all.ret,log(Rlzd.Vol))),nint=30)

#PP/ADF test does not reject stationarity in favour of explosive roots
pp.test.results <- apply(all.ret,2,pp.test,alternative="explosive")
sapply(pp.test.results,"[[","p.value")
adf.test.results <- apply(all.ret,2,adf.test,alternative="explosive")
sapply(adf.test.results,"[[","statistic")
sapply(adf.test.results,"[[","p.value")

#Fit an ARFIMA model to HiloRange
LL <- matrix(nrow=6,ncol=6)
LL2 <- LL
for (i in 0:5) {
  for (j in 0:5) {
    LL[i+1,j+1] <- 2*(i+j-fracdiff(all.ret$Hilo.Range,nar=i,nma=j)$log.likelihood)
    LL2[i+1,j+1] <- summary(arma(all.ret$Hilo.Range,order=c(i,j)))$aic
  }
}
LL
LL2
LL-LL[1,1]
LL2-LL2[1,1]
image(LL-LL[1,1])
#HiloRange and Realised Rang : ARIMA(2,d,1)
LL[3,2]
#Realised vol : ARIMA(1,d,1)

#question: what if we fit an ARMA model instead


#compare AICs
exp(0.5*(LL[3,2]-arma.fit$aic))
