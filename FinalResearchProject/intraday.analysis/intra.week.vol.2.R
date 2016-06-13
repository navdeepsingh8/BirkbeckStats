#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

load(file.path(data.path,"intra.day.returns.RData"))

# or 

#GetCCYData("EURUSD")
# ret <- ComputeCCYReturns2(EURUSD["2004/",])

#tried aggregating to 5-minute bars
#but there is a bug in the function:
#it does not produce unique minutes
#for e.g. 5-minutes may give closing minutes of 04,09,14 etc. for some days
#and 08,13,18 for other days
#agg.bars <- aggregate.bars(EURUSD["2004/",],1)

#Arrange all movements into a data table
# ret.Date <- as.Date(index(ret),tz=tzone(ret))
# movement  <- data.table(
#   year=year(ret.Date),
#   week=week(ret.Date),
#   day=wday(ret.Date),
#   hour=hour(index(ret)),
#   minute=minute(index(ret)),
#   coredata(ret$Return),
#   coredata(ret$Abs.Return),
#   coredata(ret$True.Range),
#   coredata(ret$Garman.Klass)
# )
# setkeyv(movement,c("year","week","day","hour","minute"))
# 
# save(movement,file=file.path(data.path,"intra.day.returns.RData"))

####################
# Obs freq
obs.freq <- movement[order(day,hour,minute),
                     mean(True.Range>0),
                     by=c("day","hour","minute")]

obs.freq.time <- movement[order(year,week),mean(True.Range>0),by=c("year","week")]
obs.freq.time.xts <- xts(obs.freq.time[,V1],ymd(paste(obs.freq.time[,year],"0101",sep=""))+weeks(obs.freq.time[,week]))

#Plot output
na.padding <- rep(NA,2*60+1)
day.breaks <- seq(from=0,by=1440,length.out=5)
hour.ticks <- c(day.breaks+9*60,day.breaks+15*60)

#Observation frequency intra week
pdf(file=file.path(report.path,"figures","obs-freq-week.pdf"),width=12)
plot(c(na.padding,obs.freq$V1),
     type="l",ylab=expression(P(X[t]==1)),xlab="",xaxt="n")
abline(v=day.breaks,col="red",lty=2)
axis(1,day.breaks,c("Mon","Tues","Wed","Thu","Fri"))
axis(1,hour.ticks,rep(c("06h","12h"),c(5,5)))  
dev.off()

#Obs freq thru time
pdf(file=file.path(report.path,"figures","obs-freq-time.pdf"),width=12)
plot.zoo(obs.freq.time.xts,xlab="",ylab=expression(P(X[t]==1)))
dev.off()

##################
# Intra week vol seasonality
#Standardise abs return and ranges by week

movement[,std.var.week:=Return^2/sum(Return^2),by=c("year","week")]
#movement[,std.gk.week:=sqrt(Garman.Klass^2/sum(Garman.Klass^2)),by=c("year","week")]

#NB. There is one NA in the standardised volatilities - remove it
#movement[is.na(std.abs.return),]
#correct this by making a proper week index and day index that 
#starts from the top of the series and carries all the way through
movement <- movement[-(year==2007&week==53),]
movement[week==53,]

intra.week.vol <- movement[order(day,hour,minute),
                           median(std.var.week,na.rm=TRUE),
                           by=c("day","hour","minute")]

pdf(file=file.path(report.path,"figures","intra-week-abs-return.pdf"),
    width=12)
with(intra.week.vol, {
  plot(c(na.padding,V1),type="l",
       ylim=quantile(V1[-1],c(0,0.999)),xaxt="n",
       ylab="",xlab="");
  abline(v=day.breaks,col="red",lty=2)
  axis(1,day.breaks,c("Mon","Tue","Wed","Thu","Fri"))
  axis(1,hour.ticks,rep(c("06h","12h"),c(5,5)))  
})
dev.off()


#Fit a smooth function to intra-day volatility
#Fit it to the intra-day variance as this is additive
movement[,tday:=day+(hour>21)]
movement[,min.day:=ifelse(hour<21,hour+(24-21),(hour-21))*60+(minute+1)]
movement[,std.var.day:=Return^2/sum(Return^2),by=c("year","week","tday")]
spl.intra.day.vol <- with(movement[!(is.na(std.var.day)|day==1),],
                          smooth.spline(min.day,std.var.day,
                                        all.knots=TRUE,df=(3*5)-2,
                                        keep.data=FALSE),)
spl.intra.day.vol$y <- with(spl.intra.day.vol,y/sum(y))

pdf(file=file.path(report.path,"figures","intra-day-vol-function.pdf"),
    width=12)
plot(spl.intra.day.vol,type="l",xlab="minute",ylab="")
dev.off()

save(spl.intra.day.vol,file=file.path(code.path,"intra.day.vol.RData"))

#range seasonality
# pdf(file=file.path(report.path,"figures","intra-week-true-range.pdf"),
#     width=12)
# with(intra.week.vol, {
#   plot(c(na.padding,V2),type="l",
#        ylim=quantile(V2[-1],c(0,1)),xaxt="n",
#        ylab="",xlab="");
#   abline(v=day.breaks,col="red",lty=2)
#   axis(1,day.breaks,c("Mon","Tue","Wed","Thu","Fri"))
#   axis(1,hour.ticks,rep(c("06h","12h"),c(5,5)))  
# })
# dev.off()

# build standardised intra-day returns
movement[,rlzd.var.day:=sum(Return^2),by=c("year","week","tday")]
movement[,resid.ret:=Return/sqrt(spl.intra.day.vol$y[min.day]*rlzd.var.day)]
#standardise first minute of week too
movement[tday==2&min.day==121,resid.return:=Return/sqrt(median(std.var.day)*rlzd.var.day)]

#inspect the residual returns
qqnorm(movement[year==2004&week<5,resid.ret])
qqline(movement[year==2004&week<5,resid.ret])
r <- movement[year==2004&week<50,Return]
rr <- movement[year==2004&week<50,resid.ret]
symmetry.plot(rr,type="l")
#symmetrical but very fat tailed

#rolling 1-minute mean volatility - non-overlapping days
daily.sds <- movement[,list(mean(resid.ret),mean(abs(resid.ret))),by=c("year","week","tday")]
plot(daily.sds$V1,type="l",ylim=c(-0.1,0.1))
plot(daily.sds$V2,type="l")
daily.sds[with(daily.sds,which(V2==max(V2))),]

plot(cumsum(rr),type="l")
plot(cumsum(r),type="l")
acf(rr,lag.max=400,ylim=c(-0.1,0.1))
acf(abs(rr),lag.max=1000,ylim=c(-0.1,0.2))
acf(abs(r),lag.max=1000,ylim=c(-0.1,0.2))
#first year of 1-minute returns shows a strong AC at 1-minute lag
#abs returns show decaying autocorrelations
arma.model <- arima(abs(rr),order=c(2,0,1))
print(arma.model)
acf(residuals(arma.model),lag.max=400,ylim=c(-0.1,0.1))

plot(movement[year==2004&week<50,cumsum(Return)],type="l")
acf(movement[year==2004&week<5,abs(Return)],lag.max=400)

