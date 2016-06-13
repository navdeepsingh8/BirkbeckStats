#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD")

#Compute Hilo ranges
ret <- ComputeCCYReturns2(EURUSD["2004/",])

#Filter out pre 2004 data
#ret <- ret["2004/"]

#Arrange all movements into a data table
ret.Date <- as.Date(index(ret),tz=tzone(ret))
movement  <- data.table(
  year=year(ret.Date),
  week=week(ret.Date),
  hour=hour(index(ret)),
  minute=minute(index(ret)),
  movement=coredata(ret$True.Range)>0)
movement[,day:=wday(ret.Date)]
setkeyv(movement,c("year","week","hour","day","minute"))

#Summarise by day, hour and minute
movement.summary <- movement[,mean(movement.True.Range),by=c("day","hour","minute")]
movement.intra.week <- c(rep(NA,60*2+1),movement.summary[order(day,hour,minute),V1])
pdf(file=file.path(report.path,"figures","obs-freq-week.pdf"),width=12)
plot(movement.intra.week,type="l",ylab=expression(P(X[t]==1)),xlab="",xaxt="n")
day.ticks <- c(0,seq(from=24*60+1,by=1440,length.out=4))
abline(v=day.ticks,col="red",lty=2)
axis(1,day.ticks,c("Mon","Tues","Wed","Thu","Fri"))
grid(col="gray")
dev.off()
#close to 95% movement in LDN/NYC trading
#at least 60% of minutes moving in Asian trading

#summarise by week and year
movement.time <- movement[order(year,week),mean(movement.True.Range),by=c("year","week")]
movement.time.xts <- xts(movement.time[,V1],ymd(paste(movement.time[,year],"0101",sep=""))+weeks(movement.time[,week]))
pdf(file=file.path(report.path,"figures","obs-freq-time.pdf"),width=12)
plot.zoo(movement.time.xts,xlab="",ylab=expression(P(X[t]==1)))
dev.off()
