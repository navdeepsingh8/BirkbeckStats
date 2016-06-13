#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD")
idc <- EURUSD["2004-01-07 20:59/2004-01-08 20:59","Close"]


pdf(file=file.path(present.path,"figures","intra-day-chart.pdf"),
    height=9,width=7.5)
plot.zoo(idc[-1],xlab="",ylab="",main=as.Date(index(idc)[length(idc)]),type="s")
abline(h=max(idc[-1]),col="darkgreen",lty=2,lwd=3)
abline(h=min(idc[-1]),col="darkred",lty=2,lwd=3)
abline(h=idc[1],lwd=2)
abline(h=idc[length(idc)],lwd=2)
idcn <- coredata(idc)
axis(4,c(idcn[1],max(idcn[-1]),min(idcn[-1]),idcn[length(idcn)]),
     c("O","H","L","C"),las=1)
dev.off()



pdf(file=file.path(present.path,"figures","intra-day-chart-ret.pdf"),
    height=9,width=7.5)
plot.zoo(idc[-1],xlab="",ylab="",main=as.Date(index(idc)[length(idc)]),type="n")
idcn <- coredata(idc)
axis(4,c(idcn[1],max(idcn[-1]),min(idcn[-1]),idcn[length(idcn)]),
     c("O","H","L","C"),las=1)
usr <- par("usr")
rect(usr[1],idc[1],usr[2],tail(idc,1),
     border=NA,col="gray60")
#abline(h=max(idc[-1]),col="darkgreen",lty=2,lwd=3)
#abline(h=min(idc[-1]),col="darkred",lty=2,lwd=3)
abline(h=idc[1],lwd=2)
abline(h=idc[length(idc)],lwd=2)
lines(idc,type="s")
dev.off()

pdf(file=file.path(present.path,"figures","intra-day-chart-hl.pdf"),
    height=9,width=7.5)
plot.zoo(idc[-1],xlab="",ylab="",main=as.Date(index(idc)[length(idc)]),type="n")
idcn <- coredata(idc)
axis(4,c(idcn[1],max(idcn[-1]),min(idcn[-1]),idcn[length(idcn)]),
     c("O","H","L","C"),las=1)
usr <- par("usr")
rect(usr[1],min(idc[-1]),usr[2],max(idc[-1]),
     border=NA,col="gray60")
abline(h=max(idc[-1]),col="darkgreen",lty=2,lwd=3)
abline(h=min(idc[-1]),col="darkred",lty=2,lwd=3)
#abline(h=idc[1],lwd=2)
#abline(h=idc[length(idc)],lwd=2)
lines(idc,type="s")
dev.off()

pdf(file=file.path(present.path,"figures","intra-day-chart-ohlc.pdf"),
    height=9,width=7.5)
plot.zoo(idc[-1],xlab="",ylab="",main=as.Date(index(idc)[length(idc)]),type="n")
idcn <- coredata(idc)
axis(4,c(idcn[1],max(idcn[-1]),min(idcn[-1]),idcn[length(idcn)]),
     c("O","H","L","C"),las=1)
usr <- par("usr")
rect(usr[1],min(idc[-1]),usr[2],max(idc[-1]),
     border=NA,col="gray60")
rect(usr[1],idc[1],usr[2],tail(idc,1),
     border=NA,col="royalblue")
abline(h=max(idc[-1]),col="darkgreen",lty=2,lwd=3)
abline(h=min(idc[-1]),col="darkred",lty=2,lwd=3)
abline(h=idc[1],lwd=2)
abline(h=idc[length(idc)],lwd=2)
lines(idc,type="s")
dev.off()




idohlc <- EURUSD["2001-01-03 09",]
print.xtable(xtable(as.data.frame(head(idohlc)),digits=4),
             size="tiny",
             file=file.path(present.path,"tables","sample.table.tex"))


weekend <- EURUSD["2005-09-16/2005-09-19","Close"]

#xblocks(weekend>1.22,col="red")
#xblocks(weekend,
#        function(t) t>ymd_hm("2005-09-18 22:59",tz="Europe/London") &
#                    t<ymd_hm("2005-09-18 22:59",tz="Europe/London")
#        col="darkred")
pdf(file=file.path(present.path,"figures","weekend-jump.pdf"),
    width=10)
plot.zoo(weekend,
     type="s",xlab="",ylab="",
     main="2005-09-16 - 2005-09-19")
dev.off()

pdf(file=file.path(present.path,"figures","weekend-jump-shaded.pdf"),
    width=10)
plot.zoo(weekend,
     type="n",xlab="",ylab="",
     main="2005-09-16 - 2005-09-19")
usr <- par('usr')
rect(as.numeric(ymd_hm("2005-09-16 20:59",tz="Europe/London")),
     head(weekend["2005-09-18",],1),
     as.numeric(ymd_hm("2005-09-18 22:59",tz="Europe/London")),
     tail(weekend["2005-09-16",],1),
     border=NA,col="gray60")
rect(as.numeric(ymd_hm("2005-09-18 23:00",tz="Europe/London")),
     min(weekend["2005-09-19",]),
     as.numeric(ymd_hm("2005-09-19 22:59",tz="Europe/London")),
     max(weekend["2005-09-19",]),
     border=NA,col="royalblue")
lines(weekend,type="s")
dev.off()
