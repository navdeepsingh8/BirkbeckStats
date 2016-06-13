#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
ret <- ComputeCCYReturns2(EURUSD.daily)["2004/"]
head(ret)

hlr.abs.ratio <- sort(coredata(ret$Hilo.Range / ret$Abs.Return),
                      decreasing=TRUE,index.return=TRUE)
table(wday(index(ret)[hlr.abs.ratio$ix[hlr.abs.ratio$x<1]]))



largest.tr.ratio <- sort(coredata(ret$True.Range / ret$Hilo.Range),
      decreasing=TRUE,index.return=TRUE)
head(largest.tr.ratio$x,10)
largest.tr.ratio.dates <- index(ret)[largest.tr.ratio$ix]
largest <- head(largest.tr.ratio.dates,10)

bigger.tr.table <- table(wday(largest.tr.ratio.dates[largest.tr.ratio$x>1]))
names(bigger.tr.table) <- ordered.weekdays[as.numeric(names(bigger.tr.table))]


EURUSD.daily["2005-09",]
ret["2005-09",]
pdf(file=file.path(report.path,"figures","true-range-example.pdf"),width=12)
candleChart(EURUSD.daily,subset="2005-09",multi.col=FALSE,theme='white',
            up.col="white",dn.col="black",major.ticks="months")
dev.off()

pdf(file=file.path(report.path,"figures","daily-series.pdf"),width=12)
plot.zoo(EURUSD.daily$Close["2004/"],xlab="",ylab="")
dev.off()