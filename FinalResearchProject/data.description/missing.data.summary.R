setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD.daily")


ix <- index(EURUSD.daily)
dd <- as.Date(ix,tz=tzone(EURUSD.daily))
wd <- wday(dd)

split.count <- lapply(split(EURUSD.daily$Count,wd),as.xts)
split.names <- ordered.weekdays[as.numeric(names(split.count))]


for (n in 1:length(split.count)) {
  pdf(file=file.path(report.path,"figures",
                     paste("fig-missing-data-",split.names[n],".pdf",sep="")))
  plot.zoo(split.count[[n]],xlab="",ylab="",
           ylim=c(600,1500))
  dev.off()
}

