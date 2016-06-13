setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

#Get daily measures
load(file.path(data.path,"EURUSD.daily.RData"))
EURUSD.daily.ret <- ComputeCCYReturns2(EURUSD.daily)
head(EURUSD.daily.ret)

plot(density(EURUSD.daily.ret$Hilo.Range))

#Get realized measures
load(file.path(code.path,"vol.aggregate.ljung.box.RData"))
realized.measures <- v$daily$"5"
head(realized.measures)

merged.measures <- as.data.frame(na.trim(merge(EURUSD.daily.ret[,-1],
                            realized.measures[,-1])))
head(merged.measures)
op <- par(pty="s")
lapply(1:3,function(x) { plot(merged.measures[,c(x,4)],
     ylab="Mean 5-minute absolute return"); grid() })
par(op)

cor.realized <- matrix(sapply(1:3,function(x) 
  cor(merged.measures[,4],merged.measures[,x])),
                       nrow=1)
colnames(cor.realized) <- c("$|R_t|$","$HLR_t$","$TR_t$")
print(xtable(cor.realized,
       caption="Pearson correlation between mean 5-minute absolute returns and daily volatility measures"),
      file=file.path(report.path,"table.compare.realized.tex"),
      include.rownames=FALSE,
      sanitize.colnames.function = function(x) {x}
      )


