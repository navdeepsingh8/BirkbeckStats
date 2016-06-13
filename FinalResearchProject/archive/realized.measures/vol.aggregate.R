# Script: Study the dependence of aggregated intraday volatiliy measures
#Try aggregating different sized bars over 1 week and 1 day
#and measure the dependence in the aggregated measures

#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")
#EURUSD <- EURUSD["2001/2005",]

v <- list()
lb <- list()
min.bars <- c(5,15)
agg.fun <- "mean"
low.freq <- c("daily","weekly")
              
for (l in low.freq) {
  v[[l]] <- list()
  lb[[l]] <- list()
  for (m in min.bars) {
    char.m <- as.character(m)
    vv <- volatility.aggregate(EURUSD,m,l,agg.fun)
    v[[l]][[char.m]] <- vv
    
    #Produce time series plots and ACF plots
    Box.test <- list()
    pdf(file=paste("output/EURUSD.vol.aggregate",agg.fun,m,l,"pdf",sep="."),
        paper="a4r")
    for (measure in names(vv)) {
      title <- paste(l,agg.fun,"of",m,"minute",measure)
      plot(vv[,measure],main=title)
      lags.to.use <- ifelse(l=="daily",250,50)
      acf.plots(coredata(vv[,measure]),lag.max=lags.to.use,main=title)
      box.result[[measure]] <- 
        Box.test(coredata(vv[,measure]),
                 lag=lags.to.use,
                 type="Ljung-Box")
    }
    dev.off()
    
    #Ljung-Box test values
    lb[[l]][[char.m]] <- sapply(box.result,"[",c("statistic","p.value"))
    
    print(title)
  }
}

#Print LB values in Latex table
lb.table <- do.call("rbind",lapply(1:length(lb),function(x) do.call("rbind",lb[[x]])))
#nb next line accounts manually for the number of combinations
rownames(lb.table) <- paste(rep(low.freq,c(4,4)),
                            rep(rep(min.bars,c(2,2)),2),
                            rep("min",8),
                            rownames(lb.table))
colnames(lb.table) <- c("$R_t$","$|R_t|$","$TR_t$","$HLR_t$")
print.xtable(xtable(lb.table,digits=2),
             file=file.path(report.path,"table.lb.aggreg.tex"),
             sanitize.text.function=function(x){x})

#Check for long-memory
apply(v$daily$"5",2,acf.plots,lag.max=1000)

save(v,lb,file=file.path(code.path,"vol.aggregate.ljung.box.RData"))
