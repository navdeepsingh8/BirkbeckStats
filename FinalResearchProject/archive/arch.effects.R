# Script: Study the ARCH return in daily and weekly returns

#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")
#years.to.get <- 3;
#EURUSD <- EURUSD["2001/2002",]

#Now compute weekly bars and vol measures
#Aggregate data to weekly bars to eliminate session effects
#if changing periodFlag parameter run from here onwards
# custom week close parameter
periodFlags <- c(-1,0,3)
periodLabels <- c("daily","weekly Sun-Fri","weekly Wed-Wed")

#data.frame to store p-values later on
lb.test.pval <- list()

for (periodFlag in periodFlags) {
  
 if (periodFlag>=0) {
    EURUSD.period <- custom.week(EURUSD,periodFlag)
    num.lags <- 50
  } else if (periodFlag==-1) {
    EURUSD.period <- custom.daily(EURUSD)
    num.lags <- 250
  }
    
  # Compute returns and vol measures
  retEURUSD.period <- ComputeCCYReturns2(EURUSD.period)
    
  #Plots for the measures - time series, scatter and ACF
  pdf(file=paste("output/EURUSD.period.range",periodFlag,"pdf",sep="."),paper="a4r")
  
  #Time series of vol measures
  plot(retEURUSD.period$True.Range,ylim=c(0,0.06),
       main="")
  lines(retEURUSD.period$Hilo.Range,col="red")
  lines(abs(retEURUSD.period$Return),col="green")
  
  #Relationship between Hilo.Range and Range
  op <- par(pty="s")
  plot.default(retEURUSD.period$Hilo.Range,
               retEURUSD.period$True.Range)
  grid()
  abline(0,1)
  plot.default(retEURUSD.period$Abs.Return,
               retEURUSD.period$True.Range)
  grid()
  abline(0,1)
  par(op)
  
  #Plot ACF plots for these measures
  lapply(1:ncol(retEURUSD.period), function(x)
    acf.plots(coredata(retEURUSD.period[,x]),
              lag.max=num.lags,
              main=colnames(retEURUSD.period)[x]))
  dev.off()
  
  #Record Ljung-Box statistics
  lb.test <- apply(retEURUSD.period,2,Box.test,lag=num.lags,type="Ljung-Box")
  lb.test.pval[[length(lb.test.pval)+1]] <- sapply(lb.test,"[[","statistic")
  lb.test.pval[[length(lb.test.pval)+1]] <- sapply(lb.test,"[[","p.value")
}

#Output to a Latex table
lb.test.pval.frame <- as.data.frame(do.call("rbind",(lapply(lb.test.pval,t))))
rownames(lb.test.pval.frame) <- paste(rep(periodLabels,c(2,2,2)),rep(c("statistic","$p$-value"),3))
colnames(lb.test.pval.frame) <- c("$R_t$","$|R_t|$","$TR_t$","$HLR_t$")
print.xtable(xtable(lb.test.pval.frame,digits=2),
             file=file.path(report.path,"table.lb.pval.week.tex"),
             sanitize.text.function=function(x){x})
