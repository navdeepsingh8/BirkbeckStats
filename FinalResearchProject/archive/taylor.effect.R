#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

GetCCYData("EURUSD.daily")
EURUSD.daily.ret <- ComputeCCYReturns2(EURUSD.daily)
EURUSD.daily.ret <- EURUSD.daily.ret["2004/",]
plot(EURUSD.daily.ret$Return)

#apply outlier trimming
trimming <- TRUE;
if (trimming) {
  EURUSD.daily.ret.trimmed <- apply(EURUSD.daily.ret,2,trim.outliers)
  apply(sapply(EURUSD.daily.ret.trimmed,"[[",2),2,mean)
  EURUSD.daily.ret <- as.xts(sapply(EURUSD.daily.ret.trimmed,"[[",1))
}

#Long ACF plots
lapply(1:ncol(EURUSD.daily.ret),function(x)
  acf.plots(coredata(EURUSD.daily.ret[,x]),
          lag.max=1000))

#Ljung-box statistics for powers
vol.powers <- seq(0.1,3,0.1)
ljung.box.lags <- seq(10,250,10)
taylor.test.val <- list()
for (m in names(EURUSD.daily.ret)[-1]) {
  measure.stat.val <- list()
  for (lag in ljung.box.lags) {
  b <- lapply(vol.powers,function(x) 
    Box.test(coredata(EURUSD.daily.ret)[,m]^x,
             type="Ljung-Box",lag=lag))
  measure.stat.val[[as.character(lag)]] <- (sapply(b,"[[","statistic")-lag)/sqrt(2*lag)
  }  
  taylor.test.val[[m]] <- do.call("rbind",measure.stat.val)  
}

#Plot test values - image plots
lapply(taylor.test.val,function(x) image(ljung.box.lags,vol.powers,x))

#Lattice plots - 3d surface plots
pdf(file="output/taylor.effect.pdf",paper="a4r")
lapply(1:length(taylor.test.val),function(x) 
  wireframe(z.score~lag*power,
            data=cbind(expand.grid(lag=ljung.box.lags,power=vol.powers),
                       z.score=as.vector(taylor.test.val[[x]])),
            scales=list(arrows=FALSE),
            drape = TRUE,colorkey = TRUE,
            screen = list(z = -80, x = -60),
            main=names(taylor.test.val)[x]
            )
       )

#Plot curves for 250 lags
plot(vol.powers,taylor.test.val[[1]][length(ljung.box.lags),],
     type="l",ylab="Z-score (250 lags)",main="Absolute Return")
grid()

plot(vol.powers,taylor.test.val[[2]][length(ljung.box.lags),],
     type="l",ylab="Z-score (250 lags)",main="High-Low Range / True Range")
lines(vol.powers,taylor.test.val[[3]][length(ljung.box.lags),],
      lty=2)
grid()
dev.off()

#Plot ACF for |R_t|^1.5
acf(coredata(EURUSD.daily.ret$Abs.Return),lag.max=400,ylim=c(0,0.4))
#Plot ACF for HLR_t^0.5
acf(coredata(EURUSD.daily.ret$Hilo.Range),lag.max=400,ylim=c(0,0.4))

#Try fitting a functional to the ACF
acf.values <- apply(EURUSD.daily.ret,2,acf,plot=FALSE,lag.max=1000)
acf.values <- data.frame(sapply(acf.values,"[[","acf")[-1,])

plot(acf.values$Abs.Return)
plot(acf.values$Hilo.Range)


