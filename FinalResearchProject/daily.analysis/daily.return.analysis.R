#Set up paths
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

#Compute daily returns and ranges
GetCCYData("EURUSD.daily")
ret <- ComputeCCYReturns2(EURUSD.daily)["2004/2013-04",c(1,2,6,7,8)]

#Compute realized measures
GetCCYData("EURUSD")
rv <- realized.volatility(EURUSD["2004/2013-04",],min.bars=30)

#Combine vol measures together
vol.measures <- na.trim(merge(xts(coredata(ret),as.Date(index(ret),tz=tzone(ret))),
                              xts(coredata(rv[,1]),as.Date(index(rv),tz=tzone(rv)))))
measure.names <- c("r","\\widehat\\sigma_A","\\widehat\\sigma_P",
                   "\\widehat\\sigma_{P*}","\\widehat\\sigma_{GK*}",
                   "\\widehat\\sigma_{RV}")
#compute sample moments
require(moments)
moment.funs <- c("mean","sd","skewness","kurtosis")
moments <- do.call("rbind",
                   lapply(1:length(moment.funs),function(f) apply(vol.measures,2,moment.funs[f])))
rownames(moments) <- c("Mean","SD","Skewness","Kurtosis")
moments

#add efficiencies
Efficiency <- moments[2,2]^2/moments[2,]^2
Efficiency[1] <- NA


#add autocorrelations and Box statistics
autocors <- sapply(apply(vol.measures,2,acf,lag.max=60,plot=FALSE),"[[",1)[c(2,61),]
rownames(autocors) <- c("AC(1)","AC(60)")

Box.test <- apply(vol.measures,2,Box.test,lag=60,type="Ljung-Box")
box.stats <- sapply(Box.test,"[[","statistic")

#add correlation matrix
cor.matrix <- cor(vol.measures[,2:ncol(vol.measures)])
cor.matrix[!upper.tri(cor.matrix)] <- NA
cor.matrix <- cor.matrix[-nrow(cor.matrix),]
colnames(cor.matrix) <- NULL
rownames(cor.matrix) <- paste("$\\rho_{",measure.names[2:(length(measure.names)-1)],"}$",sep="")
cor.matrix <- cbind(rep(NA,nrow(cor.matrix)),cor.matrix)

#produce summary table
summary.table <- rbind(moments,Efficiency,autocors,box.stats,cor.matrix)
summary.table
colnames(summary.table) <- paste("$",measure.names,"$",sep="")
rownames(summary.table) <- sub("box.stats","LB(60)",rownames(summary.table))
print.xtable(xtable(summary.table,
                    digits=rbind(matrix(4,2,ncol(summary.table)+1),
                                 matrix(2,5,ncol(summary.table)+1),
                                 matrix(0,1,ncol(summary.table)+1),
                                 matrix(2,nrow(cor.matrix),ncol(summary.table)+1)),
                    caption=paste("Sample moments and efficiency,",
                                  "autocorrelations and Ljung-Box statistics,",
                                  "and cross-correlations for return and volatility estimates"),
                    label="tab:summary.stats"),
             sanitize.text.function=function(x) {x},
             hline.after=c(-1,0,cumsum(c(5,3)),nrow(summary.table)),
             file=file.path(report.path,"tables","summary.stats.tex")
)

#add stats for standardised returns
ret.std <- do.call("merge",lapply(3:ncol(vol.measures),function(j) vol.measures[,1]/vol.measures[,j]))
names(ret.std) <- names(vol.measures)[3:ncol(vol.measures)]
t.std <- apply(ret.std,2,function(r) mean(r)/(sd(r)/sqrt(length(r))))
jb.std <- apply(ret.std,2,jarque.bera.test)
lb.std <- apply(ret.std,2,Box.test,lag=60,type="Ljung-Box")
lb.std.abs <- apply(abs(ret.std),2,Box.test,lag=60,type="Ljung-Box")
std.stats <- rbind(do.call("rbind",
                           lapply(1:length(moment.funs),
                                  function(f) apply(ret.std,2,moment.funs[f]))),
                   t.std,
                   sapply(jb.std,"[[","statistic"),
                   sapply(lb.std,"[[","statistic"),
                   sapply(lb.std.abs,"[[","statistic"))
colnames(std.stats) <- paste("$r / ",measure.names[3:length(measure.names)],"$",sep="")
rownames(std.stats) <- c("Mean","Std. Dev.","Skewness","Kurtosis",
                         "$t$-statistic","Jarque-Bera","LB(60) for $s_t$","LB(60) for $|s_t|$")
print.xtable(xtable(std.stats,
                    digits=rbind(matrix(2,5,ncol(std.stats)+1),
                                 matrix(0,3,ncol(std.stats)+1)),
                    label="tab:std.stats",
                    caption=paste("First four moments, $t$, Jarque-Bera and Ljung-Box statistics of returns",
                                  "standardised by range-based and realized volatility estimates")),
             sanitize.text.function=function(x) {x},
             #hline.after=c(-1,0,nrow(std.stats)),
             file=file.path(report.path,"tables","std.stats.tex"))

#histograms
for (i in 2:ncol(vol.measures)) {
  pdf(file=file.path(report.path,"figures",
                     paste("hist-daily-",sub("\\.","-",colnames(vol.measures)[i]),".pdf",sep="")))
  print(histogram(coredata(vol.measures[,i]),nint=30,xlab="",ylab="",main="",
                  xlim=c(0,0.04)))
  dev.off()
}

require(fdrtool)
pdf(file=file.path(report.path,"figures","qq-abs-return.pdf"))
par(pty="s")
plot(qhalfnorm((1:nrow(vol.measures))/(nrow(vol.measures)+1),
               sqrt(pi/2)/mean(vol.measures$Abs.Return)),
     sort(coredata(vol.measures$Abs.Return)),
     xlab="Theoretical quantiles",
     ylab="Sample quantiles")
abline(a=0,b=1)
dev.off()

pdf(file=file.path(report.path,"figures","qq-garman-klass.pdf"))
par(pty="s")
qqnorm(log(vol.measures$Garman.Klass),main="",
       xlab="Theoretical quantiles")
qqline(log(vol.measures$Garman.Klass))
dev.off()

pdf(file=file.path(report.path,"figures","qq-parkinson.pdf"))
par(pty="s")
qqnorm(log(vol.measures$Parkinson),main="",
       xlab="Theoretical quantiles")
qqline(log(vol.measures$Parkinson))
dev.off()


#time plots
for (i in 2:ncol(vol.measures)) {
  pdf(file=file.path(report.path,"figures",
                     paste("daily-ts-",sub("\\.","-",colnames(vol.measures)[i]),".pdf",sep="")),
      height=7,width=16)
  if (i==1) {
    ylim.over <- vol.measures[,i]
  } else {
    ylim.over <- vol.measures[,2:ncol(vol.measures)]
  }
  plot.zoo(vol.measures[,i],main="",xlab="",ylab="",
           ylim=quantile(ylim.over,c(0,1)))
  roller <- rollmeanr(vol.measures[,i],60)
  lines(index(roller),coredata(roller),col="lightblue",lwd=2)
  abline(h=mean(vol.measures[,i]),lty=2,col="red")
  dev.off()
}

#autocorrelation plots
l.max <- 400
acf.periods <- c("","2004/2008-06","2009-07/")

for (i in 1:length(acf.periods)) {
  use.ret <- vol.measures[acf.periods[i],]  
  acf.obj <- lapply(c(2,5,6),function(j) acf(coredata(use.ret[,j]),lag.max=l.max,plot=FALSE))
  pdf(file=file.path(report.path,"figures",paste("acf-daily-vol-400-sub",i,".pdf",sep="")),
      width=16)
  plot(1:l.max,acf.obj[[1]]$acf[2:(l.max+1)],type="l",
       xlab="",ylab="",ylim=c(-0.1,0.7))
  lines(1:l.max,acf.obj[[2]]$acf[2:(l.max+1)],col="navy")
  lines(1:l.max,acf.obj[[3]]$acf[2:(l.max+1)],col="red")
  grid(col="gray")
  abline(h=0,lty=3)
  abline(h=qnorm(c(0.025,0.975),sd=(1/sqrt(nrow(use.ret)-l.max))),lty=2,col="blue")
  dev.off()
}





####################
# Nothing reported beyond here

#try removing outliers
#acf(coredata(rbind(ret[acf.periods[1],],ret[acf.periods[2],])[,3]),lag.max=400)
#acf(coredata(ret["2008-07/2009-06",3]),lag.max=100)
ret.trimmed <- apply(ret,2,trim.outliers,num.sd=4)
nrow(ret)
sum(ret.trimmed[[2]][[2]])
acf(coredata(as.xts(ret.trimmed[[3]][[1]])["2004/2008-06"]),lag.max=400)
acf(ret.trimmed[[3]][[1]],lag.max=400)

#Lag 1 scatter plots
for (i in 2:3) {
  pdf(file=file.path(report.path,"figures",
                     paste("auto-scatter-",
                           sub("\\.","-",colnames(ret)[i]),".pdf",sep="")))
  op <- par(pty="s")
  scatter.data <- na.trim(merge(lag(ret[,i],1),ret[,i]))
  col.navy <- col2rgb("navy")
  alpha <- 70
  plot.default(scatter.data[acf.periods[2],],
               xlab="t-1",ylab="t",xlim=c(0,0.04),ylim=c(0,0.04),pch=16,
               col=rgb(col.navy[1],col.navy[2],col.navy[3],alpha,maxColorValue=255))
  points(coredata(scatter.data[acf.periods[3],1]),
         coredata(scatter.data[acf.periods[3],2]),
         pch=16,col=rgb(255,0,0,alpha,maxColorValue=255))
  points(coredata(scatter.data["2008-07/2009-06",1]),
         coredata(scatter.data["2008-07/2009-06",2]),
         col="black",pch=4,cex=0.7)
  grid(col="gray")
  par(op)
  dev.off()
}

plot(density(ret[acf.periods[2],3]))
lines(density(ret[acf.periods[3],3]))
