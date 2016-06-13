#Set up paths
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

#Compute daily returns and ranges
GetCCYData("EURUSD.daily")
ret <- ComputeCCYReturns2(EURUSD.daily)["2004/2013-04",c(6,7,8)]

#Compute realized measures
GetCCYData("EURUSD")
rv <- realized.volatility(EURUSD["2004/2013-04",],min.bars=30)

require(fracdiff)
require(arfima)

apply(log(coredata(ret)),2,fdGPH)
apply(log(coredata(ret)),2,adf.test,alternative="explosive")
#not fractionally integrated
#not unit root

#try fitting ARMA models
aics <- matrix(NA,5,4)
for (p in 0:(nrow(aics)-1)) {
  for (q in 0:(ncol(aics)-1)) {
    #aics[p+1,q+1] <- AIC(arima(log(ret$Garman.Klass),order=c(p,0,q)))
    aics[p+1,q+1] <- AIC(arima(log(rv$Rlzd.Vol),order=c(p,1,q)))
  }
}
aics
aics-aics[1,1]


#Fit ARMA(1,1) model
vol.estims <- log(ret$Garman.Klass)
#vol.estims <- log(rv$Rlzd.Vol)
model <- arima(vol.estims,order=c(1,0,1))
#Examine parameter values
print(model)

#Examine residuals for evidence of poor fit
model.resid <- xts(residuals(model),index(vol.estims))
pdf(file=file.path(report.path,"figures","cumsum-resid.pdf"),
    width=14)
plot.zoo(cumsum(model.resid),
         ylab="")
abline(h=0)
dev.off()

Box.test(coredata(model.resid),lag=60)

pdf(file=file.path(report.path,"figures","acf-resid.pdf"),
    width=14)
acf(coredata(model.resid),lag.max=60,ylim=0.1*c(-1,1),main="")
dev.off()

#Examine fitted values
vol.fitted <- vol.estims - model.resid
plot.zoo(vol.fitted)
plot(vol.estims,type="l")
plot.default(merge(vol.estims,vol.fitted))
plot.zoo(exp(vol.fitted))

#Plot theoretical ACF
theoretical.acf <- function(phi,theta,lag) {
  phi^(lag-1)*(1+phi*theta)*(phi+theta)/(1+2*phi*theta+theta^2)
}
phi <- coef(model)[1]
theta <- coef(model)[2]
sample.acf <- acf(coredata(ret$Garman.Klass),lag.max=400,plot=FALSE)
pdf(file=file.path(report.path,"figures","fitted-acf.pdf"),width=14)
plot(sample.acf$lag[-1],sample.acf$acf[-1],ylim=c(-0.1,0.5),xlab="",ylab="",main="",type="l")
grid()
abline(h=0)
lines(1:400,theoretical.acf(phi,theta,1:400),col="red")
dev.off()


