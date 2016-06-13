#Analysis in this script:
# Fed daily FX data for developed currencies
# Time plots of prices, returns and absolute returns
# Location, scale and tail statistics for marginal returns
# Normality test statistic
# Plots and tests for autocorrelation (acf, Ljung-Box, variance ratios)
# Stationarity plots of return location and scale, and autocorrelation structure

#load packages
source("functions.R")

#load data from R data file
load("FedData.Rdata")

###########################################
#Time plots
#Plot all prices on one panel and group by price levels
op <- par(mfrow=c(4,3),mar=c(2,2,2,2))
for (i in idx.dev) {
    plot(FedData[,i],main=idx.dev[i])
}
dev.off()
idx.dev.similar = list(c(1,2,3,7,8,10),c(4,5,6),9)

#Plot charts by group: price, returns, absolute returns, cumulative returns
for (s in 1:length(idx.dev.similar)) {
    group.idx <- idx.dev.similar[[s]]
    par(mfrow=c(length(group.idx),1),mar=c(2,2,2,2))

    #price charts
    for (ccy in group.idx) {
        plot(FedData[,ccy],plot.type="single")
    }
    #dev.off()

    #return and absolute return charts

    #cumulative return charts

    #Calculate and plot returns and compound returns
    ## ret <- Return.calc(FedData,method="compound")
    ## par(mfrow=c(2,1))
    ## plot(ret,plot.type="single")
    ## plot(abs(ret),plot.type="single")
    ## dev.off()

    ## ret.cum <- cumprod(na.locf(1+ret))-1
    ## plot(ret.cum,plot.type="single")
    ## dev.off()

}

#Comments about time plots

#######################################################
## Marginal return distributions: location, scale and tails

# mean, quantiles, standard deviation
summary(as.data.frame(ret))
boxplot(as.data.frame(ret))
abline(0,0)
ret.sd <- sapply(as.data.frame(ret),sd,na.rm=TRUE)
ret.mad <- sapply(as.data.frame(ret),mad,na.rm=TRUE)
par(mfrow=c(2,1))
barplot(sort(ret.sd,decreasing=TRUE),main="sd")
barplot(sort(ret.mad,decreasing=TRUE),main="mad")
lapply(ret[,idx.subdev],t.test)
#Returns are centred around zero
#Only SGD rejects a zero mean for the marginal return distribution at 5%
#GBP and CAD have the lowest variances, except for SGD which has a managed float vs USD

# tail densities
barplot(sapply(abs(ret) > 1.96*ret.mad,mean,na.rm=TRUE))
barplot(sapply(abs(ret) > 3*ret.mad,mean,na.rm=TRUE))
dev.off()
# test of zero skewness and leptokurtosis (i.e. normality)
# see bai and ng, vavra for modified jb tests
skewness(ret,na.rm=TRUE)
kurtosis(ret,na.rm=TRUE)-3
##QQ-plot of data against normal quantiles and against each other
par(mfrow=c(length(idx.subdev),length(idx.subdev)),pty="s",mar=c(2,2,2,2))
for (i in 1:length(idx.subdev)) {
    for (j in 1:length(idx.subdev)) {
        if (i==j) {
            qqnorm(coredata(ret)[,i],main="",xlab="",ylab="")
            qqline(coredata(ret)[,1])
            title(main=idx.subdev[i])
        } else if (i>j) {
           #plot quantiles of i (later ccy) against j (earlier ccy)
           qqplot(coredata(ret)[,j],coredata(ret)[,i],main="",xlab="",ylab="")
           abline(0,1)
       } else {
           plot(1,type="n",main="",xlab="",ylab="",axes=FALSE)
       }
    }
}
#Tail density, kurtosis and QQ plots indicate that
#returns are fat tailed - this could be due to non-stationarity
#of the marginal return distribution
#via a time-varying variance: heteroscedasticity

#Comment on skewness


###################################################################
# Joint return and absolute return distributions: strength of dependence
# by way of autocorrelations and other statistics
#1) Correlogram
#op <- par(mfrow=c(2,1))
num.lags=30
ret.acf <- vector("list",2)
for (i in 1:ncol(ret)) {
    ret.acf[[i]] <- acf(ret[,i],
                        lag.max=30,
                        na.action=na.pass,
                        main=names(ret)[i],
                        ylim=c(-0.25,0.25))
}
#par(op)
#some autocorrelations appear to be significant but not convincingly so

#1a) variance ratio plot
var.buckets=floor(dim(ret)[1]/200)
ret.var.bucket <- matrix(nrow=var.buckets,ncol=dim(ret)[2])
ret.mad.bucket <- ret.var.bucket
for (t in 1:var.buckets) {
  bucket.return <- period.apply(ret,seq(1,dim(ret)[1],t),colSums,na.rm=TRUE)
  ret.var.bucket[t,] <- apply(bucket.return,2,var)
  ret.mad.bucket[t,] <- apply(bucket.return,2,mad)
  cat(t," ")
}
ret.var.ratio <- zoo((ret.var.bucket/
                      matrix(rep(1:var.buckets,2),nrow=var.buckets,ncol=2))/
                      matrix(ret.var.bucket[1,],nrow=var.buckets,ncol=2,byrow=TRUE))
ret.mad.ratio <- zoo((ret.mad.bucket/
                      sqrt(matrix(rep(1:var.buckets,2),nrow=var.buckets,ncol=2)))/
                      matrix(ret.mad.bucket[1,],nrow=var.buckets,ncol=2,byrow=TRUE))
plot(ret.var.ratio,type="b",plot.type="single",col=1:2)
plot(ret.mad.ratio,type="b",plot.type="single",col=1:2)
abline(h=1,lty=2)
legend("topleft",c(names(ret)),col=c(1,2),lty=1)
ccy=3
VR.plot(ret[!is.na(ret[,ccy]),ccy],1:30)
#CAD has mean-reversion, GBP has positive autocorrelation
#But is this statistic robust to outliers?
#Apparently the Lo-MacKinlay test is robust to heteroskedasticity

# tests of uncorrelatedness exist although some of these may assume
# mean, variance and/or correlation stationarity
# ljung-box test: null = no significant correlations up to lag 8
#1) Ljung-Box test with lag 8
ret.ljung.box.pval <- box.stat(ret,stat="p.value",lag=floor(log(dim(ret)[1])))
#result: null of no significant autocorrelations is not rejected at 5% level

#2) variance ratio bootstrap test
Boot.test(ret[!is.na(ret[,2]),2],seq(2,20,2),nboot=100,wild="Normal")


##########################################################################
# Stationarity testing: mean and variance of marginal return distribution,
# and dependence structure of the joint distributions of returns and absolute returns
#
#2) mean stationarity: rolling mean plot
roll.window <- 250
ret.rollmean <- rollapply(ret,roll.window,mean,na.rm=TRUE)
plot(ret.rollmean)
#rolling median is more robust to outliers
ret.rollmedian <- rollapply(ret,roll.window,median,na.rm=TRUE)
plot(ret.rollmedian)
#generally close to zero but
#sometimes strays to between 5-10bp for periods
#punctuated by sharp moves of up to 10bp in a few weeks

#3) variance stationarity
ret.rollmeanvol <- sqrt(rollapply(ret,roll.window,sd,na.rm=TRUE))
plot(ret.rollmeanvol)
#mad is more robust to outliers
ret.rollmad <- rollapply(ret,roll.window,mad,na.rm=TRUE)
plot(ret.rollmad)
#evidence of different volatility regimes: generally higher later in the sample

#3) autocorrelation stationarity
#use ljung-box statistic to measure degree of return autocorrelation up to lag 4
#Q) does this statistic measure autocorrelation assuming the returns process has a constant mean and variance over each measurement period (i.e. 12 months)? YES - because a constant mean is removed from each observation when computing the autocorrelations for each statistic
box.stat <- function(x,stat="statistic",lag=4) {
    test.result <- apply(x,2,Box.test,lag=lag,type="Ljung")
    sapply(test.result,"[[",stat)
}

#stationarity of return autocorrelations
ret.period.box.stat <- period.apply(ret,INDEX=endpoints(ret,on="years",k=1),FUN=box.stat)
plot.zoo(ret.period.box.stat,type="b",plot.type="single",col=c(1,2),main="Yearly Ljung-Box statistic")
legend("topright",c(names(ret),".95 sig. level"),col=c(1,2,1),lty=c(1,1,2))
abline(h=qchisq(0.95,5),lty=2)
#a few periods with significant autocorrelations, a few others without
#nb significant autocorrelations could be a result of a higher frequency time variation in the mean return
#comment on whether these observations are consistent with the rolling mean statistics

#stationarity  volatility autocorrelations
ret.period.box.stat.vol <- period.apply(ret^2,INDEX=endpoints(ret,on="years",k=1),FUN=box.stat)
plot.zoo(ret.period.box.stat.vol,type="b",plot.type="single",col=c(1,2),main="Yearly Ljung-Box statistic")
legend("topright",c(names(ret),".95 sig. level"),col=c(1,2,1),lty=c(1,1,2))
abline(h=qchisq(0.95,5),lty=2)
# non-stationarity of variance seems much more pronounced than non-stationarity of mean
# GARCH or stochastic volatility models are able to generate time-varying variance

# formal tests of stationarity include:
# adf.test: null = unit root
ret.adf.test <- adf.test(coredata(ret)[!is.na(ret[,2]),2])
#unit root rejected


#######################################################
#Some more code here for testing

#all above analysis carried out on daily returns
#can carry out lower frequency analysis
priceMonthly <- to.monthly(FedDataSub$GBP)
retMonthly <- Return.calculate(priceMonthly[,4])
acf(retMonthly,na.action=na.pass,lag.max=20)
