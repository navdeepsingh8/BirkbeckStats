#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD.daily")
EURUSD.daily <- EURUSD.daily["2004/",]
EURUSD.daily.ret <- ComputeCCYReturns2(EURUSD.daily)

#Build custom measures
EURUSD.daily.cm <- ComputeCCYCM(EURUSD.daily)
EURUSD.daily.merged <- na.trim(merge(EURUSD.daily.ret,EURUSD.daily.cm))

#try standardising the data by volatility forecasting
vol.forecasts <- smooth.vol.forecast(EURUSD.daily.merged$Abs.Return)
EURUSD.daily.merged.std <- standardise(EURUSD.daily.merged,vol.forecasts)

#try removing outliers
EURUSD.daily.merged.std.out <- xts(sapply(apply(EURUSD.daily.merged.std,2,trim.outliers),"[[",1),
                                   index(EURUSD.daily.merged.std))

#Check for covariance stationarity of hilo range
hlr <- EURUSD.daily.merged$Hilo.Range
f <- filter(hlr,rep(1,100)/100,"convolution",1)
f.v <- filter(coredata(hlr)-f,rep(1,100)/100,"convolution",1)
g <- zoo(cbind(f,f.v),index(hlr))
plot(g)
hlr.kpss <- kpss.test(hlr)

lo.rs.test(hlr,10)

#Print decomposition to latex tables
print.xtable(xtable(range.decompose(EURUSD.daily.merged)),
             file=file.path(report.path,"tables","range.decomp.tex"),
             floating=FALSE)
print.xtable(xtable(range.decompose(EURUSD.daily.merged["2004/2008-06"])),
             file=file.path(report.path,"tables","range.decomp.1.tex"),
             floating=FALSE)
print.xtable(xtable(range.decompose(EURUSD.daily.merged["2009-07/"])),
             file=file.path(report.path,"tables","range.decomp.2.tex"),
             floating=FALSE)
print.xtable(xtable(range.decompose(EURUSD.daily.merged.std)),
             file=file.path(report.path,"tables","range.decomp.std.tex"),
             floating=FALSE)


#Plot LOESS plots to see whether the relationships are non-linear
loess.result(EURUSD.daily.merged,"PostDown","PreDown")
loess.result(EURUSD.daily.merged.std,"PostDown","PreDown")

a <- na.trim(with(EURUSD.daily.merged.std,merge(lag(PreDown,1),PostUp)))
lm.result <- lm(PostUp~PreDown,data=a)
summary(lm.result)

# There does seem to be a fairly negative correlation but it is more in the tails
# and stronger for non-standardised returns which is odd

loess.result(EURUSD.daily.merged,"PostDown","PreDown")
loess.result(EURUSD.daily.merged,"PostDown","PreUp")
loess.result(EURUSD.daily.merged,"Return","PreUp")
loess.result(EURUSD.daily.merged,"PostUp","PreDown")
loess.result(EURUSD.daily.merged,"Return","PreDown")
loess.result(EURUSD.daily.merged.std,"PostDown","PreDown")

loess.result(EURUSD.daily.merged.std,"Return","PreMax")



