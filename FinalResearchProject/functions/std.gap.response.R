std.gap.response <- function(daily.OHLC.prices,vol.parameter=0.1,ema.param=NA,signal="Jump") {
  #Compute vol forecasts using chosen parameter
  vol.estimates <- ComputeCCYReturns2(daily.OHLC.prices)$Garman.Klass
  vol.forecast <- exp(smooth.vol.forecast(log(vol.estimates),vol.parameter))  
  monday.vol.forecast <- vol.forecast[wday(index(vol.forecast))==2,]
  
  #Compute jumps and Monday returns
  combined <- raw.gap.returns(daily.OHLC.prices)[[2]]
  combined <- na.trim(merge(combined,monday.vol.forecast))
  
  #Compute jump from Friday highs and lows
  augmented.daily <- na.trim(merge(daily.OHLC.prices,with(daily.OHLC.prices,lag(merge(High,Low,Close),1))))
  combined <- na.trim(merge(combined,with(augmented.daily,log(merge(Open/High.1,Open/Low.1)))
                            [index(augmented.daily)%in%index(combined)]))
  names(combined) <- c("Jump","Return","Vol.Forecast","High","Low")
  
  #Compute jump from high or low depending on direction of jump
  combined$Jump.Plus <- with(combined,rbind(Low[sign(Jump)>=0],High[sign(Jump)<0]))
  combined$Envelope <- (with(augmented.daily,log(Open/Low.1)/log(High.1/Low.1))[index(augmented.daily)%in%index(combined)])
  
  #Compute stdised gaps and winsorised version for model fitting
#   if (signal=="Envelope") {
#     stdiser <- 1
#   } else {
#     stdiser <- combined$Vol.Forecast
#   }
#   std.gap <- with(combined,get(signal)/stdiser)
#   std.gap.winzd <- std.gap
#   loc <- median(std.gap)
#   scale <- IQR(std.gap)/1.34
#   std.gap.norm <- (std.gap-loc)/scale
#   thresh <- 3
#   outliers <- abs(std.gap.norm)>thresh
#   std.gap.winzd[outliers] <- loc+thresh*scale*sign(std.gap.norm[outliers])
  std.gap.winzd <- stdise(with(combined,get(signal)),combined$Vol.Forecast)
  
  #Compute trend direction
  if (!is.na(ema.param[1])) {
    trend <- xts(apply(do.call("merge",lapply(ema.param,function (p) EMA(daily.OHLC.prices$Close,p))),
                       1,diff),index(daily.OHLC.prices))
    trend <- trend[index(trend)%in%index(std.gap.winzd),]
    plot(trend,type="p",pch="+")
    #std.gap.winzd <- std.gap.winzd/sign(trend)
  }
  

  #Try a kernel smoother - loess
  std.ret <- with(combined,Return/Vol.Forecast)
#   jp.std <- with(combined,Jump.Plus/Vol.Forecast)
#   plot.default(std.gap.winzd,jp.std)
#   lm.gap <- lm(std.ret~std.gap.winzd+jp.std)
#   median(jp.std)-3*IQR(jp.std)/1.34
#   summary(lm.gap)
#   browser()
#   
#   lo.gap <- loess(std.ret~std.gap.winzd)
#   gap.grid <- seq(min(std.gap.winzd,na.rm=TRUE),max(std.gap.winzd,na.rm=TRUE),length.out=100)
#   lo.pred <- predict(lo.gap,gap.grid,se=TRUE)
#   plot(coredata(merge(std.gap.winzd,std.ret)),pch="+",col="gray",
#        xlim=quantile(std.gap.winzd,c(0,1),na.rm=TRUE),
#        ylim=quantile(std.ret,c(0.05,0.95),na.rm=TRUE))
#   lines(gap.grid,lo.pred$fit,type="l",col="red")
#   lines(gap.grid,lo.pred$fit+2*lo.pred$se.fit,lty=2,col="red")
#   lines(gap.grid,lo.pred$fit-2*lo.pred$se.fit,lty=2,col="red")
#   rug(std.gap.winzd)
#   abline(h=0); grid()
  
#   c(min(gap.grid[lo.pred$fit<0]),
#     gap.grid[length(gap.grid)-which(rev(lo.pred$fit)>0)[1]+1])
  lo.fit.plot(std.ret,std.gap.winzd,main="All")  
  if (!is.na(ema.param[1])) {
    lo.fit.plot(std.ret[trend>=0],std.gap.winzd[trend>=0],main="Uptrend")
    lo.fit.plot(std.ret[trend<0],std.gap.winzd[trend<0],main="Downtrend")
  }
}