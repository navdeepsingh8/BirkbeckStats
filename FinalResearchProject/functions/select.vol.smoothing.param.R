select.vol.smoothing.param <- function(daily.OHLC.prices,alphas=seq(0.001,0.5,length.out=100)) {
  #Goals: 
  #1) choose exp smoothing parameter for computing E[DD]
  #2) choose how to incorporate jump size into E[DD]
  
  #Sub function to compute SSE for mondays
  #First input: vol or dd measure, second input: prediction
  compute.SSE.mon <- function(vol.estim,predicted.vol,include.jumps=FALSE) {
    actual <- vol.estim[-1,]
    jumps <- abs(all.jumps[-1,])
    fitted <- xts(sapply(lapply(predicted.vol,"[[","fitted"),function(t) exp(t[,1])),index(actual))
    actual.mon <- actual[wday(index(actual))==2,]
    fitted.mon <- fitted[wday(index(fitted))==2,]
    jumps.mon <- jumps[wday(index(jumps))==2,]
    
    SSE.mon <- apply(fitted.mon,2,function(forecast) {
      jumps.std <- jumps.mon/forecast;
      actual.std <- actual.mon/forecast;
      lm(as.formula(paste("actual.mon~forecast",ifelse(include.jumps,"+jumps.std",""),sep="")))
    }
    )
  }  
  
  #Get jumps
  augmented.prices <- na.trim(merge(daily.OHLC.prices,with(daily.OHLC.prices,lag(Close,1))))
  all.jumps <- with(augmented.prices,log(Open/Close.1))
  
  #Compute GK vol forecasts
  vol.estimates.gk <- ComputeCCYReturns2(daily.OHLC.prices)$Garman.Klass
  hw <- lapply(alphas,function(alpha) HoltWinters(log(vol.estimates.gk),alpha,FALSE,FALSE))
  
  #Compute GK+jump vol forecasts
  jumps <- raw.gap.returns(daily.OHLC.prices)[[2]][,1]
  vol.estimates.gk.jump <- vol.estimates.gk
  vol.estimates.gk.jump[index(jumps)] <- sqrt(vol.estimates.gk[index(jumps)]^2+abs(jumps)^2*(pi/2))
  hw.with.jumps <- lapply(alphas,function(alpha) HoltWinters(vol.estimates.gk.jump,alpha,FALSE,FALSE))
  
  #Compute SSE with respect to Monday Parkinson vol
  vol.estimates.park <- ComputeCCYReturns2(daily.OHLC.prices)$Parkinson  
  SSE.mon <- sapply(compute.SSE.mon(vol.estimates.park,hw),function(lm) summary(lm)$sigma)
  SSE.mon.with.jumps <- sapply(compute.SSE.mon(vol.estimates.park,hw,include.jumps=TRUE),function(lm) summary(lm)$sigma)
  plot(alphas,SSE.mon.with.jumps,type="l",col="red")
  lines(alphas,SSE.mon)  
  
  #Compute SSE with respsect to Monday drawdowns
  drawdowns <- abs(rbind(with(augmented.prices[all.jumps>0,],log(Low/Open)),
                         with(augmented.prices[all.jumps<=0,],log(High/Open))))  
  SSE.mon.dd <- sapply(compute.SSE.mon(drawdowns,hw),function(lm) summary(lm)$sigma)
  SSE.mon.dd.with.jumps <- sapply(compute.SSE.mon(drawdowns,hw,include.jumps=TRUE),function(lm) summary(lm)$sigma)
  SSE.mon.dd.with.jumps.2 <- sapply(compute.SSE.mon(drawdowns,hw.with.jumps),function(lm) summary(lm)$sigma)
  plot(alphas,SSE.mon.dd.with.jumps,type="l",col="red")
  lines(alphas,SSE.mon.dd) 
  lines(alphas,SSE.mon.dd.with.jumps.2,col="blue")
  
  
  #Return fitted risk model
  print(alphas[which.min(SSE.mon.dd.with.jumps)])
  lm.dd <- compute.SSE.mon(drawdowns,hw,include.jumps=TRUE)[[which.min(SSE.mon.dd.with.jumps)]]
  summary(lm.dd)
  return(lm.dd)
  
}