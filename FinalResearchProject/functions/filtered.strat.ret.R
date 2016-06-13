filtered.strat.ret <- function(daily.OHLC.prices,vol.alpha=0.1,signal.thresh=0.2,max.dd=0.025,dd.model) {
  
  #browser()
  
  #compute vol forecasts
  vol.gk <- ComputeCCYReturns2(daily.OHLC.prices)$Garman.Klass
  vol.forecast <- exp(smooth.vol.forecast(log(vol.gk),vol.alpha))
  
  #compute return signals
  combined <- raw.gap.returns(daily.OHLC.prices)[[2]]
  combined <- na.trim(merge(combined,vol.forecast[index(combined),]))
  names(combined) <- c("jump","returns","vol.forecast")
  combined$std.jump <- with(combined,jump/vol.forecast)
  
  #compute leverage based on drawdown model
  dd.model.summ <- summary(dd.model)
  coefs <- dd.model.summ$coefficients
  combined$max.dd.forecast <- (coefs["forecast",1]+2*coefs["forecast",2])*combined$vol.forecast +
    (coefs["jumps.std",1]+2*coefs["jumps.std",2])*abs(combined$std.jump) +
    2*dd.model.summ$sigma
  combined$max.dd.forecast <- 2*coefs["forecast",1]*combined$vol.forecast
  #combined$max.dd.forecast <- with(combined,1.15*vol.forecast+0.0035*abs(std.jump)+2*0.0040)

  positions <- xts(rep(0,nrow(combined)),index(combined))
  names(positions) <- "unlevered"
  positions[combined$std.jump > signal.thresh[2]] <- -1
  positions[combined$std.jump < signal.thresh[1]] <- 1
  positions <- positions[!(positions==0)] 
  
  combined.cut <- combined[index(combined)%in%index(positions),]
  positions$leveraged <- positions$unlevered*(max.dd/combined.cut$max.dd.forecast)

  
  #compute returns
  strat.ret <- positions$leveraged*combined.cut[,"returns"]
  return(list(strat.ret,positions,combined))
}