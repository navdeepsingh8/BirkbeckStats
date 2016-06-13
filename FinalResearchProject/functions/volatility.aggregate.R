volatility.aggregate <- function(prices,min.bars=1,agg.frequency="weekly",FUN=mean) {
  
  #aggregate prices if needed
  if (min.bars>1) {
    prices <- to.minutes(prices,min.bars)
    names(prices) <- OHLC.labels
  }
  
  #recompute vol measure
  vol.measure <- ComputeCCYReturns2(prices)
  
  #aggregate vol measure to weekly or daily
  if (agg.frequency=="weekly") {
    #week is based on Sunday to Friday
    vol.measure <- xts(coredata(vol.measure),with_tz(index(vol.measure),tzone="Australia/Sydney"))
    aggregate.measure <- apply.weekly(vol.measure,function(x) apply(x,2,FUN))
    aggregate.measure <- xts(coredata(aggregate.measure),with_tz(index(aggregate.measure),tzone="Europe/London"))
  } else if (agg.frequency=="daily") {
    end.days.w <- daily.endpoints(vol.measure)
    aggregate.measure <- xts(period.apply(vol.measure,end.days.w,function(x) apply(x,2,FUN)),
                             index(vol.measure)[end.days.w[2:length(end.days.w)]])
  }
  return(aggregate.measure)
}
