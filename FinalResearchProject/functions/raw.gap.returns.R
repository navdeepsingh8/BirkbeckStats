raw.gap.returns <- function(daily.OHLC.prices) {
  #compute weekend gaps and monday returns
  augmented.daily <- na.trim(merge(daily.OHLC.prices,with(daily.OHLC.prices,lag(merge(High,Low,Close),1))))
  combined <- with(augmented.daily,log(merge(Open/Close.1,Close/Open)))[wday(index(augmented.daily))==2]
  
  #simple strat: fade the gap until the close of the day
  #no position sizing, no trade filtering, no exit filtering
  strat.ret <- -sign(combined[,1])*combined[,2]
  return(list(strat.ret,combined))
  
}