strat.perf.stats <- function(strat.ret) {

  strat.period <- as.numeric(end(strat.ret)-start(strat.ret))/260
 
  log.prices <- cumsum(as.zoo(strat.ret))
  net.high <- rollapplyr(log.prices,FUN=max,width=length(log.prices),partial=TRUE)
  drawdown <- net.high-log.prices
    
  win.pct <- 
  avg.winner <- median(strat.ret[strat.ret>0])
  avg.loser <- median(strat.ret[strat.ret<=0])
  ten.worst <- head(sort(coredata(strat.ret)),10)
  
  
  
  plot.zoo(cumsum(strat.ret),type="s",
           plot.type="single",xlab="",ylab="")
  hist(strat.ret)
  
  return(list(c(Ann.Ret=(sum(strat.ret)/strat.period),
                Worst.DD=max(drawdown),
                RD.Ratio=sum(strat.ret)/sum(drawdown),
                Win.Pct=mean(strat.ret>0),
                Profit.Factor=avg.winner/-avg.loser),
              Ten.Worst=ten.worst))

}