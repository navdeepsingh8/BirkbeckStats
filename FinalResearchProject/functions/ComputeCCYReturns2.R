ComputeCCYReturns2 <- function(ccy) {
  
  #browser()
  logccy <- log(ccy)
  #Close to close returns
  Returns <- diff(logccy$Close)
  #Risk measures
  retccy <- merge(Returns,
                  abs(Returns)*sqrt(pi/2),
                  Returns^2,
                  Hilo.Range=with(logccy,High-Low),
                  True.Range=True.Range(logccy),
                  Parkinson=Parkinson(logccy,jumps=FALSE),
                  Parkinson.2=Parkinson(logccy),
                  
                  Garman.Klass=Parkinson(logccy,gk.estimator=TRUE))
  names(retccy) <- c("Return","Abs.Return","Sqr.Return","Hilo.Range",
                     "True.Range","Parkinson","Parkinson.2",
                     "Garman.Klass")
  return(retccy[2:dim(retccy)[1],])
}
