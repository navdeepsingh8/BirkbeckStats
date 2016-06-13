range.decompose <- function(z) {
  
  #loop thru lags
  w1 <- c(1,-1,0,0)
  w2 <- c(0,0,1,-1)
  #decomp <- list()
  #range.cov <- vector()
  #range.acov <- vector()
  for (lag in 1:1) {
    #Compute covariance matrix of components
    comps <- na.trim(with(z, merge(lag(PreUp,lag),
                               lag(PreDown,lag),
                               PostUp,
                               PostDown)))
    comps.cov <- cov(comps)
    range.acov <- t(w1) %*% comps.cov %*% w2
    #check vs cov of ranges - matches
    #range.cov[lag] <- cov(na.trim(with(z,merge(Hilo.Range,
    #                                           lag(Hilo.Range,lag)))))[1,2]
    #decomposition of range autocovariance at lag 1
    decomp <- (comps.cov[1:2,3:4]*matrix(c(1,1,1,1),nrow=2,ncol=2))/as.numeric(range.acov)
  }
  
  return(decomp)
}