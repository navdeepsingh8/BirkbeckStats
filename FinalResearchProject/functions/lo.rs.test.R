lo.rs.test <- function(hlr,q=0) {
  
  cs <- cumsum(hlr-mean(hlr))
  
  num <- max(cs)-min(cs)
  
  if (q==0) {
    denom <- (sd(hlr)*sqrt(length(hlr)))
  } else {
    w <- 1 - (1:q)/(q+1)
    ac <- acf(hlr,type="cov",lag.max=q,plot=FALSE)$acf[2:(q+1)]
    denom <- sqrt(length(hlr)*(var(hlr)[1]+sum(2*w*ac)))
  }
  lo.stat <- list()
  lo.stat$statistic <- num/denom
  lo.stat$p.value <- pnorm(lo.stat$statistic,sqrt(pi/2),(pi^2)/6)
  return(lo.stat)
  
}
