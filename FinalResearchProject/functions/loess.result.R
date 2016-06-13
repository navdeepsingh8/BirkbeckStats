loess.result <- function(z,resp,pred,lag=1) {
  a <- na.trim(with(z,merge(lag(get(pred),lag),get(resp))))
  lo <- loess(as.formula(paste(resp,pred,sep="~")),data=a)
  loess.plot(lo)
}