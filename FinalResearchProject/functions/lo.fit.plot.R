lo.fit.plot <- function(y,x,...) {
  lo <- loess(y~x)
  x.grid <- seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=100)
  lo.pred <- predict(lo,x.grid,se=TRUE)
  plot(coredata(merge(x,y)),pch="+",col="gray",
       xlim=quantile(x,c(0,1),na.rm=TRUE),
       ylim=quantile(y,c(0.05,0.95),na.rm=TRUE),...)
  lines(x.grid,lo.pred$fit,type="l",col="red")
  lines(x.grid,lo.pred$fit+2*lo.pred$se.fit,lty=2,col="red")
  lines(x.grid,lo.pred$fit-2*lo.pred$se.fit,lty=2,col="red")
  rug(x)
  abline(h=0); grid()
}