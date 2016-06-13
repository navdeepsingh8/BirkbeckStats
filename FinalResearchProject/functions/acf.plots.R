acf.plots <- function(data,lag.max=NULL,...) {
  #op <- par(mfrow=c(2,1),mar=c(4,4,3,2)+0.1)
  acf.obj <- acf(data,lag.max,plot=FALSE)
  min.max <- quantile(with(acf.obj,acf[2:length(acf)]),c(0,1))
  min.max[1] <- min(0,min.max[1])
  plot(acf.obj,ylim=min.max,...)
  #grid(col="gray")
  #acf(data,lag.max,type="partial",...)
  #grid()
  #par(op)
}
