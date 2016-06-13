#Now plot the results
seasonality.plots <- function(l,title) {
  
  pdf(file=paste("output/EURUSD.intra.week.vol",title,"pdf",sep="."),paper="a4r")
  attach(l)
  
  plot(year.week.mean,type="l",axes=FALSE,
       xlab="year.week",ylab="vol measure median")
  ticks <- seq(from=1,to=length(year.week.mean),length.out=5)
  axis(side=1,at=ticks,labels=names(year.week.mean)[ticks])
  axis(side=2)
  grid()
  plot(year.week.sd,type="l",axes=FALSE,
       xlab="year.week",ylab="vol measure sd")
  axis(side=1,at=ticks,labels=names(year.week.sd)[ticks])
  axis(side=2)
  grid()
  hour.tick <- (1:dim(intra.week.median)[1])/60;
  for (day in ordered.weekdays[ordered.weekdays %in% dimnames(intra.week.median)[[2]]]) {
    plot(hour.tick,intra.week.median[,day],main=day,xlab="hour",ylab="vol.measure.norm",type="l",
         ylim=c(min(intra.week.quant[[2]],na.rm=TRUE),
                quantile(intra.week.quant[[1]],0.95,na.rm=TRUE)))
    lines(hour.tick,intra.week.quant[[1]][,day],lty=2,col="green")
    lines(hour.tick,intra.week.quant[[2]][,day],lty=2,col="red")
  }
  day.tick <- (1:length(intra.week.median.cat))/(60*24)
  plot(day.tick,intra.week.median.cat,type="l",
       main="Week",ylab="vol.measure.norm",xlab="day")
  detach(l)
  dev.off()
}
