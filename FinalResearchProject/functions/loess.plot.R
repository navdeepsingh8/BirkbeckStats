loess.plot <- function(lo) {
  with(lo,plot.default(x,y,
                       xlim=quantile(x,c(0.005,0.995)),
                       ylim=quantile(y,c(0.005,0.995))))
  new.data <- with(lo,seq(quantile(x,0.01),quantile(x,0.99),length.out=100))
  predicted <- predict(lo,newdata=new.data,se=TRUE)
  lines(new.data,predicted$fit,col="red")
  lines(new.data,with(predicted,fit+2*se.fit),col="red",lty=3)
  lines(new.data,with(predicted,fit-2*se.fit),col="red",lty=3)
  grid()
}