tail.plot <- function(data.to.sort,pct=0.01) { 
  #compute tails
  sorted.data <- sort(data.to.sort,decreasing=TRUE,index.return=TRUE);
  x.ind <- 1:floor(length(sorted.data$x)*pct/2)
  right.tail <- log(head(sorted.data$x,length(x.ind)))
  left.tail<-rev(log(abs(tail(sorted.data$x,length(x.ind)))))
  norm.tail <- log(qnorm(x.ind/length(data.to.sort),
                         median(data.to.sort),mad(data.to.sort),
                         lower.tail=FALSE))
  
  #plot tails
  data.to.plot <- data.frame(right.tail,left.tail,norm.tail)
  xtick <- x.ind / length(data.to.sort)
  plot(xtick,data.to.plot$right.tail,
       type="l",ylab="log(X)",xlab="Tail probability",
       ylim=c(min(sapply(data.to.plot,min)),
              max(sapply(data.to.plot,max))))
  lines(xtick,data.to.plot$left.tail,col="red")
  lines(xtick,data.to.plot$norm.tail,lty=2)
  return(sorted.data)
}