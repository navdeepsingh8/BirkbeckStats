#Trim outliers
trim.outliers <- function(x,num.sd=4,sd.fun=mad,return.idx=FALSE) {
  loc <- median(x);
  scale <- sd.fun(x);
  thresh <- num.sd*scale;
  y <- (x-loc)/scale
  idx.replace <- (y > num.sd)|(y < -num.sd)
  x[idx.replace] <- sign(y[idx.replace])*thresh;
  
  return(list(x,idx.replace))
}