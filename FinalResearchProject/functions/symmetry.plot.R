symmetry.plot <- function(x,prob=0.5,...) {
  sorted.dist.data <- sort(x)
  tail.length <- floor(length(x)*prob)
  op <- par(pty="s")
  plot(head(sorted.dist.data,tail.length),
       rev(tail(sorted.dist.data,tail.length)),
       xlab="Left tail",ylab="Right tail",...)
  abline(0,-1)
  grid()
  par(op)
}