aggregate.bars <- function(z,min.bars) {
  
  if (min.bars >1) {
    z.head <- head(z,2*min.bars-1)
    z.start <- which(minute(z.head)[-1] %% min.bars == 0)+1
    z <- to.minutes(z[-(1:(z.start-1)),],
                    min.bars)
    colnames(z) <- OHLC.labels
  }
  return(z)
  
}