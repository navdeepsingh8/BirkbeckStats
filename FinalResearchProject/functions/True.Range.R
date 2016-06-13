#Compute true range statistic for a zoo object
True.Range <- function(z) {
  #browser()
  augmented.data <- merge(z,Close.1=lag(z$Close,1))
  ranges <- na.trim(with(augmented.data,abs(merge(High-Low,High-Close.1,Close.1-Low))))
  names(ranges) <- c("High-Low","High-P.Close","P.Close-Low")
  True.Range <- xts(apply(coredata(ranges),1,max,na.rm=TRUE),
                    index(ranges))
  #True.Range <- with(augmented.data,apply(merge(High,Close.1),2,max,na.rm=TRUE)-
  #                    apply(merge(Low,Close.1),2,min,na.rm=TRUE))
  names(True.Range) <- "True.Range"
  return(True.Range)
}