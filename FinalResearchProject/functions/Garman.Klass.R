#Compute true range statistic for a zoo object
Garman.Klass <- function(z,jumps=TRUE) {
  #browser()
  augmented.data <- merge(z,Close.1=lag(z$Close,1))
  parkinson <- na.trim(with(augmented.data,
                            0.5*(High-Low)^2
                            -(2*log(2)-1)*(Close-Close.1)^2))
  co <- with(augmented.data,(Open-Close.1)^2)
  
  
  
  if (jumps==TRUE)
    parkinson <- parkinson+co
    
  #True.Range <- xts(apply(coredata(ranges),1,max,na.rm=TRUE),
  #                  index(ranges))
  #True.Range <- with(augmented.data,apply(merge(High,Close.1),2,max,na.rm=TRUE)-
  #                    apply(merge(Low,Close.1),2,min,na.rm=TRUE))
  #names(True.Range) <- "True.Range"
  return(parkinson)
}