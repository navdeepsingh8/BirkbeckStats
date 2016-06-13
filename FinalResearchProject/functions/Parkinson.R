#Compute true range statistic for a zoo object
Parkinson <- function(z,jumps=TRUE,vol=TRUE,gk.estimator=FALSE) {
  #browser()
  augmented.data <- merge(z,Close.1=lag(z$Close,1))
  if (gk.estimator==TRUE) {
    parkinson <- with(augmented.data,
                              0.5*(High-Low)^2
                              -(2*log(2)-1)*(Close-Open)^2)
  } else {
    parkinson <- with(augmented.data,(High-Low)^2/(4*log(2)))
  }
  co <- with(augmented.data,(Open-Close.1)^2)
  
  if (vol==TRUE) {
    #parkinson <- parkinson*ifelse(gk.estimator==TRUE,1.034,pi*log(2)/2)
    #co <- co*pi/2
    #browser()
    if (jumps==TRUE)
      parkinson <- parkinson+co
    
    parkinson <- sqrt(parkinson)
  } else {
    if (jumps==TRUE)
      parkinson <- parkinson+co
  }
  
  
  return(na.trim(parkinson))
}