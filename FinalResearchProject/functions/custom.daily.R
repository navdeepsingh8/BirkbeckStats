custom.daily <- function(z,close.hour=21) {  
  #get daily end points - last time up until 20:59 on Mon-Fridays
  end.days <- daily.endpoints(z,close.hour)
  
  z.daily <- xts(cbind(period.apply(z$Open,end.days,head,1),
                             period.apply(z$High,end.days,max),
                             period.apply(z$Low,end.days,min),
                             period.apply(z$Close,end.days,tail,1),
                       Count=period.apply(z$Close,end.days,length)),
                       index(z)[end.days[2:length(end.days)]])  
  colnames(z.daily)[ncol(z.daily)] <- "Count"
  return(z.daily)
}
