#closeDay can be 0 to 5
custom.week <- function(y,closeDay=1) {
  #First convert to Australian time   
  y.aus <- xts(coredata(y),with_tz(index(y),tzone="Australia/Sydney"))
  
  if (closeDay==0) {
    #Sun to Fri week
    y.period <- to.period(y.aus,period='weeks',k=1,indexAt="startof")
    names(y.period) <- OHLC.labels
  }  else {  
    #Weekday week end
    #find endpoints
    ends.y <- endpoints(y.aus,on="days",1)
    ends.y <- ends.y[ends.y != 0]
    w <- weekdays(index(y.aus)[ends.y],abbreviate=TRUE)
    ends.week <- ends.y[grep(ordered.weekdays[1+closeDay],w)]
    
    #create xts obj
    y.period <- xts(cbind(period.apply(y.aus$Open,ends.week,function(x) x[1]),
                    period.apply(y.aus$High,ends.week,max),
                    period.apply(y.aus$Low,ends.week,min),
                    period.apply(y.aus$Close,ends.week,function(x) x[length(x)])),
              index(y.aus)[ends.week[2:length(ends.week)]])
  }
  #and then back to London times
  rm(y.aus)
  y.period <- xts(coredata(y.period),with_tz(index(y.period),tzone="Europe/London"))
  return(y.period)
}
