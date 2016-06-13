first.last.minutes <- function(EURUSD,tzone=tzone(EURUSD)) {
  dt <- index(EURUSD)
  dd <- as.Date(dt,tz=tzone)
  lab <- cbind(year=year(dd),week=week(dd),weekday=wday(dd),
               minute=hour(dt)*60+minute(dt))
  
  timedate.dt <- data.table(lab,dt)
  setkeyv(timedate.dt,c("year","week","weekday"))
  
  first.min <- timedate.dt[,head(minute,1),by="year,week,weekday"]
  last.min <- timedate.dt[,tail(minute,1),by="year,week,weekday"]
  return(list(timedate.dt,first.min,last.min))
}
