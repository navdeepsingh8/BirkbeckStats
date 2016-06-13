daily.endpoints <- function(z,close.hour=21) {
  ix <- index(z)
  dd <- as.Date(ix,tz=tzone(z))
  dt <- data.table(date=dd,
                   wday=wday(dd),
                   minute=hour(ix)*60+minute(ix),
                   index=1:length(ix))
  setkeyv(dt,c("date","wday","minute","index"))
  #end points are the last minute before the close hour on each day
  dt3 <- dt[wday >=2 & wday <= 6 & minute<=close.hour*60,max(index),by=date]
  end.days <- dt3$V1
  return(end.days)
}