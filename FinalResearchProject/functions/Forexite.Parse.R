#Parse Forexite data
Forexite.Parse <- function(ccy,nrows=-1) {
  if(missing(ccy))
    stop("Must supply the name of the text file (without extension)");
  
  
  csv.path <- file.path(data.path,paste(ccy,".txt",sep=""));
  
  if(!file.exists(csv.path))
    stop(paste("File",csv.path,"does not exist."));
  
  csv.data <- read.csv(csv.path,
                       header=TRUE,sep=",",nrows=nrows,
                       col.names=c("ticker","date","time","Open","High","Low","Close","vol"),
                       colClasses=c("factor",rep("character",2),rep("double",4),"integer")
  )
  date.times <- as.POSIXct(paste(csv.data$date,csv.data$time),format="%Y%m%d %H%M%S")
  xts.gmt <- with(csv.data,xts(cbind(Open,High,Low,Close),date.times,tzone="GMT"))
  #convert to London times to deal with DST issue
  assign(ccy,xts(coredata(xts.gmt),with_tz(index(xts.gmt),"Europe/London")))
  save(list=ccy,file=file.path(data.path,paste(ccy,".RData",sep="")))
}

SaveDailyData <- function(ccy) {

  daily.varname <- paste(ccy,"daily",sep=".")
  
GetCCYData(ccy)
  
assign(daily.varname,custom.daily(get(ccy)))
save(list=daily.varname,file=file.path(data.path,paste(daily.varname,".RData",sep="")))
}

GetCCYData <- function(ccy) {
  load(file.path(data.path,paste(ccy,"RData",sep=".")),.GlobalEnv);
}

GetCCYReturns <- function(ccy) {
  load(file.path(data.path,paste(ccy,"ret","RData",sep=".")),.GlobalEnv);
}

