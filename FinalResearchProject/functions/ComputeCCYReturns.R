ComputeCCYReturns <- function(ccy,first=NULL) {
  GetCCYData(ccy) ;
  #Truncate if needed
  if (!is.null(first))
    assign(ccy,get(ccy)[(1:first),]) 
  #Compute log prices, returns and ranges  
  retccy <- paste("ret",ccy,sep="")
  logccy <- log(get(ccy))
  #Close to close returns
  assign(retccy,
         merge(Return=diff(logccy$Close),
               Hilo.Range=with(logccy,High-Low),
               True.Range=True.Range(logccy)))
  #ignore first row with the NA return on it
  assign(retccy,get(retccy)[2:dim(get(retccy))[1],])
  save(list=retccy,file=file.path(data.path,paste(ccy,"ret","RData",sep=".")))
}