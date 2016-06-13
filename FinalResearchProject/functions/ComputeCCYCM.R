#CM = contingency measures
ComputeCCYCM <- function(ccy) {
  logccy <- log(ccy)
  cmccy <- na.trim(merge(PreUp=with(logccy,Close-Low),
                         PreDown=with(logccy,Close-High),
                         PostUp=logccy$High-lag(logccy$Close,1),
                         PostDown=logccy$Low-lag(logccy$Close,1),
                         PreUpO=with(logccy,High-Open),
                         PreDownO=with(logccy,Low-Open)))
  names(cmccy) <- c("PreUp","PreDown","PostUp","PostDown","PreUpO","PreDownO")
  
  
  max.measure <- function(moves) {
    which.max.moves <- apply(abs(coredata(moves)),1,which.max)
    max.moves <- sapply(1:nrow(moves),function(r)
      moves[r,which.max.moves[r]])
    return(xts(max.moves,index(moves)))
  }
  
  cmccy$PreMax <- max.measure(with(cmccy,merge(PreUp,PreDown)))
  cmccy$PostMax <- max.measure(with(cmccy,merge(PostUp,PostDown)))
  
  return(cmccy)
}