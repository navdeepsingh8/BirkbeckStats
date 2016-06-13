contingency.analysis <- function(resp,pred=resp,quants=seq(0,1,0.2),corner.size=1,pred.vol=0,pred.lag=1) {
  
  #build contingency table
  combined <- na.trim(merge(lag(pred,pred.lag),resp))
  colnames(combined) <- c("pred","resp")
  breaks.resp <- with(combined,quantile(resp,quants))
  breaks.pred <- with(combined,quantile(pred,quants))
  L <- nrow(combined)
  bucket.today <- with(combined,cut(resp,breaks.resp))
  bucket.yesterday <- with(combined,cut(pred,breaks.pred))
  contingency <- table(bucket.yesterday,bucket.today)
  
  #compute stats
  #1. Pearson stat
  stats <- list()
  expected <- L/((length(quants)-1)^2)
  scaled.contingency <- (contingency - expected)/sqrt(expected)
  
  stats[["scaled.contingency"]] <- scaled.contingency
  stats[["norm.contingency"]] <- contingency/L
  stats[["pearson"]] <- sum(scaled.contingency^2)
  stats[["pearson.pval"]] <- 1-pchisq( sum(scaled.contingency^2),df=(length(quants)-2)^2,)
  stats[["pearson.thresh"]] <- qchisq(0.95,df=(length(quants)-2)^2)
  
  #2. reversals and momentum
  middle <- (1+corner.size):(nrow(scaled.contingency)-corner.size)
  corners <- scaled.contingency[-middle,-middle]
  left <- 1:corner.size
  right <- (corner.size+1):nrow(corners)
  if (pred.vol==1) {
    #predictor is pre.up
    mr.cells <- cbind(-corners[right,left],corners[right,right])
    rev.cells <- corners[right,left]
    mom.cells <- corners[right,right]
    hetero.cells <- cbind(corners[right,left],corners[right,right])    
  } else if (pred.vol==-1) {
    #predictor is post.down
    mr.cells <- cbind(corners[left,left],-corners[left,right])
    rev.cells <- corners[left,right]
    mom.cells <- corners[left,left]
    hetero.cells <- cbind(corners[left,left],corners[left,right])
  } else {
    #predictor is return or pre.max
    mr.cells <- cbind(corners[left,left],corners[right,right],
                      -corners[left,right],-corners[right,left])
    rev.cells <- cbind(corners[right,left],corners[left,right])
    mom.cells <- cbind(corners[right,right],corners[left,left])
    hetero.cells <- corners
  }
  
  stat.names <- c("mr","rev","mom","hetero")
  for (n in stat.names) {
    stat.cells <- get(paste(n,"cells",sep="."))
    stats[[n]] <- sum(stat.cells)/sqrt(length(stat.cells))
    stats[[paste(n,"pval",sep=".")]] <- 1-pnorm(stats[[n]])
    stats[[paste(n,"thresh",sep=".")]] <- qnorm(c(0.025,0.975))
  }
  
  return(stats)
}