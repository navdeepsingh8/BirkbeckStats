#Compute the intraweek seasonality in the vol measure of choice
week.seasonality <- function(vol.measure) {
  
  #browser()
  #Compute the location and scale for each week in the sample  
  #use Sydney times so that the week starts in the right place
  print("Computing weekly location and scales...")
  original.index <- index(vol.measure)
  vol.measure <- xts(coredata(vol.measure),with_tz(index(vol.measure),tzone="Australia/Sydney"))
  year.week.idx <- factor(paste(year(index(vol.measure)),
                                sprintf("%02d",week(index(vol.measure))),
                                sep="."),
                          ordered=TRUE);
  year.week.mean <- tapply(vol.measure,year.week.idx,mean);
  year.week.sd <- tapply(vol.measure,year.week.idx,sd);
  
  
  #Now normalise every minute by it's weekly location and scale
  print("Standardising each data point by it's weekly location and scale...")
  vol.norm <- vol.measure;
  for (i in levels(year.week.idx)) {
    vol.norm[year.week.idx==i] <- 
      (vol.measure[year.week.idx==i]-coredata(year.week.mean[i]))/
      coredata(year.week.sd[i]);  
  }
  #Back to London times for the final numbers
  vol.norm <- xts(coredata(vol.norm),original.index)
  
  
  
  #Finally for each of the (60*24*5) minutes in the week,
  #compute the mean and quantiles of the standardised ATR estimates
  print("Computing location and scale of standardised data...")
  day.idx <- factor(weekdays(index(vol.norm),abbreviate=TRUE));
  time.idx <- factor(paste(sprintf("%02d",hour(index(vol.norm))),
                           sprintf("%02d",minute(index(vol.norm))),sep=":"));
  vol.norm.median <- tapply(coredata(vol.norm),list(time.idx,day.idx),median,na.rm=TRUE)
  vol.norm.conf <- list(upper=tapply(coredata(vol.norm),list(time.idx,day.idx),quantile,probs=0.975,na.rm=TRUE),
                        lower=tapply(coredata(vol.norm),list(time.idx,day.idx),quantile,probs=0.025,na.rm=TRUE))
  #Stitch them all together - do this more concisely later
  vol.norm.median.cat <- vector();
  for (day in ordered.weekdays[ordered.weekdays %in% dimnames(vol.norm.median)[[2]]]) {
    vol.norm.median.cat <- c(vol.norm.median.cat,vol.norm.median[,day])
  }
  return(list(year.week.mean=year.week.mean,
              year.week.sd=year.week.sd,
              intra.week.median=vol.norm.median,
              intra.week.quant=vol.norm.conf,
              intra.week.median.cat=vol.norm.median.cat))
}
