# Script: Quantile analysis of FX minute data
# Load EURUSD minute data
# Quantiles and tail thickness
#   Tail thickness is much higher than normal distribution
#   This is due to a very small number of very large outliers
# Contingency table of outlier density by weekday and hour
#   This shows the outliers mostly occur during London hours
#   with a big hotspot on Friday afternoon
# Motivates study of intra-week volatility (intra.week.vol.R)

setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")
years.to.get<-3;
EURUSD <- EURUSD[1:(60*24*5*50*years.to.get),];

#check for missing data
apply(is.na(EURUSD),2,sum)

#check for outliers
diffs <- diff(EURUSD)
returns <- diff(log(EURUSD$Close))
p <- c(0.005,0.025,0.25,0.5,0.75,0.975,0.995);
quantile(diffs$Close,probs=p)
quantile(returns,probs=p)
#99% of the moves are between -8 and 8 pips or -6 and 6bp

#Plot the tails - compared to normal distribution
#sorted.diffs <- tail.plot(coredata(diffs$Close),0.01)
sorted.returns <- tail.plot(coredata(returns),0.01)


#Looking at the biggest 
#isolate the biggest observations
#that go off the charts compared to the rest
#These are the biggest 0.1% of returns - above 13bps
sorted.abs.returns <- sort(abs(coredata(returns)),decreasing=TRUE,index.return=TRUE);
biggest.returns <- lapply(sorted.abs.returns,head,floor(2*0.0005*length(sorted.abs.returns$x)));
str(biggest.returns)
plot(biggest.returns$x)
tail(biggest.returns$x,1)


#Now let's check where in the week these occur
#The FX market runs 24 hours from Sun night to Friday night
#but different regions dominate trading depending on the time
# Sydney / Tokyo: 10pm - 9am
# London: 8am - 6pm
# NYC: 1pm - 11pm
biggest.returns$dates <- index(diffs)[biggest.returns$ix];
biggest.returns$days <- weekdays(biggest.returns$dates,abbreviate=TRUE);
biggest.table <- table(biggest.returns$days,hour(biggest.returns$dates))/length(biggest.returns$x)
biggest.table <- biggest.table[ordered.weekdays,]
print(biggest.table)

pdf(file="output/EURUSD.outliers.pdf",paper="a4r")
temp <- tail.plot(coredata(returns),0.01)
abline(v=0.0005)
for (day in ordered.weekdays) {
  print(barchart(biggest.table[day,],xlim=c(0,max(biggest.table)),main=day,
                 xlab="Proportion",ylab="Starting hour"))
}
dev.off()
