setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

#Get historical data
GetCCYData("EURUSD.daily")
r <- ComputeCCYReturns2(EURUSD.daily)["2004/"]

#Set up simulation parameters
day.mesh <- 60*24/5
sim.days <- nrow(r)
sim.dates <- seq(as.Date("2001-01-01"),by=1,len=sim.days)
idx <- rep(seq(1:sim.days),rep(day.mesh,sim.days))
daily.vol <- 0.01

#Set up snowfall
require(snowfall)
sfInit( parallel=TRUE, cpus=2, type="SOCK" )
export.vars <- c("day.mesh","sim.days","sim.dates","idx","daily.vol")
sfExport(list=export.vars)
sfLibrary(data.table)
sfLibrary(xts)
sfSource("functions.R")
sfClusterSetupRNG()

wrapper <- function(indx) {
  #generate intraday returns according to GBM
  mesh.returns <- rnorm(day.mesh*sim.days,0,0.01/sqrt(day.mesh))
  #compute daily return and vol measures
  dt <- data.table(price=cumsum(mesh.returns),
                   days=idx)
  setkey(dt,"days")
  daily.series <- dt[,list(Open=head(price,1),High=max(price),Low=min(price),Close=tail(price,1)),by=days]
  daily.series.zoo <- exp(as.xts(daily.series,order.by=sim.dates)[,-1])
  ret <- na.exclude(ComputeCCYReturns2(daily.series.zoo)[,1:3])
  #return mean, sd and autocorrelations of returns
  return(c(apply(ret,2,mean),apply(ret,2,sd)))
}

sims <- 1000
result <- do.call("rbind",sfLapply(1:sims, wrapper))
sfStop()

#Examine results
hist(result[,4],30,col="lightblue",xlab="",ylab="",main="")

#return divied by return sd
hist(result[,1]/result[,4],30,col="lightblue",xlab="",ylab="",main="")
abline(v=mean(r$Return)/sd(r$Return),col="red",lwd=2)

#abs return divided by return sd
#under GBM follows half-normal distribution, E = sqrt(2/pi), V = (1-2/pi)
hist(result[,2]/result[,4],30,col="lightblue",xlab="",ylab="",main="")
mean(r$Abs.Return)/sd(r$Return)
sqrt(2/pi)

#hilo range divided by return sd
#under GBM follows a distribution noted on p62 of Parkinson
#E = sqrt(8/pi), E^2 = 4ln2
hist(result[,3]/result[,4],30,col="lightblue",xlab="",ylab="",main="")
mean(r$Hilo.Range)/sd(r$Return)
sqrt(8/pi)
mean(r$Hilo.Range^2)/var(r$Return)
4*log(2)
mean(r$Hilo.Range^2)/mean(r$Hilo.Range)^2
(4*log(2))/(8/pi)

#variability of hilorange
hist(result[,6]/result[,4],30,col="lightblue",xlab="",ylab="",main="")
sd(r$Hilo.Range)/sd(r$Return)
#it is far more variable than would be expected 
#is this because of the high serial correlation?

#compare serial correlation to simulated serial correlation

#So, simulated hilo range has lognormal density - matched in the data
# but has a far lower variance relative to daily return variance
# than what we see in the data