#Load data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
#Daily returns and vol measures
load(file.path(data.path,"EURUSD.daily.RData"))
daily.vol <- ComputeCCYReturns2(EURUSD.daily)
#Realized vol measures
load("vol.aggregate.ljung.box.RData")
realized.vol <- v$daily$"5"

#test for long memory
lo.rs.test(realized.vol$Hilo.Range,200)
acf(coredata(realized.vol$Hilo.Range),lag.max=1000)

#Vol forecasts
calc.vol.forecasts <- function(z) {
  s <- lapply(1:ncol(z),function (x) smooth.vol.forecast(z[,x]))
  s <- merge(s[[1]],s[[2]],s[[3]],s[[4]])
  names(s) <- names(z)
  return(s)
}

daily.forecasts <- calc.vol.forecasts(daily.vol)
realized.forecasts <- calc.vol.forecasts(realized.vol)

#Create standardised measures

all.forecasts <- na.trim(merge(daily.forecasts[,2:3],
                               realized.forecasts[,2:3]))
names(all.forecasts)[3:4] <- paste("Rlzd.",names(all.forecasts)[1:2],sep="")

all.measures <- na.trim(merge(daily.vol[,2:3],realized.vol[,2:3]))
names(all.measures)[3:4] <- paste("Rlzd.",names(all.forecasts)[1:2],sep="")

std.measures <- list()
std.names <- vector()
for (m in 1:ncol(all.measures)) {
  for (f in 1:ncol(all.forecasts)) {
    std.measures[[length(std.measures)+1]] <- 
      standardise(all.measures[,m],
                  all.forecasts[,f])
    std.names <- c(std.names,paste(names(all.forecasts)[f],
                                   names(all.measures)[m],
                                   sep="."))
  }
}
std.measures <- do.call("merge",std.measures)
names(std.measures) <- std.names

# Plot acfs of the standardised measures
lapply(1:ncol(std.measures),function(x) 
  acf.plots(coredata(std.measures[,x]),lag.max=250,
            main=colnames(std.measures)[x]))

acf(std.measures$Abs.Return.Rlzd.Hilo.Range,lag.max=1000)
lo.rs.test(std.measures$Abs.Return.Rlzd.Hilo.Range,20)

