setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")

result.nothing <- lapply(1:100, function(s) simulate(vol.model=FALSE,vol.seasonality=FALSE,jumps=FALSE))
result.vol.model <- lapply(1:100, function(s) simulate(vol.seasonality=FALSE,jumps=FALSE))
result.vol.seasonality <- lapply(1:100, function(s) simulate(vol.model=FALSE,jumps=FALSE))
result.jumps <- lapply(1:100, function(s) simulate(vol.model=FALSE,vol.seasonality=FALSE))
result.everything <- lapply(1:100, function(s) simulate())

save(list=ls()[grep("^result",ls())],file=file.path(code.path,"simulation.results.RData"))


make.perf.tables(result.nothing,"nothing",
                 caption="constant daily volatility, constant intraday volatility and no Monday jumps")
make.perf.tables(result.vol.model,"vol.model",
                 caption="time-varying daily volatility, constant intraday volatility and no Monday jumps")
make.perf.tables(result.vol.seasonality,"vol.seasonality",
                 caption="constant daily volatility, seasonal intraday volatility and no Monday jumps")
make.perf.tables(result.jumps,"jumps",
                 caption="constant daily volatility, constant intraday volatility and with Monday jumps")
make.perf.tables(result.everything,"everything",
                 caption="time-varying daily volatility, seasonal intraday volatility and with Monday jumps")

#Plot process cond vol and prices
s <- simulate()
pdf(file=file.path(report.path,"figures","sim-vol.pdf"),width=12)
plot.zoo(s[[3]][,1],xlab="",ylab="",xaxt="n")
dev.off()
pdf(file=file.path(report.path,"figures","sim-price.pdf"),width=12)
plot.zoo(s[[4]]$Close,xlab="",ylab="",xaxt="n")
dev.off()
