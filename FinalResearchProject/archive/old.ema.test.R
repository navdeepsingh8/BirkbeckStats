#Tests in this script:
# Compute PL for multiple parameterisations of the 2-EMA crossover rule
# Calculate Sharpe ratio and drawdown statistics for each one

#load packages
source("preamble.R")
source("functions.R")

#load data
load("FedData.Rdata")

#two ema crossover rule
#parameters
params <- vector("list",2)
params[[1]] <- seq(5,10,5)
params[[2]] <- seq(50,60,25)
pl <- vector("list",length(params[[1]])*length(params[[2]]))
i <- 1
for (p1 in params[[1]]) {
    for (p2 in params[[2]]) {
        pl[[i]] <- rule.pl(FedData$CAD,"twoEMA",params=c(p1,p2),return.EMA=FALSE,lag.pl=1,tc=0.002)
        i <- i+1
        cat(i/length(pl),"\t")
    }
}

#figure out date convention for FX data
#plot(cumprod(1+pl$net)-1)
#ann return, sharpe, dd ratio
#260*mean(pl)
sr <- matrix(NA,length(pl),2)
dd <- sr
for (i in 1:length(pl)) {
    sr[i,] <- sqrt(260)*apply(pl[[i]]$pl,2,mean)/apply(pl[[i]]$pl,2,sd)
    #dd[i,] <- 260*apply(pl[[i]]$pl,2,mean)/apply(pl[[i]]$pl,2,ddstat)
}

persp(params[[1]], params[[2]], matrix(sr[,2],9,9), phi = 30, theta = 45,
  xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
  main = "Surface elevation data",ticktype="detailed"
)

