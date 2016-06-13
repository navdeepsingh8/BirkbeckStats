simulate <- function(vol.model=TRUE,vol.seasonality=TRUE,jumps=TRUE) {
  
  #Get vol seasonality function
  load("intra.day.vol.RData")
  
  #Set up simulation parameters
  day.mesh <- 60*24
  sim.weeks <- 52*10
  sim.dates <- as.Date("2004-01-05")+sort(c(sapply(0:4,function(n) seq(from=n,by=7,length.out=sim.weeks))))
  sim.days <- length(sim.dates)
  min.idx <- rep(1:day.mesh,sim.days)
  day.idx <- rep(seq(1:sim.days),rep(day.mesh,sim.days))
  start.vol <- 0.0060
  arma.param <- c(0.9934,-0.8981,-5.2341,0.1136)
  exp.mean <- 0.14
  vol.measures <- c("Abs.Return","Parkinson","Parkinson.2","Garman.Klass")
  #Set up output vectors
  cond.vol <- c(start.vol,rep(NA,sim.days-1))
  jump.vol <- rep(0,sim.days)
  intra.day.returns <- rep(NA,length(day.idx))  
  
  #Simulate from here
  
  #1. generate conditional vol  
  if (vol.model==TRUE) {
    
    for (day in 1:sim.days) {            
      if (day>1) {
        cond.vol[day] <- exp(arma.param[3] + 
                               arma.param[1]*(log(cond.vol[day-1])-arma.param[3]) + 
                               arma.param[2]*rnorm(1,0,arma.param[4]))
      }
    } 
  } else {
    cond.vol[2:length(cond.vol)] <- start.vol
  }
  
  #2. compute jump vol and adjust cond vol if needs be
  if (jumps == TRUE) {
    num.jumps <- sim.days %/% 5
    if (vol.model==TRUE) {
      f <- rexp(num.jumps,1/exp.mean)
      f <- sapply(1:num.jumps,function(i) min(f[i],0.8))
    } else {
      f <- rep(exp.mean,num.jumps)
    }
    jump.grid.daily <- seq(from=1,by=5,length.out=num.jumps)
    jump.vol[jump.grid.daily] <- cond.vol[jump.grid.daily]*sqrt(f)
    cond.vol[jump.grid.daily] <- sqrt(cond.vol[jump.grid.daily]^2-jump.vol[jump.grid.daily]^2)
  }
  
  #3. simulate intraday returns
  cond.vol.grid <- rep(cond.vol,rep(day.mesh,sim.days))  
  if (vol.seasonality==TRUE) {
    intra.day.returns <- rnorm(day.mesh*sim.days,0,sqrt(cond.vol.grid^2*rep(spl.intra.day.vol$y,sim.days)))
  } else {
    intra.day.returns <- rnorm(day.mesh*sim.days,0,sqrt(cond.vol.grid^2*(1/day.mesh)))
  }
  #3b. Add jumps if needed
  if (jumps==TRUE) {
  jump.grid.intra.day <- min.idx==1 
  intra.day.returns[jump.grid.intra.day] <- 
    intra.day.returns[jump.grid.intra.day] + rnorm(sim.days,0,jump.vol)
  }

#Collect true vol process
true.vol <- xts(sqrt(cond.vol^2+jump.vol^2),sim.dates)

#Store intraday returns in data.table
dt <- data.table(ret=intra.day.returns,
                 price=cumsum(intra.day.returns),
                 mins=min.idx,
                 days=day.idx)
setkeyv(dt,c("days","mins"))

#Compute realized vol measure using 30 minute returns
sub.min <- 30
dt[,subsample:=rep(rep(1:(day.mesh/sub.min),rep(sub.min,day.mesh/sub.min)),sim.days)]
sub.ret <- dt[,sum(ret),by=c("days","subsample")]
dt.rv <- sub.ret[,sqrt(sum(V1^2)),by=days]
rv <- as.xts(dt.rv$V1,sim.dates)

#Compute daily returns and vol measures
daily.series <- dt[,list(Open=head(price,1),High=max(price),Low=min(price),Close=tail(price,1)),by=days]
daily.series.zoo <- exp(as.xts(daily.series,order.by=sim.dates))[,-1]
ret <- ComputeCCYReturns2(daily.series.zoo)[,vol.measures]

#Compute performance stats
vols <- na.trim(merge(true.vol,ret,Rlzd.Vol=rv))
bias <- apply(vols,2,function(f) mean(f-vols$true.vol))[-1]
mape <- bias/mean(vols$true.vol)
se <- apply(vols,2,sd)[-1]
eff <- se[1]^2/se^2  
lb <- sapply(apply(vols,2,Box.test,lag=60,type="Ljung-Box"),"[[","statistic")
lb.ratio <- lb[-1]/lb[1]  
vol.cor <- cor(vols)
return(list(rbind(mape,se,eff,lb.ratio),vol.cor,vols,daily.series.zoo))
}


make.perf.tables <- function(results,label,caption,rows=c(1,2,3,4)) {
  stats <- lapply(results,"[[",1)
  S <- stats[[1]]
  perf.mean <- matrix(sapply(1:length(S),
                             function(s) mean(sapply(stats,"[",s))),nrow(S),ncol(S))
  perf.sd <- matrix(sapply(1:length(S), 
                           function(s) sd(sapply(stats,"[",s))),nrow(S),ncol(S))
  perf.table <- perf.mean[rows,]
  rownames(perf.table) <- c("MAPE","Std. Dev.","Efficiency","LB ratio")[rows]
  digs <- rbind(rep(4,ncol(S)+1),
                rep(4,ncol(S)+1),
                rep(1,ncol(S)+1),
                rep(2,ncol(S)+1))
  colnames(perf.table) <- c("$\\widehat\\sigma_A$","$\\widehat\\sigma_P$","$\\widehat\\sigma_{P*}$",
                            "$\\widehat\\sigma_{GK*}$","$\\widehat\\sigma_{RV}$")  
  print.xtable(xtable(perf.table,digits=digs[rows,],
                      label=paste("tab:sim.perf",label,sep="."),
                      caption=paste("Simulation performance for estimators with",caption)),
               sanitize.text.function=function(x) {x},
               file=file.path(report.path,"tables",paste("sim.perf",label,"tex",sep=".")))
  
  cors <- lapply(results,"[[",2)
  C <- cors[[1]]
  cors.mean <- matrix(sapply(1:length(C),
                             function(c) mean(sapply(cors,"[",c))),nrow(C),ncol(C))
  cors.mean[lower.tri(cors.mean,diag=TRUE)] <- NA
  colnames(cors.mean) <- c("$\\sigma_t$",colnames(perf.table))
  rownames(cors.mean) <- colnames(cors.mean)
  print.xtable(xtable(cors.mean,digits=2,
                      label=paste("tab:sim.cor",label,sep="."),
                      caption=paste("Mean correlation between volatility estimates for simulation with",caption)),
               sanitize.text.function=function(x) {x},
               file=file.path(report.path,"tables",paste("sim.cor",label,"tex",sep=".")))
  
}