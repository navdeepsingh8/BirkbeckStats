realized.volatility <- function(z,min.bars=1,close.hour=21) {

  z.agg <- aggregate.bars(z,min.bars)
  
  #compute x-minute returns
  z.ret <- ComputeCCYReturns2(z.agg)
  
  #define days
  end.days <- daily.endpoints(z.ret,close.hour)
  
  #compute realized measure
  N <- (24*60)/min.bars
  rlzd <- xts(cbind(sqrt(period.apply(z.ret$Return^2,end.days,sum)),
                    period.apply(z.ret$Abs.Return,end.days,sum)/sqrt((2*N)/pi),
                    period.apply(z.ret$Hilo.Range,end.days,sum),
                    period.apply(z.ret$Return,end.days,length)),
              index(z.ret)[end.days[2:length(end.days)]])
  colnames(rlzd) <- c("Rlzd.Vol","Rlzd.Abs","Rlzd.Range","Count")
  
  #adjust for count
  
  with(rlzd,{
    Rlzd.Vol <- Rlzd.Vol*sqrt(N/Count);
    Rlzd.Abs <- Rlzd.Abs*sqrt(N/Count);
  })
  return(rlzd)
}