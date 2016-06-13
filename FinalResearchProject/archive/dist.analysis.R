#Prelim data loading
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYReturns("EURUSD")

#Convert to XTS for now (until we re-parse the Forexite data)
retEURUSD <- as.xts(retEURUSD)

#Downsample returns for 
#ECDF and QQ plots
dist.downscale <- 5
dist.data <- coredata(retEURUSD$Return[seq(from=1,by=dist.downscale,to=dim(retEURUSD)[1])])

#Produce plots and stats
#Save plots to PDF
pdf(file="output/EURUSD.1.min.dist.pdf",paper="a4r")

#ECDF plot
plot(ecdf(dist.data),
     main="EURUSD 1-minute")
abline(v=c(min(dist.data),max(dist.data)),lty=2,col=c("red","green"))
grid()
#highly peaked distribution
#discontinuity at zero return
#very long tails both sides

#Density esimate
EURUSD.density <- density(coredata(retEURUSD$Return),bw=0.0001/6)
plot(EURUSD.density)
#discontinuities are clear in the centre

#QQ plot
qqnorm(dist.data,pch=".")
qqline(dist.data)
grid()
#very fat tails both sides compared to normal distribution
#right tail looks fatter than left tail

#Tail symmetry plot - confirms right tail fatter than left
symmetry.plot(coredata(retEURUSD$Return),0.5,
              main="Tail symmetry plot",
              type="l",col="red")

#Independence? Check for autocorrelation
acf.plots(coredata(retEURUSD$Return),
          main="EURUSD 1-minute returns")
acf.plots(abs(coredata(retEURUSD$Return)),
          main="EURUSD 1-minute absolute returns")
acf.plots(coredata(retEURUSD$Hilo.Range),
          main="EURUSD 1-minute range")
acf.plots(coredata(retEURUSD$True.Range),
          main="EURUSD 1-minute true range")
#Very weak dependence in raw returns (MA(1)-like)
#Long-range dependence in absolute returns and ranges
#Shows that returns are certainly not independent

#Close PDF device
dev.off()

#Stats now
abs.values <- c(1e-2,1e-3,1e-4,1e-5)
probs <- sapply(abs.values, function(x) mean(abs(retEURUSD$Return) < x))
names(probs) <- paste("< |",as.character(abs.values),"|",sep="")
print(probs)
#The last number indicates the number of zero returns
#because the minimum tick size is USD0.0001


#Jarque-Bera test - assumes iid returns
jarque.bera.test(retEURUSD$Return)
#normality is rejected outright

#Autocorrelation tests:
lb.test <- apply(merge(retEURUSD,Abs.Return=abs(retEURUSD$Return)),2,Box.test,lag=60,type="Ljung-Box")
sapply(lb.test,"[[","p.value")

lb.test.values <- t(matrix(sapply(lb.test,"[[","statistic")))
colnames(lb.test.values) <- c("$R_t$","$HLR_t$","$TR_t$","$|R_t|$")
x.lb.test.values <- xtable(lb.test.values,
                           digits=0)
print(x.lb.test.values,
      file=file.path(report.path,"table.lb.test.tex"),
      sanitize.text.function = function(x){x},
      include.rownames=FALSE)
#Bai and Ng (2005) have a modified JB test for weakly dependent data
#Richardson and Smith (1993) take a different approach
#Vavra (2011) suggests another alternative in her PhD thesis


