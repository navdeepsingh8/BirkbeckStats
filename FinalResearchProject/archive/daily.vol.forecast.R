#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
load(file.path(data.path,"EURUSD.daily.RData"))
EURUSD.daily.ret <- ComputeCCYReturns2(EURUSD.daily)

#try HW filtering with alpha between 0.001 and 0.1
alphas <- seq(0.001,0.1,length.out=100)
  hw <- lapply(alphas,function(alpha)
    HoltWinters(EURUSD.daily.ret$Abs.Ret,alpha,FALSE,FALSE))

plot(alphas,sapply(hw,"[[","SSE"),
     type="b")

#Select alpha = 0.1
fitted.hw <- hw[[which(alphas==0.1)]]

#Create vol forecast object - v_t = 1-step ahead forecast FOR date t
vol.forecast <- xts(fitted.hw$fitted[,1],
                    index(EURUSD.daily.ret)[2:length(index(EURUSD.daily.ret))])
names(vol.forecast) <- "vol.forecast"
vol.forecast <- na.trim(lag(vol.forecast,1))

#compare with original data
forecast.eval <- na.trim(merge(vol.forecast,EURUSD.daily.ret$Abs.Return))
with(forecast.eval,plot.default(vol.forecast,Abs.Return))
plot.zoo(forecast.eval)
forecast.eval$Abs.Return.Norm <- with(forecast.eval,Abs.Return/vol.forecast)
plot(forecast.eval$Abs.Return.Norm)

save(vol.forecast,file="daily.vol.forecast.RData")
