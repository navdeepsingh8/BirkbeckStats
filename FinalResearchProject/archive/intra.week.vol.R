# Script: Intra-week volatility seasonality
# At the minute level there is a clear volatility seasonality within week
# Taking the average true range for 1 minute bars as my volatility measure
# I estimate the extent of this seasonality and plot it
# Results show the most volatile times during the day are
# 1. A peak at 10AM
# 2. A higher peak at around 5PM 
# There is a sharp pick up volatility from 7AM in the morning,
# and a gradual drop off after the 5PM peak, reaching pre-7AM levels
# by midnight
# There is a very slight hump in volatility during Asian trading
# Also, volatility increases through the week, reaching a crescendo
# on Friday afternoon
#and Sun night 10pm - Mon morning 1am
#Chances are liquidity also follows this pattern


#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYReturns("EURUSD")

#convert to xts for now
retEURUSD <- as.xts(retEURUSD,tz="Europe/London")

#compute seasonality stats - takes a while
seasonality.absret <- week.seasonality(abs(retEURUSD$Return))
seasonality.tr <- week.seasonality(retEURUSD$True.Range)
seasonality.hilorange <- week.seasonality(retEURUSD$Hilo.Range)

#save to file
save(seasonality.absret,
     seasonality.tr,
     seasonality.hilorange,
     file="EURUSD.seasonality.RData")

#load("EURUSD.seasonality.RData")
seasonality.plots(seasonality.absret,"absolute.return")
seasonality.plots(seasonality.tr,"true.range")
seasonality.plots(seasonality.hilorange,"hilo.range")
