#Developed currencies
idx.dev <- c("GBP","EUR","CHF","NOK","SEK","DKK","AUD","NZD","JPY","SGD","CAD")
idx.subdev <- c("GBP","CHF","NOK","AUD","CAD","SGD")
FedData <- FedData.All[,idx.dev]



#Data availability stats
trimmed.prices <- lapply(FedData,na.trim)
lapply(lapply(trimmed.prices,function(x) rbind(start(x),end(x))),as.Date)
sapply(trimmed.prices,length)
sapply(sapply(trimmed.prices,is.na),sum)

#Display dates with missing data for GBP
index(trimmed.prices$GBP[is.na(trimmed.prices$GBP)])

#save data to a file
save(idx.dev,idx.subdev,FedData,trimmed.prices,ret,ret.cum,file="FedData.Rdata")
