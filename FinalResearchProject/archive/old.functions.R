# Functions in this file:
# Return.calc   Calculate returns
# ddstat        Drawdown statistics
# twoEMA        Two EMA crossover rule
# rule.pl       Rule profits
#
# #calculate returns
# #needs to deal with NA prices
# Return.calc <- function(x,method="compound") {
#     #calculate numeric-to-numeric returns
#     #browser()
#     y <- na.locf(x)
#     if (method=="compound") {
#         ret <- diff(log(y))
#     } else if (method=="simple") {
#         ret <- y[2:length(y)]/lag(y,-1)-1
#     }
#     #ret <- Return.calculate(na.locf(x),...)
#     #place NA returns onto days with NA prices
#     x2 <- lag(x,-1)
#     coredata(ret)[which(is.na(x2))] <- NA
#     return(ret)
# }
# 
# #Two EMA crossover rule
# #Option to calculate EMAs of prices or returns
# twoEMA<-function(x,params=c(20,100),return.EMA=FALSE) {
#     #calculate returns if required
#     if (return.EMA)
#         x <- Return.calc(x,method="simple")
#     #x are the input values into the rule
#     #extract numeric input values y
#     y <- x[!is.na(x)]
#     #calculate signal based on y
#     mac <- EMA(y,params[1])-EMA(y,params[2])
#     sig <- ifelse(mac>0,1,-1)
#     #Use LOCF to carry over signals onto days with
#     #NA input values x and trim off leading NA signals
#     z <- merge(sig,x)
#     return(na.locf(z$sig,na.rm=TRUE))
# }
# 
# #Calculate the returns to a rule
# rule.pl <- function(x,rulefun,...,lag.pl=1,tc=0) {
#     #apply the rule to the price series to generate positions
#     sig <- match.fun(rulefun)(x,...)
#     #calculate turnover
#     turnover <- rbind(zoo(1,start(sig)),abs(diff(sig)))
#     #Rule returns = asset returns * position (appropriately lagged)
#     ret <- Return.calc(x,method="simple")
#     pl <- ret*lag(sig,-1*lag.pl)
#     #Replace NA rule returns with zeros (assume no asset return on that day)
#     pl[is.na(pl)] <- 0
#     #Apply transaction costs
#     return(list(pl=merge(gross=pl,net=pl - tc*lag(turnover,-1*lag.pl)),
#                 turnover=turnover))
# }
# 
# ddstat <- function(x) {
#     browser()
#     ddlist <- sortDrawdowns(findDrawdowns(x))
#     ddstat <- (sum(abs(ddlist$return)*(ddlist$length/260))/
#                as.numeric((end(x)-start(x))/365))
# }
