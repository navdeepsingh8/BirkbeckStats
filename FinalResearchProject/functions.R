#load packages
require(lattice)
require(xts)
require(quantmod)
require(lubridate)
require(tseries)
require(xtable)
require(data.table)

#set global variables and paths
data.path <- file.path("C:","Users","Navdeep","Documents","Market Data");
disso.path <- file.path("C:","Users","Navdeep","Google Drive","Study","Applied Mathematics",
"MSc Statistics","Dissertation")
code.path <- file.path(disso.path,"code")
function.path <- file.path(code.path,"functions")
report.path <- file.path(disso.path,"doc","report")
present.path <- file.path(disso.path,"doc","presentation")
OHLC.labels <- c("Open","High","Low","Close")
ordered.weekdays <- c("Sun","Mon","Tue","Wed","Thu","Fri")

#load all custom functions
for (nm in list.files(function.path,pattern="[.][Rr]$"))  
  source(file.path(function.path,nm))




















