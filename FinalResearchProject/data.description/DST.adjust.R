setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")

start.min <- first.last.minutes(dt)
timedate.dt <- start.min[[1]]
first.min <- start.min[[2]]
last.min <- start.min[[3]]
first.min.table <- table(first.min$weekday,first.min$V1)
last.min.table <- table(last.min$weekday,last.min$V1)

print.start.end.table <- function(table,row) {
  table.out <- t(as.matrix(table[row,][table[row,]!=0]))
  rownames(table.out) <- ordered.weekdays[row]
  mins <- as.numeric(colnames(table.out))
  colnames(table.out) <- paste(mins%/%60,sprintf("%02i",mins%%60),sep=":")
  return(table.out)
}

head(EURUSD,15)

print.xtable(xtable(print.start.end.table(first.min.table,1),
       caption="Table of counts of first minute of the week, in London times",
       label="tab:first.quotes"),
      file=file.path(report.path,"table.first.last.quotes.tex"))
             
print.xtable(xtable(print.start.end.table(last.min.table,6),
                    caption="Table of counts of last minute of the week, in London times",
                    label="tab:last.quotes"),
             file=file.path(report.path,"table.first.last.quotes.tex"),
             append=TRUE)
