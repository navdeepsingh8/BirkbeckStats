setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD")

sample.table <- xtable(as.data.frame(head(EURUSD["2013-04-22 08",],15)),digits=4,
                       caption="Sample of 15 minutes of raw data from 8AM on 22/04/2013",
                       label="tab:sample.table")

print.xtable(sample.table,table.placement="h",
      file=file.path(report.path,"tables","sample.table.tex"))
