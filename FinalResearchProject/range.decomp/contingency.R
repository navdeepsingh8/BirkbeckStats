#Grab data
setwd("~/Google Drive/Study/Applied Mathematics/MSc Statistics/Dissertation/Code")
source("functions.R")
GetCCYData("EURUSD.daily")
EURUSD.daily <- EURUSD.daily["2004/"]
EURUSD.daily.ret <- ComputeCCYReturns2(EURUSD.daily)

#Build custom measures
EURUSD.daily.cm <- ComputeCCYCM(EURUSD.daily)
EURUSD.daily.merged <- na.trim(merge(EURUSD.daily.ret,EURUSD.daily.cm))

#Scatter plots of contemporaneous custom measures
pdf(file=file.path(report.path,"figures","pre-scatter.pdf"))
par(pty="s")
with(EURUSD.daily.merged,plot.default(PreDown,PreUp))
abline(a=0,b=-1,col="navy",lty=2)
grid()
dev.off()

pdf(file=file.path(report.path,"figures","post-scatter.pdf"))
par(pty="s")
with(EURUSD.daily.merged,plot.default(PostDown,PostUp))
grid()
abline(h=0,v=0,col="red",lty=2)
abline(a=0,b=-1,col="navy",lty=2)
dev.off()

pdf(file=file.path(report.path,"figures","premax-hist.pdf"))
plot(histogram(coredata(EURUSD.daily.merged$PreMax),
               nint=30,xlab="",ylab=""))
dev.off()

pdf(file=file.path(report.path,"figures","postmax-hist.pdf"))
plot(histogram(coredata(EURUSD.daily.merged$PostMax),
               nint=30,xlab="",ylab=""))
dev.off()

pdf(file=file.path(report.path,"figures","return-premax.pdf"))
par(pty="s")
with(EURUSD.daily.merged,plot.default(Return,PreMax,
                                      xlim=0.04*c(-1,1),ylim=0.04*c(-1,1)))
grid()
abline(h=0,v=0,col="red",lty=2)
abline(a=0,b=1,col="red",lty=2)
abline(a=0,b=-1,col="red",lty=2)
dev.off()
par(op)


#Normalise measures by vol forecasts
standardising <- FALSE
# standardising <- TRUE
# EURUSD.daily.merged <- standardise(EURUSD.daily.merged,
#                 smooth.vol.forecast(EURUSD.daily.merged$Abs.Return,0.1))

#build out predictor-response combinations
#contingency.combo <- as.matrix(expand.grid(c("Return","PostMax"),
#                                           c("PreUp","PreDown"),
#                                           stringsAsFactors=FALSE))
contingency.combo <- rbind(rep("Return",2),
                           c("Return","PreMax"),
                           c("PostMax","PreMax"))#,
#contingency.combo)
#tab.ref <- paste("\\ref{tab:",sapply(1:nrow(contingency.combo), function(i) paste(contingency.combo[i,2],
#                                                   contingency.combo[i,1],
#                                                   sep=".")),"}",sep="")
#contingency.combo <- cbind(contingency.combo,tab.ref)
colnames(contingency.combo) <- c("Response","Predictor")

#print combinations to table
print.xtable(xtable(contingency.combo),
             include.rownames=FALSE,
             file=file.path(report.path,"tables","contingency.combo.tex"))

#build contingency tables and compute statistics
contingency.results <- list()
corner.size <- 4
for (i in 1:dim(contingency.combo)[1]) {
  response <- contingency.combo[i,1]
  predictor <- contingency.combo[i,2]
  result.name <- paste(predictor,response,sep=".")
  if (grepl("Up",predictor)) {
    y.vol <- 1
  } else if (grepl("Down",predictor)) {
    y.vol <- -1
  } else {
    y.vol <- 0
  }
  contingency.results[[result.name]] <-
    with(EURUSD.daily.merged,
         contingency.analysis(get(response),get(predictor),
                              pred.lag=1,
                              quants=seq(0,1,0.1),
                              corner.size=corner.size,
                              pred.vol=y.vol))
}

#print contingency tables
if (standardising!=TRUE) {
  sc.tables.range <- lapply(contingency.results,"[[","scaled.contingency")
  lapply(1:length(sc.tables.range),function(x) 
    pretty.print.sc(sc.tables.range[[x]],names(sc.tables.range)[x],1))
}

#print table of statistics

extract.stats <- c("pearson","pearson.pval","rev","rev.pval","mom","mom.pval")
table.pval <- sapply(1:length(extract.stats),function(i) 
  sapply(contingency.results,"[[",extract.stats[i]))
colnames(table.pval) <- c("$\\chi^2$","$P(>\\chi^2)$",
                          paste("$REV(",corner.size,")$",sep=""),
                          paste("$P(>REV(",corner.size,"))$",sep=""),
                          paste("$MOM(",corner.size,")$",sep=""),
                          paste("$P(>MOM(",corner.size,"))$",sep=""))
xtable.pval <- xtable(table.pval,
                      caption=paste("Pearson, $REV$ and $MOM$ statistics and their $p$-values"
                                    ,ifelse(standardising,"adjusted for heteroskedasticity",""),sep=" "),
                      label=paste("tab:contingency.pval",ifelse(standardising,".std",""),sep=""))
align(xtable.pval) <- c("r|rr|rr|rr")
print.xtable(xtable.pval,
             file=file.path(report.path,"tables",
                            paste("contingency.pval",
                                  ifelse(standardising,".std",""),".tex",sep="")),
             sanitize.text.function=function(x) {x})


# #write statistics and contingency tables to Latex
# hetero.table <- cbind(
#   sapply(contingency.results$Return.Return,"[[","hetero.pval")[1:10],
#   sapply(contingency.results$PreMax.Return,"[[","hetero.pval")[1:10],
#   sapply(contingency.results$PreUp.Return,"[[","hetero.pval")[1:10],
#   sapply(contingency.results$PreDown.Return,"[[","hetero.pval")[1:10])
# colnames(hetero.table) <- c("Return.Return","PreMax.Return","PreUp.Return","PreDown.Return")
# print.xtable(xtable(hetero.table,digits=3,
#                     caption=paste("$VJ(",corner.size,")$ and $VJ_a(",corner.size,")$ statistic $p$-values at daily lags",sep=""),
#                     label="tab:hetero.pval"),
#              file=file.path(report.path,"table.sc.hetero.pval.tex"))
# 
# pearson.table <- t(sapply(1:10,function(x) sapply(lapply(contingency.results,"[[",x),"[[","pearson.pval")))
# print.xtable(xtable(pearson.table,digits=3,
#                     label="tab:pearson.pval",
#                     caption="Pearson statistic $p$-values at daily lags"),
#              file=file.path(report.path,"table.sc.pearson.pval.tex"))
# 
# mr.table <- t(sapply(1:10,function(x) sapply(lapply(contingency.results,"[[",x),"[[","reversal.pval")))
# print.xtable(xtable(mr.table,digits=3,
#                     label="tab:mr.pval",
#                     caption=paste("$MR(",corner.size,")$"," statistic $p$-values at daily lags",sep="")),
#              file=file.path(report.path,"table.sc.mr.pval.tex"))
