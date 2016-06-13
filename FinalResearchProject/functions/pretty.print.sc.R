pretty.print.sc <- function(a,name,digits) {
  rownames(a) <- 1:nrow(a)
  colnames(a) <- 1:ncol(a)
  print.xtable(xtable(colour.table(a,digits=digits),
                      caption=name,
                      label=paste("tab:",name,sep="")),
               file=tolower(file.path(report.path,"tables",paste("sc",name,"tex",sep="."))),
               sanitize.text.function = function(x){x})
}