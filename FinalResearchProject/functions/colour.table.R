colour.table <- function(x,digits=3,up=1,down=-up) {
  b <- matrix("",ncol=ncol(x),nrow=nrow(x))
  dimnames(b) <- dimnames(x)
  rownames(b) <- rownames(x)
  colnames(b) <- colnames(x)
  
  b <- formatC(x,dig=digits,format="f")
  b[x>=up] <- paste("\\textcolor{green}{",
                   formatC(x[x>up], dig=digits, format="f"),
                   "}",sep="")
  b[x<=down] <- paste("\\textcolor{red}{",
                     formatC(x[x<down], dig=digits, format="f"),
                     "}",sep="")
  return(b)
}
