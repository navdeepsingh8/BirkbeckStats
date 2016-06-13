standardise <- function(z,v) {
  s <- na.trim(merge(z,v))
  s <- do.call("merge",
               lapply(1:ncol(z),function(j)
                 s[,j]/s[,ncol(s)]))
  return(s)  
}