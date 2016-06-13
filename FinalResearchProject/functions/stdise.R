stdise <- function(z,divisor=1,thresh=3) {
  std.z <- z/divisor
  
  loc <- median(std.z)
  scale <- IQR(std.z)/1.34
  std.z.norm <- (std.z-loc)/scale
  
  outliers <- abs(std.z.norm)>thresh
  std.z.winzd <- std.z
  std.z.winzd[outliers] <- loc+thresh*scale*sign(std.z.norm[outliers])  
  return(std.z.winzd)
}