col2alpha <- function(color,alpha) {
  rgb.values <- col2rgb(color)
  color <- rgb(rgb.values[1], rgb.values[2], rgb.values[3], alpha=alpha, maxColorValue=255)
  return(color)
}
