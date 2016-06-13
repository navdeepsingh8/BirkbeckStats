smooth.vol.forecast <- function(vol.measure,alpha=0.1) {
#  alphas <- seq(0.001,0.1,length.out=100)
#hw <- lapply(alphas,function(alpha)
hw <- HoltWinters(vol.measure,alpha,FALSE,FALSE)

#plot(alphas,sapply(hw,"[[","SSE"),
#     type="b")

#Select alpha = 0.1
#fitted.hw <- hw[[which(alphas==0.1)]]

#Create vol forecast object - v_t = 1-step ahead forecast FOR date t
vol.forecast <- xts(hw$fitted[,1],
                    index(vol.measure)[2:length(index(vol.measure))])
#names(vol.forecast) <- "vol.forecast"
return(na.trim(lag(vol.forecast,1)))

}