### plot 19: potential-temperature plots
RPlot19 <- function (data) {
  ## needs THETA, THETAV, THETAE, THETAP, THETAQ, PSFC
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  plotWAC (data[, c("Time", "THETA", "THETAV")], ylab="potential temperatures",
           legend.position = "topright")
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "THETAE", "THETAP", "THETAQ")], 
           ylab="ad. pot. temperatures", legend.position = "top")
  mtext(paste(FigFooter,'generated by RPlot19',FigDatestr),1,outer=T,cex=0.75)
  # plots vs pressure:
  layout(matrix(2:1, ncol = 2), widths = c(5,5), heights = 1)
  op <- par (mar=c(5,2,1,1)+0.1)
  yl <- c(max (data$PSFC, na.rm=TRUE), min (data$PSFC, na.rm=TRUE))
  plot (data[, c("THETAP", "PSFC")], type='l', col='blue', 
        xlab='Ad. Pot. T. [K]', ylab='P [hPa]', xlim=c(250,350), ylim=yl)
  points (data$THETAE, data$PSFC, type='l', col='green')
  points (data$THETAQ, data$PSFC, type='l', col='red')
  legend ("topright", legend=c("THETAP", "THETAE", "THETAQ"), 
          lwd=1, col=c('blue', 'green', 'red'), cex=0.75)
  op <- par (mar=c(5,5,1,1)+0.1)
  plot (data[, c("THETA", "PSFC")], type='l', col='blue', xlab='Pot. T. [K]', 
        ylim=yl, ylab='P [hPa]')
  points (data$THETAV, data$PSFC, type='l', col='green')
  legend ("topleft", legend=c("THETA", "THETAV"), lwd=1, 
          col=c('blue', 'green'), cex=0.75)
  mtext(paste(FigFooter,'generated by RPlot19',FigDatestr),1,outer=T,cex=0.75)
}

