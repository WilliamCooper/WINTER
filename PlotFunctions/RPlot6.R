### plot 6: ambient pressures
RPlot6 <- function (data) { 
  ## needs PSFD, PSFRD, PSFDC, PSFC, PS_A
  op <- par (mar=c(2,4,1,2)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (DF <- data[, c("Time", "PSFD", "PSFRD")], 
           col=c('blue', 'skyblue', 'red', 'red'), ylab='pressure  PSy [hPa]')
  points (data$Time, (data$PSFD-data$PSFRD)*50+600, type='l', col='red')
  axis (4, at=c(500,600,700), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (500, 'red'); hline (700, 'red')
  legend ("bottomleft", 
          legend=c("(PSFD-PSFRD)*50+600", "+/-2 hPa"),
          lty=c(1,2), cex=0.75,
          col=c('red', 'red'))
  op <- par (mar=c(5,4,1,2)+0.1)
  plotWAC (data[, c("Time", "PSFDC", "PSFC", "PS_A")], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab="corrected PSyC [hPa]")
  points (data$Time, (data$PSFC-data$PSFDC)*50+700, type='l', col='red')
  axis (4, at=c(650,700,750), labels=c("-1", "0", "1"), col='red', col.axis='red')
  hline (650, 'red'); hline (750, 'red')
  legend ("bottomleft", 
          legend=c("(PSFC-PSFDC)*50+700", "+/-1 hPa"),
          lty=c(1,2), cex=0.75,
          col=c('red', 'red'))
  title (sprintf ("mean difference PSFC-PSFDC: %.1f;  PS_A-PSFC: %.1f", 
                  mean (data$PSFC-data$PSFDC, na.rm=TRUE),
                  mean (data$PS_A-data$PSFDC, na.rm=TRUE)), cex.main=0.75)
}

