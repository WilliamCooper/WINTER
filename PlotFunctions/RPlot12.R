### Plot 12: IRU comparisons
RPlot12 <- function (data) {
  ## needs PITCH, ROLL, THDG, PITCH_IRS2, ROLL_IRS2, THDG_IRS2
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,2)+0.1)
  DF <- data[, c("Time", "PITCH", "PITCH_IRS2")]
  DF$DifferenceX50 <- (DF$PITCH - DF$PITCH_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-10.,10.), ylab="PITCH [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(-2.5,0,2.5), labels=c("-0.05", "0", "0.05"), col='red', col.axis='red')
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f +/- %.2f", 
                  mean (data$PITCH-data$PITCH_IRS2, na.rm=TRUE),
                  sd   (data$PITCH-data$PITCH_IRS2, na.rm=TRUE)))
  DF <- data[, c("Time", "ROLL", "ROLL_IRS2")]
  DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylab="ROLL [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(-2.5,0,2.5), labels=c(NA, "+/-0.05", NA), col='red', col.axis='red')
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f sd %.2f", 
                  mean (data$ROLL-data$ROLL_IRS2, na.rm=TRUE),
                  sd   (data$ROLL-data$ROLL_IRS2, na.rm=TRUE)))
  op <- par (mar=c(5,4,1,2)+0.1)
  DF <- data[, c("Time", "THDG", "THDG_IRS2")]
  DF$DifferenceX500 <- (DF$THDG - DF$THDG_IRS2) * 500 + 180
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-60,390), ylab="THDG [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(180-25, 180, 180+25), labels=c(NA, "+/-0.05", NA), col='red', col.axis='red')
  hline (180-25, 'red'); hline (180+25, 'red')
  #hline (180-125, 'green', lwd=2)
  hline (90, 'lightblue'); hline (180, 'lightblue')
  hline (270, 'lightblue'); hline (360, 'lightblue'); hline (0, 'lightblue')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
         box.col='red', text.col='red', cex=0.5)
  #title( sprintf ("mean difference: %.2f", 
  #mean (data$THDG-data$THDG_IRS2, na.rm=TRUE)))
}

