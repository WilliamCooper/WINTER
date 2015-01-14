### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data) { 
  ## needs PSFD, PSFRD, QCF, QCFR, PS_A, QC_A
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  DF <- data[, c("Time", "PSFD", "PSFRD")]
  DF$PSFD <- DF$PSFD + data$QCF
  DF$PSFRD <- DF$PSFRD + data$QCFR
  DF$PSA   <- data$PS_A + data$QC_A
  colnames(DF) <- c("Time", "PtotLeft", "PtotRight", "PtotAvionics")
  plotWAC (DF, col=c('blue', 'darkgreen', 'cyan'), ylab='Ptot [hPa]')
  op <- par (mar=c(5,4,1,1)+0.1)
  DF$PtotLeft <- DF$PtotLeft-DF$PtotRight
  DF$PtotRight <- NULL
  DF$PtotAvionics <- NULL
  colnames (DF) <- c("Time", "PtotDiff")
  plotWAC (DF, ylim=c(-0.5, 0.5), ylab="Ptot(left) - Ptot(right)")
  title (sprintf ("mean difference: %.1f", 
                  mean (DF$PtotDiff, na.rm=TRUE)), cex.main=0.75)
  hline (0.2); hline (-0.2)
}

