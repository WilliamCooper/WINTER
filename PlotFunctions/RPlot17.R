### Plot 17: all-flight Skew-T
RPlot17 <- function (data) {
  ## needs PSFC, ATX, DPXC
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  DF <- data[, c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, AverageInterval=5, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, whole-flight-average", Flight)))
}

