### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data) { 
  ## needs QCF, QCFR, QCR, QCFC, QCFRC, QCRC, QC_A, TASF, TASFR, 
  ## TASR, TAS_A, TASHC, MACHF, MACHFR, MACHR, MACH_A
  op <- par (mar=c(2,4,1,2)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (data[, c("Time", "QCF", "QCFR", "QCR")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='QCy [hPa]', 
           legend.position='bottom')
  points (data$Time, (data$QCF-data$QCFR)*10+80, type='l', col='red')
  axis (4, at=c(60,80,100), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (60, col='red'); hline (100, col='red')
  legend("bottomleft", legend=c("red: (QCF-QCFR)*10+80", 
                                "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  op <- par (mar=c(5,4,1,2)+0.1)
  plotWAC (data[, c("Time", "QCFC", "QCFRC", "QCRC", "QC_A")], 
           col=c('blue', 'darkorange', 'darkgreen', 'brown'), ylab=' corrected QCyC [hPa]',
           legend.position='bottom')
  points (data$Time, (data$QCFC-data$QCFRC)*10+80, type='l', col='red')
  axis (4, at=c(60,80,100), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (100, col='red'); hline (60, col='red')
  legend("bottomleft", c("red: (QCFC-QCFRC)*10+80", 
                         "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  title (sprintf ("mean difference QCFC-QCFRC=%.1f;  QC_A-QCFRC=%.1f",
                  mean (data$QCFC-data$QCFRC, na.rm=TRUE),
                  mean (data$QC_A-data$QCFRC, na.rm=TRUE)), cex.main=0.75)
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (data[, c("Time", "TASF", "TASFR", "TASR", "TAS_A", "TASHC")], 
           col=c('blue', 'lightblue', 'darkorange', 'darkgreen', 'red'), ylab='TASy [m/s]', 
           legend.position='bottom')
  title (sprintf ("diff vs TASF: %.1f (TASFR), %.1f (TASR), %.1f (TAS_A), %.1f (TASHC)",
                  mean (data$TASFR-data$TASF, na.rm=TRUE), 
                  mean (data$TASR-data$TASF, na.rm=TRUE), 
                  mean (data$TAS_A-data$TASF, na.rm=TRUE),
                  mean (data$TASHC-data$TASF, na.rm=TRUE)), cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "MACHF", "MACHFR", "MACHR", "MACH_A")], 
           col=c('blue', 'lightblue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
}

