### plot 5: humidity
RPlot5 <- function (data) { 
  ## needs DP_DPB, DP<DPT, DP_VXL, EW_DPT, EX_VXL, MR, ATX, 
  ## PSFC, CAVP_DPB, CAVP_DPT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotWAC (DF <- data[, c("Time", "DP_DPB", "DP_DPT", "DP_VXL", "ATX")], 
           ylab=expression (paste ("dew point  DPy  [", degree, "C]")), 
           lty=c(1,1,2,1), lwd=c(2,1.5,1,3), legend.position='bottomright', 
           col=c('blue', 'red', 'darkgreen', 'black'))
  title(sprintf("Means DPB-DPT: %.2f; DPB-VXL: %.2f", 
                mean (data$DP_DPB-data$DP_DPT, na.rm=TRUE), 
                mean (data$DP_DPB-data$DP_VXL, na.rm=TRUE)), cex.main=0.8)
  plot(DF <- data[, c("DP_DPB", "DP_DPT")], pch=20, col='blue', 
       ylab=expression (paste ("dew point  DPy  [", degree, "C]")))
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$DP_DPT <- data$DP_VXL
  # set high transparency (30) to avoid obscuring DPT by VXL
  tgreen <- rgb(0,100,0,30,maxColorValue=255)
  points (DF, pch=20, col=tgreen, cex=0.5)
  legend('bottomright', legend=c("y=DP_DPT", "y=DP_VXL"), 
         pch=20, col=c('blue', tgreen))
  title("dashed orange lines: +/-1C error bands", cex.main=0.8)
  # DP cavity pressures:
  plotWAC (data[, c("Time", "CAVP_DPT", "CAVP_DPB", "PSFC")], 
           lwd=c(2,1,1), lty=c(1,2,1), ylab='CAVP')
  title (sprintf ("mean above PSFC: %.1f (DPB) and %.1f (DPT)", 
                  mean (data$CAVP_DPB - data$PSFC, na.rm=TRUE),
                  mean (data$CAVP_DPT - data$PSFC, na.rm=TRUE), cex.main=0.75))
  # vapor pressure and mixing ratio
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (data[, c("Time", "EW_DPB", "EW_DPT", "EW_VXL")], ylab="EWy [hPa]", 
           logxy='y', ylim=c(1e-2, 100))
  lineWAC (data$Time, MurphyKoop (data$ATX, data$PSFC), col='cyan', lty=2)
  title ("cyan line: equilibrium vapor pressure at ATX")
  plotWAC (data[, c("Time", "MR")], ylab="mixing ratio [g/kg]",
           logxy='y', ylim=c(0.01, 100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "RHUM")], ylab="relative humidity [%]")
  hline (100)
}

