### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data) {
  ## needs DBARD_LPC, DBARF_LPO, DBAR3_RPO, DBARP_RPI, DBAR1DC_LPI
  ## PLWCF_LPO, PLWCD_LPC, PLWC, PLWCC, PLWC1DC_LPI
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # DBAR:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "DBARD_LPC", "DBARF_LPO")]
  DF$DBARD_LPC <- SmoothInterp(data$DBARD_LPC)
  DF$DBARF_LPO <- SmoothInterp(data$DBARF_LPO)
  plotWAC (DF, ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  DF <- data[, c("Time", "DBAR3_RPO", "DBARP_RPI")]
  DF$DBAR3_RPO <- SmoothInterp(data$DBAR3_RPO)
  DF$DBARP_RPI <- SmoothInterp(data$DBARP_RPI)
  plotWAC (DF, ylim=c(0,2), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "DBAR1DC_LPI")]
  DF$DBAR1DC_LPI <- SmoothInterp(data$DBAR1DC_LPI)
  plotWAC (DF)
  title ("1-min filter", cex.main=0.75)
  # PLWC:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWCD_LPC", "PLWCF_LPO", "PLWCC")]
  DF$PLWCD_LPC <- SmoothInterp(data$PLWCD_LPC)
  DF$PLWCF_LPO <- SmoothInterp(data$PLWCF_LPO)
  DF$PLWCC <- SmoothInterp(data$PLWCC)
  plotWAC (DF, ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
  hline (10); hline (15)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWC1DC_LPI")]
  DF$PLWC1DC_LPI <- SmoothInterp(data$PLWC1DC_LPI)
  plotWAC (DF, ylim=c(0,1))
  title ("1-min filter", cex.main=0.75)
  # more info on FSSP (REJDOF, transit time rejects, laser voltage)
  op <- par (mar=c(2,4,1,1)+0.1)
  # while there is another REJDOF_LPC for the CDP, there is no total count to normalize
  DF <- data[, c("Time", "REJDOF_LPO")]
  DF$DOFREJ <- 100 * data$TCNTF_LPO / (data$REJDOF_LPO + data$TCNTF_LPO + 0.1)
  DF$REJDOF_LPO <- NULL
  plotWAC(DF, ylab='% DOF-accepted', ylim=c(0,100))
  DF <- data[, c("Time", "REJAT_LPO")]
  DF$TTACC <- data$TCNTF_LPO / (data$TCNTF_LPO + data$REJAT_LPO + 0.1)
  DF$REJAT_LPO <- NULL
  plotWAC (DF, ylab='% TOF-accepted', ylim=c(0,100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC(data[, c("Time", "FREF_LPO")], ylab="FSSP laser voltage", ylim=c(4,12))
  hline (8); hline (10)
}

