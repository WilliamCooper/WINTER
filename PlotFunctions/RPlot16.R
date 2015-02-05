### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data) {
  ## needs DBARD_LPC, DBARF_LPT, DBARP_RPT, DBAR1DC_LPB, DBARU_RPC
  ## PLWCF_LPT, PLWCD_LPC, PLWC, PLWCC, PLWC1DC_LPB
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # DBAR:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "DBARD_LPC", "DBARF_LPT")]
  DF$DBARD_LPC <- SmoothInterp(data$DBARD_LPC)
  DF$DBARF_LPT <- SmoothInterp(data$DBARF_LPT)
  plotWAC (DF, ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  #DF <- data[, c("Time", "DBAR3_RPO", "DBARP_RPT")]
  DF <- data[, c("Time", "DBARP_RPT", "DBARU_RPC")]
  #DF$DBAR3_RPO <- SmoothInterp(data$DBAR3_RPO)
  DF$DBARP_RPT <- SmoothInterp(data$DBARP_RPT)
  DF$DBARU_RPC <- SmoothInterp(data$DBARU_RPU)
  plotWAC (DF, ylim=c(0,2), ylab="DBARP and DBARU", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "DBAR1DC_LPB")]
  DF$DBAR1DC_LPB <- SmoothInterp(data$DBAR1DC_LPB)
  plotWAC (DF)
  title ("1-min filter", cex.main=0.75)
  # PLWC:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWCD_LPC", "PLWCF_LPT", "PLWCC")]
  DF$PLWCD_LPC <- SmoothInterp(data$PLWCD_LPC)
  DF$PLWCF_LPT <- SmoothInterp(data$PLWCF_LPT)
  DF$PLWCC <- SmoothInterp(data$PLWCC)
  plotWAC (DF, ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
  hline (10); hline (15)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWC1DC_LPB")]
  DF$PLWC1DC_LPB <- SmoothInterp(data$PLWC1DC_LPB)
  plotWAC (DF, ylim=c(0,1))
  title ("1-min filter", cex.main=0.75)
  # more info on FSSP (REJDOF, transit time rejects, laser voltage)
  op <- par (mar=c(2,4,1,1)+0.1)
  # while there is another REJDOF_LPC for the CDP, there is no total count to normalize
  DF <- data[, c("Time", "REJDOF_LPT")]
  DF$DOFREJ <- 100 * data$TCNTF_LPT / (data$REJDOF_LPT + data$TCNTF_LPT + 0.1)
  DF$REJDOF_LPT <- NULL
  plotWAC(DF, ylab='% DOF-accepted', ylim=c(0,100))
  DF <- data[, c("Time", "REJAT_LPT")]
  DF$TTACC <- data$TCNTF_LPT / (data$TCNTF_LPT + data$REJAT_LPT + 0.1)
  DF$REJAT_LPT <- NULL
  plotWAC (DF, ylab='% TOF-accepted', ylim=c(0,100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC(data[, c("Time", "FREF_LPT")], ylab="FSSP laser voltage", ylim=c(4,12))
  hline (8); hline (10)
}

