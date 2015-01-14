### plot 15: CN, FSSP, CDP, F300, CONCP, CONC1DC_LPI
RPlot15 <- function(data) {
  ## needs CONCN, CNTS, CONCD_LPC, CONCF_LPO, CONC3_RPO, CONCP_RPI, CONC1DC_LPI
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  # remove zeroes for log plot:
  data$CONCN[!is.na(data$CONCN) & (data$CONCN <= 0)] <- NA
  data$CONCD_LPC[!is.na(data$CONCD_LPC) & (data$CONCD_LPC <= 0)] <- NA
  data$CONCF_LPO[!is.na(data$CONCF_LPO) & (data$CONCF_LPO <= 0)] <- NA
  data$CONC3_RPO[!is.na(data$CONC3_RPO) & (data$CONC3_RPO <= 0)] <- NA
  data$CONCP_RPI[!is.na(data$CONCP_RPI) & (data$CONCP_RPI <= 0)] <- NA
  data$CONC1DC_LPI[!is.na(data$CONC1DC_LPI) & (data$CONC1DC_LPI <= 0)] <- NA
  plotWAC (data[, c("Time", "CNTS", "CONCN", "CONCP_RPI")], 
           logxy='y', ylim=c(100,1.e6), 
           ylab=expression (paste ("CONCy [cm"^"-3"*"] or CNTS [/s]")))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "CONCD_LPC", "CONCF_LPO", "CONC3_RPO", "CONC1DC_LPI")], 
           logxy='y', ylim=c(0.001,1e4), ylab=expression(paste("CONCy [cm"^"-3"*"]")))
}

