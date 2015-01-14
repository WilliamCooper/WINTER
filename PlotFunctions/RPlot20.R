### plot 20: CDP/SP100 size distributions
RPlot20 <- function (data) {
  ## needs CCDP_LPC, DS100_LPO; references fname from calling environment
  kount = 0
  netCDFfile = open.ncdf(fname)
  CCDP <- get.var.ncdf(netCDFfile, "CCDP_LPC")
  CFSSP <- get.var.ncdf (netCDFfile, "CS100_LPO")
  CellSizes <- att.get.ncdf (netCDFfile, "CCDP_LPC", "CellSizes")
  CellLimitsD <- CellSizes$value
  CellSizes <- att.get.ncdf (netCDFfile, "CS100_LPO", "CellSizes")
  CellLimitsF <- CellSizes$value
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  op <- par (mar=c(2,2,1,1)+0.1)
  for (j in 1:length(Data$Time)) {
    if (is.na(Data$Time[j])) {next}
    if (kount >= 24) {next}
    CDP <- CCDP[,j]
    FSSP <- CFSSP[,j]
    CDP[CDP <= 0] <- 1e-4
    FSSP[FSSP <= 0] <- 1e-4
    if ((any(CDP > 1, na.rm=TRUE)) || (any(FSSP > 1, na.rm=TRUE))) {
      kount <- kount + 1
      ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
              op <- par (mar=c(5.2,2,1,1)+0.1))
      plot (CellLimitsD, CDP, type='s', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
      points (CellLimitsF, FSSP, type='s', col='darkgreen')
      title(sprintf("Time=%s", strftime (Data$Time[j], format="%H:%M:%S", tz='UTC')), 
            cex.main=.75)
      legend ("topright", legend=c("CDP", "FSSP"), col=c('blue', 'darkgreen'), 
              lwd=c(2,1), cex=0.75) 
    }
  }
  Z <- close.ncdf (netCDFfile)
}

