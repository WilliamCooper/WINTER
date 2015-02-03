### plot 22: UHSAS size distributions
RPlot22 <- function (data) {
  ## needs CUHSAS_RPC; references fname from calling environment
  kount = 0
  netCDFfile = open.ncdf(fname)
  Time <- get.var.ncdf(netCDFfile, "Time")
  TASX <- get.var.ncdf(netCDFfile, "TASX")
  CUHSAS <- get.var.ncdf (netCDFfile, "CUHSAS_RPC")
  CPCASP <- get.var.ncdf (netCDFfile, "CS200_RPT")
  time_units <- att.get.ncdf (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  CellSizes <- att.get.ncdf (netCDFfile, "CUHSAS_RPC", "CellSizes")
  CellLimitsU <- CellSizes$value
  CellSizes <- att.get.ncdf (netCDFfile, "CS200_RPT", "CellSizes")
  CellLimitsP <- CellSizes$value
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  op <- par (mar=c(2,2,1,1)+0.1)
  for (j in 1:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 90)) {next}
    if (kount >= 24) {next}
    UHSAS <- CUHSAS[, j]
    PCASP <- CPCASP[, j]
    UHSAS[UHSAS <= 0] <- 1e-4
    PCASP[PCASP <= 0] <- 1e-4
    if ((any(UHSAS > 50, na.rm=TRUE)) && (any(PCASP > 1, na.rm=TRUE))) {
      kount <- kount + 1
      ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
              op <- par (mar=c(5.2,2,1,1)+0.1))
      plot (CellLimitsU, UHSAS, type='s', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
      points (CellLimitsP, PCASP, type='s', col='darkgreen')
      title(sprintf("UHSAS, Time=%s", strftime (Time[j], format="%H:%M:%S", tz='UTC')), 
            cex.main=.75)
      legend ("topright", legend=c("UHSAS", "PCASP"), col=c('blue', 'darkgreen'), 
              lwd=c(2,1), cex=0.75) 
    }
  }
  Z <- close.ncdf (netCDFfile)
}

