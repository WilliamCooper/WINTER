### plot 20: CDP/SP100 size distributions
RPlot20 <- function (data) {
  ## needs CCDP_LPC, CS100_LPT; references fname from calling environment
  kount = 0
  netCDFfile = open.ncdf(fname)
  CCDP <- get.var.ncdf(netCDFfile, "CCDP_LPC")
  CFSSP <- get.var.ncdf (netCDFfile, "CS100_LPT")
  Time <- get.var.ncdf (netCDFfile, "Time")
  TASX <- get.var.ncdf(netCDFfile, "TASX")
  time_units <- att.get.ncdf (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  CellSizes <- att.get.ncdf (netCDFfile, "CCDP_LPC", "CellSizes")
  CellLimitsD <- CellSizes$value
  CellSizes <- att.get.ncdf (netCDFfile, "CS100_LPT", "CellSizes")
  CellLimitsF <- CellSizes$value
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  ## yes, I know, bad-practice-reference to calling environment for StartTime
  ifelse (StartTime > 0, jstart <- getIndex(Time, StartTime), jstart <- 1)
  # print (sprintf ("start time in RPlot20 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1)
  for (j in jstart:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 90)) {next}
    if (kount >= 24) {break}
    CDP <- CCDP[,j]
    FSSP <- CFSSP[,j]
    ## convert distributions to number per cm per um
    for (m in 2:length(CDP)) {
      CDP[m] <- CDP[m] / (CellLimitsD[m] - CellLimitsD[m-1])
    }
    for (m in 2:length(FSSP)) {
      FSSP[m] <- FSSP[m] / (CellLimitsF[m] - CellLimitsF[m-1])
    }
    CDP[CDP <= 0] <- 1e-4
    FSSP[FSSP <= 0] <- 1e-4
    if ((any(CDP > 1, na.rm=TRUE)) || (any(FSSP > 1, na.rm=TRUE))) {
      kount <- kount + 1
      ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
              op <- par (mar=c(5.2,2,1,1)+0.1))
      plot (CellLimitsD, CDP, type='s', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
      points (CellLimitsF, FSSP, type='s', col='darkgreen')
      title(sprintf("Time=%s", strftime (Time[j], format="%H:%M:%S", tz='UTC')), 
            cex.main=.75)
      legend ("topright", legend=c("CDP", "FSSP"), col=c('blue', 'darkgreen'), 
              lwd=c(2,1), cex=0.75) 
    }
  }
  Z <- close.ncdf (netCDFfile)
}

