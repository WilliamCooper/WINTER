require(Ranadu)
require(maps)
require(mapdata)
require(mapproj)

### configuration:
SavePlotsToFiles <- TRUE # if TRUE plots are saved to a file, and not displayed
Flight <- "rf09"
Project <- "FRAPPE"
x <- readline(sprintf("Project is %s; CR to accept or enter new project name:$", Project))
if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format):", 
                      Flight))
if (nchar(x) > 1) {Flight <- x}
print(sprintf("Flight is %s", Flight))
nplots=0
print (sprintf("Plots desired %s: CR to accept, or enter new spec", nplots))
x <- readline ("new spec: (0 => all, -1 => project set, n => plot n, can be a sequence):")
if (nchar(x) > 0) {nplots <- eval(parse(text=paste('c(',x,')', sep='')))}
print (nplots)

### get data
fname = sprintf("%s%s/%s%sR.nc", DataDirectory (), Project, Project, Flight)
print (fname)
# VarList must include all variable names that are used in this routine
VarList <- c("ACINS", "ACINS_IRS2", "ADIFR", "BDIFR", "AKRD", "SSRD", "ATHL1",
             "ATHL2", "ATRL", "AT_A", "AT_A2", "TASF", "TASR", "TAS_A", "TASX",
             "CAVP_DPT", "CAVP_DPB", "CNTS", "FCN", "FCNC", "ATX", "DPXC",
             "CONCF_LPO", "CONCD_LPC", "CONC3_RPO", "CONCN","CONCP_RPI",
             "CORAW_AL", "DBARF_LPO", "DBARD_LPC", "DBAR3_RPO", "DBAR1DC_LPI", "DBARP_RPI",
             "DP_UVH", "DP_DPB", "DP_DPT", "DPXC", "DVALUE", "EWX", "EW_DPB", 
             "EW_DPT", "EW_UVH", "FCN", "FCNC", "GGALT", "GGALT_NVTL", "PALT", 
             "ALT", "ALT_A", "ALT_A2", "GGLAT", "GGLON", "GGSPD", "GGQUAL", 
             "GGVEW", "GGVNS", "GGVEW_NVTL", "GGVNS_NVTL", "GGVSPD_NVTL",
             "GSPD", "GSPD_A", "GVEW_A", "GVNS_A", "IWD", "IWS", "WDC", "WSC",
             "LAT", "LON", "LATC", "LONC", "LAT_A", "LON_A", "MACHF", "MACHR",
             "MACH_A", "MR", "MR_UVH", "PALT_A", "PITCH",
             "ROLL", "THDG", "PITCH_IRS2", "ROLL_IRS2", "THDG_IRS2", "PLWC", "PLWCC",
             "PLWCD_LPC", "PLWCF_LPO", "PLWC1DC_LPI", "PSFD", "PSFRD", "PSFDC", "PSFC", 
             "PSXC", "QCF", "QCFC", "QCFR", "QCFRC", "QCR", "QCRC", "QC_A", "PS_A",
             "REJDOF_LPO", "REJAT_LPO", "RHODT", "RHODT_UVH", "RHUM", "RICE",
             "RSTB", "RTHL1", "RTHL2", "RTRL", "RT_A", "THETA", "THETAP",
             "THETAE", "THETAQ", "THETAV", "TKAT", "TRSTB", "TVIR", "VEW",
             "VNS", "VEWC", "VNSC", "VSPD", "VSPD_A", "WIC", "TCNTF_LPO", "FREF_LPO")

Data <- getNetCDF (fname, VarList)
# DataV: select only points where TASX > 60, and optionally limit time range
StartTime <- 0; EndTime <- 0   ## cab set start and end times for plot
DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
DataV <- DataV[DataV$TASX > 60, ]

# functions used later:
hline <- function(Time, y, col='black') {
  SE <- getStartEnd(Time)
  lines (c(Time[getIndex(Time, SE[1])], Time[getIndex (Time, SE[2])]), 
         c(y, y), col=col, lty=2)
}
formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}
testPlot <- function (k) {
  return(k %in% nplots || nplots == 0)
}

if (SavePlotsToFiles) {
  plotfile = sprintf("~/RStudio/%s/%s%sPlots.pdf", Project, Project, Flight)
  pdf (file = plotfile)
  print (sprintf ("plots saved in file %s", plotfile))
}
########################## start of plot generation ###########################
### plot 1: construct flight track with map
if (testPlot(1)) {
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotTrack(Data, .Spacing=60, .WindFlags=10)
  title (Flight)
  plotWAC(Data$Time, Data$GGALT/0.3048, ylab="Altitude [ft]")
}
### plot 2: construct one-per-hour track plots
if (testPlot(2)) {
  SE <- getStartEnd(Data$Time)
  for (hr in 0:32) {
    if (hr*10000 < SE[2] && (hr+1)*10000 > SE[1]) {
      Start <- ifelse ((hr*10000 < SE[1]), SE[1], hr*10000)
      End   <- ifelse (((hr+1)*10000 > SE[2]), SE[2], (hr+1)*10000)
      plotTrack (Data, .Range=setRange(Data$Time, Start, End), .Spacing=15, .WindFlags=10)
    }
  }
}

### plot 3: plot all temperatures, one plot
if (testPlot(3)) {
  plotWAC (DataV[, c("Time", "ATHL1", "ATHL2", "ATRL", "AT_A")],
         ylab='ATx', lty=c(1,1,2,1), lwd=c(2,1.5,1,2), 
         legend.position='bottomright')
  title(sprintf("Means L1-L2: %.2f; L2-RL: %.2f; L2-_A: %.2f", 
              mean (DataV$ATHL1-DataV$ATHL2, na.rm=TRUE), 
              mean (DataV$ATHL2-DataV$ATRL, na.rm=TRUE),
              mean (DataV$ATHL2-DataV$AT_A, na.rm=TRUE)), cex.main=0.8)
}

### plot 4: plot differences, individual pairs of temperatures
if (testPlot(4)) {
  op <- par (mar=c(5,5,2,1)+0.1)
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(5,6))
# ATHL1,2 section
  plot (DF <- DataV[, c("ATHL1", "ATHL2")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$ATHL2 <- (DataV$ATHL2 - DataV$ATHL1)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("bottomright", legend=c("red: y=(ATHL2-ATHL1)*5", 
                                  "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(ATHL2~ATHL1, data=DataV)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("ATHL2=%.3f(ATHL1)%.3f\nmean diff ATHL2-ATHL1=%.2f +/- %.2f", 
                  coef[2], coef[1], mean (DataV$ATHL2-DataV$ATHL1, na.rm=TRUE),
                  sd(DataV$ATHL2-DataV$ATHL1, na.rm=TRUE))
  } else {
    t <- sprintf ("ATHL2=%.3f(ATHL1)+%.3f\nmean diff ATHL2-ATHL1=%.2f +/-%.2f", coef[2], 
                  coef[1], mean (DataV$ATHL2-DataV$ATHL1, na.rm=TRUE),
                  sd(DataV$ATHL2-DataV$ATHL1, na.rm=TRUE))
  }
  title(t, cex.main=0.75)
  
# ATRL section:
  plot (DF <- DataV[, c("ATHL2", "ATRL")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$ATRL <- (DataV$ATHL2 - DataV$ATRL)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("bottomright", legend=c("red: y=(ATHL2-ATRL)*5", 
                                  "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(ATRL~ATHL2, data=DataV)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("ATRL=%.3f(ATHL2)%.3f\n mean diff ATRL-ATHL2=%.2f +/-%.2f", 
                  coef[2], coef[1], mean(DataV$ATRL-DataV$ATHL2, na.rm=TRUE),
                  sd(DataV$ATRL-DataV$ATHL2, na.rm=TRUE))
  } else {
    t <- sprintf ("ATRL=%.3f(ATHL2)+%.3f\n mean diff ATRL-ATHR2=%.2f +/-%.2f", coef[2], 
                  coef[1], mean(DataV$ATRL-DataV$ATHL2, na.rm=TRUE), 
                  sd(DataV$ATRL-DataV$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.75)
  
# AT_A section:
  plot (DF <- DataV[, c("ATHL2", "AT_A")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$AT_A <- (DataV$ATHL2 - DataV$AT_A)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("topleft", legend=c("red: y=(ATHL2-AT_A)*5", 
                                  "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(AT_A~ATHL2, data=DataV)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("AT_A=%.3f(ATHL2)%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", 
                  coef[2], coef[1], mean(DataV$AT_A-DataV$ATHL2, na.rm=TRUE),
                  sd(DataV$AT_A-DataV$ATHL2, na.rm=TRUE))
  } else {
    t <- sprintf ("AT_A=%.3f(ATHL2)+%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", coef[2], 
                  coef[1], mean(DataV$AT_A-DataV$ATHL2, na.rm=TRUE), 
                  sd(DataV$AT_A-DataV$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.8)
}


### plot 5: humidity
if (testPlot(5)) {
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotWAC (DF <- DataV[, c("Time", "DP_DPB", "DP_DPT", "DP_UVH")], 
           ylab='DPx', lty=c(1,1,2), lwd=c(2,1.5,1), 
           legend.position='bottomright', 
           col=c('blue', 'red', 'darkgreen'))
  title(sprintf("Means DPB-DPT: %.2f; DPB-UVH: %.2f", 
                mean (DataV$DP_DPB-DataV$DP_DPT, na.rm=TRUE), 
                mean (DataV$DP_DPB-DataV$DP_UVH, na.rm=TRUE)), cex.main=0.8)
  
  plot(DF <- DataV[, c("DP_DPB", "DP_DPT")], pch=20, col='blue', ylab='DPy')
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$DP_DPT <- DataV$DP_UVH
  # set high transparency (30) to avoid obscuring DPT by UVH
  tgreen <- rgb(0,100,0,30,maxColorValue=255)
  points (DF, pch=20, col=tgreen, cex=0.5)
  legend('bottomright', legend=c("y=DP_DPT", "y=DP_UVH"), 
         pch=20, col=c('blue', tgreen))
  title("dashed orange lines: +/-1C error bands", cex.main=0.8)
  # DP cavity pressures:
  plotWAC (DataV[, c("Time", "CAVP_DPT", "CAVP_DPB", "PSFC")], ylab='CAVP')
  # vapor pressure and mixing ratio
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (DataV[, c("Time", "EW_DPB", "EW_DPT", "EW_UVH")], ylab="EWy", 
           logxy='y', ylim=c(1e-2, 100))
  lineWAC (DataV$Time, MurphyKoop (DataV$ATX, DataV$PSFC), col='cyan', lty=2)
  title ("cyan line: equilibrium vapor pressure at ATX", cex.main=0.75)
  plotWAC (DataV[, c("Time", "MR", "MR_UVH")], ylab="MRy", 
           logxy='y', ylim=c(0.01, 100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "RHUM")], ylab="relative humidity [%]")
  hline (Data$Time, 100)
}

### plot 6: ambient pressures
if (testPlot (6)) {
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (DF <- DataV[, c("Time", "PSFD", "PSFRD")], 
           col=c('blue', 'skyblue', 'red', 'red'), ylab='PSy')
  points (DataV$Time, (DataV$PSFD-DataV$PSFRD)*50+600, type='l', col='red')
  hline (Data$Time, 500, 'red'); hline (Data$Time, 700, 'red')
  legend ("bottomleft", 
          legend=c("PSFD", "PSFRD", "(PSFD-PSFRD)*50+600", "+/-2 hPa"),
          lty=c(1,1,1,1,2), cex=0.35,
          col=c('blue', 'skyblue', 'red', 'red'))
  
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "PSFDC", "PSFC", "PS_A")], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab="PSyC")
  points (DataV$Time, (DataV$PSFDC-DataV$PSFC)*50+700, type='l', col='red')
  hline (Data$Time, 650, 'red'); hline (Data$Time, 750, 'red')
  legend ("bottomleft", 
          legend=c("PSFDC", "PSFC", "PS_A", "(PSFDC-PSFC)*50+700", "+/-1 hPa"),
          lty=c(1,1,1,1,2), cex=0.35,
          col=c('blue', 'skyblue', 'darkgreen', 'red', 'red'))
}

### plot 7: dynamic pressure; also TAS and MACH
if (testPlot(7)) {
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (DataV[, c("Time", "QCF", "QCFR", "QCR")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='QCy', 
           legend.position='bottom')
  points (DataV$Time, (DataV$QCF-DataV$QCFR)*10+80, type='l', col='red')
  hline (Data$Time, 60, col='red'); hline (Data$Time, 100, col='red')
  title("red: (QCF-QCFR)*10+80; dashed red: +/- 2 hPa [diff]", cex.main=0.85)
  
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "QCFC", "QCFRC", "QCRC", "QC_A")], col=c('blue', 'darkorange', 'darkgreen', 'brown'), ylab='QCyC',
           legend.position='bottom')
  points (DataV$Time, (DataV$QCFC-DataV$QCFRC)*10+80, type='l', col='red')
  hline (Data$Time, 100, col='red'); hline (Data$Time, 60, col='red')
  title("red: (QCFC-QCFRC)*10+80; dashed red: +/- 2 hPa [diff]", cex.main=0.85)
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "TASF", "TASR", "TAS_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='TASy', 
           legend.position='bottom')
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "MACHF", "MACHR", "MACH_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
  
}

### plot 8: total pressure (static + dynamic)
if (testPlot(8)) {
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  DF <- DataV[, c("Time", "PSFD", "PSFRD")]
  DF$PSFD <- DF$PSFD + DataV$QCF
  DF$PSFRD <- DF$PSFRD + DataV$QCFR
  colnames(DF) <- c("Time", "PtotLeft", "PtotRight")
  plotWAC (DF, col=c('blue', 'darkgreen'), ylab='Ptot')
  
  op <- par (mar=c(5,4,1,1)+0.1)
  DF$PtotLeft <- DF$PtotLeft-DF$PtotRight
  DF$PtotRight <- NULL
  colnames (DF) <- c("Time", "PtotDiff")
  plotWAC (DF, ylim=c(-0.5, 0.5), ylab="Ptot(left) - Ptot(right)")
  hline (Data$Time, 0.2); hline (Data$Time, -0.2)
}

### plot 9: wind
if (testPlot(9)) {
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # set high transparency (30) to avoid obscuring first trace
  tgreen <- rgb(0,100,0,60,maxColorValue=255)
  line.colors=c('blue', tgreen)
  line.widths <- c(1,1)
  line.types <- c(1,3)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (DataV[, c("Time", "WDC", "IWD")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  plotWAC (DataV[, c("Time", "WSC", "IWS")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "WIC")])
  title (sprintf ("flight-average vertical wind: %.02f", 
                  mean (DataV$WIC, na.rm=TRUE)), cex.main=0.75)
  hline(Data$Time, 2); hline (Data$Time, -2)
}


### plot 10: Schuler oscillation
if (testPlot(10)) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,3))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "GGVEW", "VEW")]
  DF$DifferenceX50 <- (DataV$GGVEW-DataV$VEW)*50
  DF$GGVEW_NVTL <- DataV$GGVEW_NVTL
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (Data$Time, 50, 'red')
  hline (Data$Time, -50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  DF <- DataV[, c("Time", "GGVNS", "VNS")]
  DF$DifferenceX50 <- (DataV$GGVNS-DataV$VNS)*50
  DF$GGVNS_NVTL <- DataV$GGVNS_NVTL
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (Data$Time, 50, 'red')
  hline (Data$Time, -50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- DataV[, c("Time", "GGQUAL")]
  plotWAC(DF, ylim=c(0,5))
}

### plot 11: attack, sideslip
if (testPlot (11)) {
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "AKRD", "PITCH")]
  DF$AOAREF <- DF$PITCH - (180/pi) * DataV$VSPD / DataV$TASX
  plotWAC (DF, lwd=c(2,1,1), lty=c(1,2,1))
  title (sprintf ("mean diff AKRD-AOAREF = %.02f", 
                  mean (DF$AKRD-DF$AOAREF, na.rm=TRUE)), cex.main=0.75)
  hline (Data$Time, 1); hline (Data$Time, 3)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- DataV[, c("Time", "SSRD")]
  u <- -1. * DataV$WSC * sin (DataV$WDC*pi/180)
  v <- -1. * DataV$WSC * cos (DataV$WDC*pi/180)
  DF$SSREF <- -DataV$THDG + atan2((DataV$GGVEW-u), (DataV$GGVNS-v)) * 180/pi
  DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] <- 
    DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] + 360.
  plotWAC (DF)
  title (sprintf ("mean diff SSRD-SSREF = %.02f", 
                  mean (DF$SSRD-DF$SSREF, na.rm=TRUE)), cex.main=0.75)
  hline (Data$Time, -2); hline (Data$Time, 2)
}


### Plot 12: IRU comparisons
if (testPlot (12)) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "PITCH", "PITCH_IRS2")]
  DF$DifferenceX50 <- (DF$PITCH - DF$PITCH_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-10.,10.), ylab="PITCH [deg.]",
           col=line.colors, lty=line.types)
  hline (Data$Time, -2.5, 'red'); hline (Data$Time, 2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  DF <- DataV[, c("Time", "ROLL", "ROLL_IRS2")]
  DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylab="ROLL [deg.]",
           col=line.colors, lty=line.types)
  hline (Data$Time, -2.5, 'red'); hline (Data$Time, 2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- DataV[, c("Time", "THDG", "THDG_IRS2")]
  DF$DifferenceX50 <- (DF$THDG - DF$THDG_IRS2) * 500 + 180
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-60,390), ylab="THDG [deg.]",
           col=line.colors, lty=line.types)
  hline (Data$Time, 180-25, 'red'); hline (Data$Time, 180+25, 'red')
  hline (Data$Time, 90, 'lightblue'); hline (Data$Time, 180, 'lightblue')
  hline (Data$Time, 270, 'lightblue'); hline (Data$Time, 360, 'lightblue'); hline (Data$Time, 0, 'lightblue')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
         box.col='red', text.col='red', cex=0.5)
}
  
### plot 13: IRU continued, ACINS, VSPD
if (testPlot (13)) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "ACINS", "ACINS_IRS2")]
  plotWAC (DF, ylab="ACINS")
  plotWAC (DataV[, c("Time", "VSPD", "VSPD_A", "GGVSPD_NVTL")])
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "GGALT", "GGALT_NVTL", "ALT_A", "ALT_A2")],
           legend.position = "topright")
}

### plot 14: UHSAS
# if (testPlot (14)) {
#   op <- par (mfrow=c(1,1), mar=c(4,5,2,2)+0.1)
#   plot (Time, DataV$CONCU_RWO, ylab="CONCU", type='l', log='y')
#   points (Time, DataV$CONCU100_RWO, type='l', col="green")
#   points (Time, DataV$CONCU500_RWO, type='l', col="red")
# }


### plot 15: CN, FSSP, CDP, F300, CONCP
if (testPlot (15)) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  # remove zeroes for log plot:
  DataV$CONCN[!is.na(DataV$CONCN) & (DataV$CONCN <= 0)] <- NA
  DataV$CONCD_LPC[!is.na(DataV$CONCD_LPC) & (DataV$CONCD_LPC <= 0)] <- NA
  DataV$CONCF_LPO[!is.na(DataV$CONCF_LPO) & (DataV$CONCF_LPO <= 0)] <- NA
  DataV$CONC3_RPO[!is.na(DataV$CONC3_RPO) & (DataV$CONC3_RPO <= 0)] <- NA
  DataV$CONCP_RPI[!is.na(DataV$CONCP_RPI) & (DataV$CONCP_RPI <= 0)] <- NA
  plotWAC (DataV[, c("Time", "CNTS", "CONCN")], 
           logxy='y', ylim=c(100,1.e6))
  plotWAC (DataV[, c("Time", "CONCD_LPC", "CONCF_LPO", "CONC3_RPO")], 
           logxy='y', ylim=c(0.001,1e4), ylab='CONCy')
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "CONCP_RPI")], 
           logxy='y', ylim=c(10,1e5))
}

### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
SG2 <- 61
if (testPlot (16)) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # DBAR:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "DBARD_LPC", "DBARF_LPO")]
  DF$DBARD_LPC <- signal::filter(signal::sgolay(3, SG2), DataV$DBARD_LPC)
  DF$DBARF_LPO <- signal::filter(signal::sgolay(3, SG2), DataV$DBARF_LPO)
  plotWAC (DF, ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  DF <- DataV[, c("Time", "DBAR3_RPO", "DBARP_RPI")]
  DF$DBAR3_RPO <- signal::filter(signal::sgolay(3, SG2), DataV$DBAR3_RPO)
  DF$DBARP_RPI <- signal::filter(signal::sgolay(3, SG2), DataV$DBARP_RPI)
  plotWAC (DF, ylim=c(0,2), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- DataV[, c("Time", "DBAR1DC_LPI")]
  DF$DBAR1DC_LPI <- signal::filter(signal::sgolay(3, SG2), DataV$DBAR1DC_LPI)
  plotWAC (DF)
  title ("1-min filter", cex.main=0.75)
  # PLWC:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- DataV[, c("Time", "PLWCD_LPC", "PLWCF_LPO", "PLWCC")]
  DF$PLWCD_LPC <- signal::filter(signal::sgolay(3, SG2), DataV$PLWCD_LPC)
  DF$PLWCF_LPO <- signal::filter(signal::sgolay(3, SG2), DataV$PLWCF_LPO)
  DF$PLWCC <- signal::filter(signal::sgolay(3, SG2), DataV$PLWCC)
  plotWAC (DF, ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  plotWAC (DataV[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
  hline (Data$Time, 10); hline (Data$Time, 15)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- DataV[, c("Time", "PLWC1DC_LPI")]
  DF$PLWC1DC_LPI <- signal::filter(signal::sgolay(3, SG2), DataV$PLWC1DC_LPI)
  plotWAC (DF, ylim=c(0,1))
  title ("1-min filter", cex.main=0.75)
  # more info on FSSP (REJDOF, transit time rejects, laser voltage)
  op <- par (mar=c(2,4,1,1)+0.1)
  # while there is another REJDOF_LPC for the CDP, there is no total count to normalize
  DF <- DataV[, c("Time", "REJDOF_LPO")]
  DF$DOFREJ <- 100 * DataV$TCNTF_LPO / (DataV$REJDOF_LPO + DataV$TCNTF_LPO + 0.1)
  DF$REJDOF_LPO <- NULL
  plotWAC(DF, ylab='% DOF-accepted', ylim=c(0,100))
  DF <- DataV[, c("Time", "REJAT_LPO")]
  DF$TTACC <- DataV$TCNTF_LPO / (DataV$TCNTF_LPO + DataV$REJAT_LPO + 0.1)
  DF$REJAT_LPO <- NULL
  plotWAC (DF, ylab='% TOF-accepted', ylim=c(0,100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC(DataV[, c("Time", "FREF_LPO")], ylab="FSSP laser voltage", ylim=c(4,12))
  hline (Data$Time, 8); hline (Data$Time, 10)
}

### Plot 17: all-flight Skew-T
if (testPlot (17)) {
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  DF <- DataV[, c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, AverageInterval=5)
        +ggtitle(sprintf("Flight %s, whole-flight-average", Flight)))
}
  
### plot 18: plot skew-T for individual climbs and descents:
if (testPlot (18)) {
  # search for soundings: 3 min climb/descent > tol
  del <- 180
  tol <- 3.5
  DataT <- DataV[!is.na(DataV$Time) & !is.na(DataV$GGALT), ]
  r <- 1:length(DataT$GGALT)
  for (i in (del+1):(length(DataT$GGALT)-del)) {
    if (!is.na(DataT$GGALT[i]) && abs (DataT$GGALT[i+del]-DataT$GGALT[i]) < tol*del) {
      r[i] <- NA   
    }
    if (abs (DataT$GGALT[i-del]-DataT$GGALT[i]) > tol*del) {
      r[i] <- i
    }
  }
  r[1:del] <- NA
  L <- length(DataT$GGALT)
  r[(L-del):L] <- NA
  s <- r[!is.na(r)]
  # look for breaks of at least del:
  #print (sprintf ("start at 1 %d", s[1]))
  startSounding <- 1
  startAlt <- DataT$GGALT[s[1]]
  lastAltChange <- 0
  for (j in 1:(length(s)-1)) {
    if (s[j+1]-s[j] > del) {
      #print (sprintf ("break at %d %d", j, s[j]))
      #print (sprintf ("alt change: %.1f", DataT$GGALT[s[j]]-startAlt))
      AltChange <- DataT$GGALT[s[j]] - startAlt
      if (AltChange * lastAltChange < 0) {
        DF <- DataT[s[startSounding:endSounding], c("PSFC", "ATX", "DPXC")]
        colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
        print(SkewTSounding (DF)
              +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                               formatTime(DataT$Time[s[startSounding]]),
                               formatTime(DataT$Time[s[endSounding]]))))
        startSounding <- j+1
      }
      startAlt <- DataT$GGALT[s[j+1]]
      lastAltChange <- AltChange
      endSounding <- j
    }
  }
  L <- length (s)
  #print (sprintf ("end at %d %d", L, s[L]))
  #print (sprintf (" alt change: %.1f", DataT$GGALT[L]-startAlt))
  DF <- DataT[s[startSounding:endSounding], c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF)
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  startSounding <- endSounding + 1
  endSounding <- length(s)
  DF <- DataT[s[startSounding:endSounding], c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF)
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  
# this is just to illustrate how the sounding-finder works; eventually suppress
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotWAC (DF <- DataT[, c("Time", "GGALT")])
  lines (DF[r, ], lwd=2, col='red')
  title ("red: segments selected for Skew-T soundings")
}

### plot 19: potential-temperature plots
if (testPlot (19)) {
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "THETA", "THETAV")], ylab="potential temperatures",
           legend.position = "topright")
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (DataV[, c("Time", "THETAE", "THETAP", "THETAQ")], 
           ylab="ad. pot. temperatures", legend.position = "top")
  # plots vs pressure:
  layout(matrix(2:1, ncol = 2), widths = c(5,5), heights = 1)
  op <- par (mar=c(5,2,1,1)+0.1)
  plot (DataV[, c("THETA", "PSFC")], type='l', col='blue', xlab='Pot. T. [K]', ylab='P [hPa]')
  points (DataV$THETAV, DataV$PSFC, type='l', col='green')
  legend ("bottomleft", legend=c("THETA", "THETAV"), lwd=1, col=c('blue', 'green'), cex=0.75)
  op <- par (mar=c(5,5,1,1)+0.1)
  plot (DataV[, c("THETAP", "PSFC")], type='l', col='blue', 
        xlab='Ad. Pot. T. [K]', ylab='P [hPa]', xlim=c(300,400))
  points (DataV$THETAE, DataV$PSFC, type='l', col='green')
  points (DataV$THETAQ, DataV$PSFC, type='l', col='red')
  legend ("bottomright", legend=c("THETAP", "THETAE", "THETAQ"), 
          lwd=1, col=c('blue', 'green', 'red'), cex=0.75)
}

# plot 20: CDP/SP100 size distributions
if (testPlot (20)) {
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
    if (kount >= 24)      {next}
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
  close.ncdf (netCDFfile)
}


### this section searches for speed runs and other maneuvers
del <- 75
tol <- .3
delz <- 20
DataT <- DataV[!is.na(DataV$Time) & !is.na(DataV$TASX), ]
r <- 1:length(DataT$GGALT)
for (i in (del+1):(length(DataT$GGALT)-del)) {
  if ((!is.na(DataT$TASX[i]) && abs (DataT$TASX[i+del]-DataT$TASX[i]) < tol*del) ||
        (abs(DataT$GGALT[i+del]-DataT$GGALT[i]) > delz)) {
    r[i] <- NA   
  }
  if ((abs (DataT$TASX[i-del]-DataT$TASX[i]) > tol*del) &&
        (abs(DataT$GGALT[i-del]-DataT$GGALT[i]) < delz)) {
    r[i] <- i
  }
}
r[1:del] <- NA
L <- length(DataT$GGALT)
r[(L-del):L] <- NA
s <- r[!is.na(r)]
# look for breaks of at least del:
# print (sprintf ("start at 1 %d", s[1]))
#plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
#lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
startSpeedRun <- 1
startTime <- DataT$Time[s[1]]
lastSpeedChange <- 0
for (j in 1:(length(s)-1)) {
  if (s[j+1]-s[j] > del) {
    endTime <- DataT$Time[s[j]]
    print (sprintf( "Speed run times: %s--%s", strftime(startTime, format="%H%M%S", tz='UTC'),
                    strftime (endTime, format="%H%M%S", tz='UTC')))
    startTime <- DataT$Time[s[j+1]]
  }
}

if (SavePlotsToFiles) {
  dev.off()
}

system (sprintf ("evince %s", plotfile))



