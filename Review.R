
## ----initialization,echo=FALSE,include=FALSE-----------------------------

library(knitr)
opts_chunk$set(echo=FALSE, include=TRUE, fig.lp="fig:")
opts_chunk$set(fig.width=5.5, fig.height=7, fig.align="center", 
               digits=4, fig.show="asis")
thisFileName <- "Review"
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(maps, quietly=TRUE)
require(mapdata, quietly=TRUE)
require(mapproj, quietly=TRUE)
require(ggplot2)
require(grid)
require(ggthemes)



## ----configuration, include=TRUE-----------------------------------------

SavePlotsToFiles <- FALSE # if TRUE plots are saved to a file, and not displayed
nps <- 20
Flight <- "rf09"
Project <- "FRAPPE"
# x <- readline(sprintf("Project is %s; CR to accept or enter new project name: ", Project))
# if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format): ", 
                      Flight))
if (nchar(x) > 1) {Flight <- x}
print(sprintf("Flight is %s", Flight))
nplots=c(1, 3:17, 19:20)
print ("Plots desired: CR to accept, or enter new spec")
x <- readline ("new spec: (0 => all, -1 => project set, n => plot n, can be a sequence): ")
if (nchar(x) > 0) {nplots <- eval(parse(text=paste('c(',x,')', sep='')))}
print (nplots)

### get data
fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
print (fname)
# VarList must include all variable names that are used in this routine
VarList <- c("ACINS", "ACINS_IRS2", "ADIFR", "BDIFR", "AKRD", "SSRD", "ATHL1",
             "ATHL2", "ATRL", "AT_A", "AT_A2", "TASF", "TASR", "TAS_A", "TASX","TASHC",
             "CAVP_DPT", "CAVP_DPB", "CNTS", "FCN", "FCNC", "ATX", "DPXC",
             "CONCF_LPO", "CONCD_LPC", "CONC3_RPO", "CONCN","CONCP_RPI", "CONC1DC_LPI",
             "CORAW_AL", "DBARF_LPO", "DBARD_LPC", "DBAR3_RPO", "DBAR1DC_LPI", "DBARP_RPI",
             "DP_UVH", "DP_DPB", "DP_DPT", "DPXC", "DVALUE", "EWX", "EW_DPB", 
             "EW_DPT", "EW_UVH", "FCN", "FCNC", "GGALT", "GGALT_NVTL", "PALT", 
             "ALT", "ALT_A", "ALT_A2", "GGLAT", "GGLON", "GGSPD", "GGQUAL", 
             "GGVEW", "GGVNS", "GGVEW_NVTL", "GGVNS_NVTL", "GGVSPD_NVTL",
             "GSPD", "GSPD_A", "GVEW_A", "GVNS_A", "IWD", "IWS", "WDC", "WSC",
             "LAT", "LON", "LATC", "LONC", "LAT_A", "LON_A", "MACHF", "MACHR",
             "MACH_A", "MR", "PALT_A", "PITCH", "UXC", "VYC",
             "ROLL", "THDG", "PITCH_IRS2", "ROLL_IRS2", "THDG_IRS2", "PLWC", "PLWCC",
             "PLWCD_LPC", "PLWCF_LPO", "PLWC1DC_LPI", "PSFD", "PSFRD", "PSFDC", "PSFC", 
             "PSXC", "QCF", "QCFC", "QCFR", "QCFRC", "QCR", "QCRC", "QC_A", "PS_A",
             "REJDOF_LPO", "REJAT_LPO", "RHODT", "RHUM", "RICE",
             "RSTB", "RTHL1", "RTHL2", "RTRL", "RT_A", "THETA", "THETAP",
             "THETAE", "THETAQ", "THETAV", "TKAT", "TRSTB", "TVIR", "VEW",
             "VNS", "VEWC", "VNSC", "VSPD", "VSPD_A", "WIC", "TCNTF_LPO", "FREF_LPO")
Data <- getNetCDF (fname, VarList)

# data: select only points where TASX > 60, and optionally limit time range
StartTime <- 0; EndTime <- 0 ## can set start and end times for plot
DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
DataV <- DataV[DataV$TASX > 60, ]

#--------------------------------------------------------------------
# functions used later:
hline <- function(y, col='black', lwd=1) {
  ## note: 'Data' is a 'free variable' and needs to exist in the calling environment 
  SE <- getStartEnd(Data$Time)
  lines (c(Data$Time[getIndex(Data$Time, SE[1])], 
           Data$Time[getIndex (Data$Time, SE[2])]), 
         c(y, y), col=col, lty=2, lwd=lwd)
}

formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}

testPlot <- function (k) {
  return(k %in% nplots || nplots == 0)
}

SmoothInterp <- function (x) {
  d <- zoo::na.approx (as.vector(x), maxgap=100, na.rm = FALSE)
  d[is.na(d)] <- 0
  return (signal::filter(signal::sgolay(3, 61), d))
}

if (SavePlotsToFiles) {
  plotfile = sprintf("~/RStudio/%s/%s%sPlots.pdf", Project, Project, Flight)
  pdf (file = plotfile)
  print (sprintf ("plots saved in file %s", plotfile))
}

########################## start of plot functions ###########################
### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA) { 
  ## needs LATC, LONC, WDC, WSC, GGALT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotTrack(data, .Spacing=60, .WindFlags=10)
  title (Flight)
  plotWAC(data$Time, data$GGALT/0.3048, ylab="Altitude [ft]")
}

#----------------------------------------------------------------------------
### plot 2: construct one-per-hour track plots
RPlot2 <- function (data, Flight=NA) { 
  ## needs LATC, LONC, WDC, WSC, GGALT
  SE <- getStartEnd(data$Time)
  for (hr in 0:32) {
    if (hr*10000 < SE[2] && (hr+1)*10000 > SE[1]) {
      Start <- ifelse ((hr*10000 < SE[1]), SE[1], hr*10000)
      End <- ifelse (((hr+1)*10000 > SE[2]), SE[2], (hr+1)*10000)
      plotTrack (data, .Range=setRange(data$Time, Start, End), 
                 .Spacing=15, .WindFlags=10)
    }
  }
}

#----------------------------------------------------------------------------
### plot 3: plot all temperatures, one plot
RPlot3 <- function (data) { 
  ## needs ATHL1, ATHL2, ATRL, AT_A
  plotWAC (data[, c("Time", "ATHL1", "ATHL2", "ATRL", "AT_A")],
           ylab='ATy', lty=c(1,1,2,1), lwd=c(2,1.5,1,2), 
           legend.position='bottomright')
  title(sprintf("Means L1-L2: %.2f; L2-RL: %.2f; L2-_A: %.2f", 
                mean (data$ATHL1-data$ATHL2, na.rm=TRUE), 
                mean (data$ATHL2-data$ATRL, na.rm=TRUE),
                mean (data$ATHL2-data$AT_A, na.rm=TRUE)), cex.main=0.8)
}

#----------------------------------------------------------------------------
### plot 4: plot differences, individual pairs of temperatures
RPlot4 <- function (data) { 
  ## needs ATHL1, ATHL2, ATRL, AT_A
  op <- par (mar=c(5,5,2,1)+0.1)
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(5,6))
  # ATHL1,2 section
  plot (DF <- data[, c("ATHL1", "ATHL2")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$ATHL2 <- (data$ATHL2 - data$ATHL1)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("bottomright", legend=c("red: y=(ATHL2-ATHL1)*5", 
                                  "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(ATHL2~ATHL1, data=data)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("ATHL2=%.3f(ATHL1)%.3f\nmean diff ATHL2-ATHL1=%.2f +/- %.2f", 
                  coef[2], coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
                  sd(data$ATHL2-data$ATHL1, na.rm=TRUE))
  } else {
    t <- sprintf ("ATHL2=%.3f(ATHL1)+%.3f\nmean diff ATHL2-ATHL1=%.2f +/-%.2f", 
                  coef[2], coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
                  sd(data$ATHL2-data$ATHL1, na.rm=TRUE))
  }
  title(t, cex.main=0.75)
  # ATRL section:
  plot (DF <- data[, c("ATHL2", "ATRL")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$ATRL <- (data$ATHL2 - data$ATRL)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("bottomright", legend=c("red: y=(ATHL2-ATRL)*5", 
                                  "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(ATRL~ATHL2, data=data)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("ATRL=%.3f(ATHL2)%.3f\n mean diff ATRL-ATHL2=%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$ATRL-data$ATHL2, na.rm=TRUE),
                  sd(data$ATRL-data$ATHL2, na.rm=TRUE))
  } else {
    t <- sprintf ("ATRL=%.3f(ATHL2)+%.3f\n mean diff ATRL-ATHR2=%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$ATRL-data$ATHL2, na.rm=TRUE), 
                  sd(data$ATRL-data$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.75)
  # AT_A section:
  plot (DF <- data[, c("ATHL2", "AT_A")], pch=20)
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$AT_A <- (data$ATHL2 - data$AT_A)*5
  points (DF, col='red', type='l', lwd=2)
  lines (c(-70,30), c(5,5), col='red', lty=2)
  lines (c(-70,30), c(-5,-5), col='red', lty=2)
  legend ("topleft", legend=c("red: y=(ATHL2-AT_A)*5", 
                              "dashed lines: +/-1C error bands"), 
          box.col='red', text.col='red', cex=0.5)
  fm <- lm(AT_A~ATHL2, data=data)
  coef <- coefficients (fm)
  if (coef[1] < 0.) {
    t <- sprintf ("AT_A=%.3f(ATHL2)%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE),
                  sd(data$AT_A-data$ATHL2, na.rm=TRUE))
  } else {
    t <- sprintf ("AT_A=%.3f(ATHL2)+%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE), 
                  sd(data$AT_A-data$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.8)
}

#----------------------------------------------------------------------------
### plot 5: humidity
RPlot5 <- function (data) { 
  ## needs DP_DPB, DP<DPT, DP_UVH, EW_DPT, EX_UVH, MR, ATX, 
  ## PSFC, CAVP_DPB, CAVP_DPT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotWAC (DF <- data[, c("Time", "DP_DPB", "DP_DPT", "DP_UVH", "ATX")], 
           ylab='DPx', lty=c(1,1,2,1), lwd=c(2,1.5,1,3), 
           legend.position='bottomright', ylim=c(-40,20),           
           col=c('blue', 'red', 'darkgreen', 'black'))
  title(sprintf("Means DPB-DPT: %.2f; DPB-UVH: %.2f", 
                mean (data$DP_DPB-data$DP_DPT, na.rm=TRUE), 
                mean (data$DP_DPB-data$DP_UVH, na.rm=TRUE)), cex.main=0.8)
  plot(DF <- data[, c("DP_DPB", "DP_DPT")], pch=20, col='blue', ylab='DPy')
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$DP_DPT <- data$DP_UVH
  # set high transparency (30) to avoid obscuring DPT by UVH
  tgreen <- rgb(0,100,0,30,maxColorValue=255)
  points (DF, pch=20, col=tgreen, cex=0.5)
  legend('bottomright', legend=c("y=DP_DPT", "y=DP_UVH"), 
         pch=20, col=c('blue', tgreen))
  title("dashed orange lines: +/-1C error bands", cex.main=0.8)
  # DP cavity pressures:
  plotWAC (data[, c("Time", "CAVP_DPT", "CAVP_DPB", "PSFC")], ylab='CAVP')
  title (sprintf ("mean above PSFC: %.1f (DPB) and %.1f (DPT)", 
                  mean (data$CAVP_DPB - data$PSFC, na.rm=TRUE),
                  mean (data$CAVP_DPT - data$PSFC, na.rm=TRUE), cex.main=0.75))
  # vapor pressure and mixing ratio
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (data[, c("Time", "EW_DPB", "EW_DPT", "EW_UVH")], ylab="EWy", 
           logxy='y', ylim=c(1e-2, 100))
  lineWAC (data$Time, MurphyKoop (data$ATX, data$PSFC), col='cyan', lty=2)
  title ("cyan line: equilibrium vapor pressure at ATX")
  plotWAC (data[, c("Time", "MR")], ylab="MRy", 
           logxy='y', ylim=c(0.01, 100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "RHUM")], ylab="relative humidity [%]")
  hline (100)
}

#----------------------------------------------------------------------------
### plot 6: ambient pressures
RPlot6 <- function (data) { 
  ## needs PSFD, PSFRD, PSFDC, PSFC, PS_A
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (DF <- data[, c("Time", "PSFD", "PSFRD")], 
           col=c('blue', 'skyblue', 'red', 'red'), ylab='PSy')
  points (data$Time, (data$PSFD-data$PSFRD)*50+600, type='l', col='red')
  hline (500, 'red'); hline (700, 'red')
  legend ("bottomleft", 
          legend=c("(PSFD-PSFRD)*50+600", "+/-2 hPa"),
          lty=c(1,2), cex=0.75,
          col=c('red', 'red'))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "PSFDC", "PSFC", "PS_A")], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab="PSyC")
  points (data$Time, (data$PSFC-data$PSFDC)*50+700, type='l', col='red')
  hline (650, 'red'); hline (750, 'red')
  legend ("bottomleft", 
          legend=c("(PSFC-PSFDC)*50+700", "+/-1 hPa"),
          lty=c(1,2), cex=0.75,
          col=c('red', 'red'))
  title (sprintf ("mean difference PSFC-PSFDC: %.1f;  PS_A-PSFC: %.1f", 
                  mean (data$PSFC-data$PSFDC, na.rm=TRUE),
                  mean (data$PS_A-data$PSFDC, na.rm=TRUE)), cex.main=0.75)
}

#----------------------------------------------------------------------------
### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data) { 
  ## needs QCF, QCFR, QCR, QCFC, QCFRC, QCRC, QC_A, TASF, TASR, TAS_A, TASHC, 
  ## MACHF, MACHR, MACH_A
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (data[, c("Time", "QCF", "QCFR", "QCR")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='QCy', 
           legend.position='bottom')
  points (data$Time, (data$QCF-data$QCFR)*10+80, type='l', col='red')
  hline (60, col='red'); hline (100, col='red')
  legend("bottomleft", legend=c("red: (QCF-QCFR)*10+80", 
                                "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "QCFC", "QCFRC", "QCRC", "QC_A")], 
           col=c('blue', 'darkorange', 'darkgreen', 'brown'), ylab='QCyC',
           legend.position='bottom')
  points (data$Time, (data$QCFC-data$QCFRC)*10+80, type='l', col='red')
  hline (100, col='red'); hline (60, col='red')
  legend("bottomleft", c("red: (QCFC-QCFRC)*10+80", 
                         "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  title (sprintf ("mean difference QCFC-QCFRC=%.1f;  QC_A-QCFRC=%.1f",
                  mean (data$QCFC-data$QCFRC, na.rm=TRUE),
                  mean (data$QC_A-data$QCFRC, na.rm=TRUE)), cex.main=0.75)
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (data[, c("Time", "TASF", "TASR", "TAS_A", "TASHC")], 
           col=c('blue', 'darkorange', 'darkgreen', 'red'), ylab='TASy', 
           legend.position='bottom')
  title (sprintf ("diff vs TASF: %.1f (TASR), %.1f (TAS_A), %.1f (TASHC)",
                  mean (data$TASR-data$TASF, na.rm=TRUE), 
                  mean (data$TAS_A-data$TASF, na.rm=TRUE),
                  mean (data$TASHC-data$TASF, na.rm=TRUE)), cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "MACHF", "MACHR", "MACH_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
}

#----------------------------------------------------------------------------
### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data) { 
  ## needs PSFD, PSFRD, QCF, QCFR, PS_A, QC_A
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  DF <- data[, c("Time", "PSFD", "PSFRD")]
  DF$PSFD <- DF$PSFD + data$QCF
  DF$PSFRD <- DF$PSFRD + data$QCFR
  DF$PSA   <- data$PS_A + data$QC_A
  colnames(DF) <- c("Time", "PtotLeft", "PtotRight", "PtotAvionics")
  plotWAC (DF, col=c('blue', 'darkgreen', 'cyan'), ylab='Ptot')
  op <- par (mar=c(5,4,1,1)+0.1)
  DF$PtotLeft <- DF$PtotLeft-DF$PtotRight
  DF$PtotRight <- NULL
  DF$PtotAvionics <- NULL
  colnames (DF) <- c("Time", "PtotDiff")
  plotWAC (DF, ylim=c(-0.5, 0.5), ylab="Ptot(left) - Ptot(right)")
  title (sprintf ("mean difference: %.1f", 
                  mean (DF$PtotDiff, na.rm=TRUE)), cex.main=0.75)
  hline (0.2); hline (-0.2)
}

#---------------------------------------------------------------------------
### plot 9: wind
RPlot9 <- function (data) {
  ## needs WDC, WSC, WIC, IWD, IWS, UXC, VYC
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # set high transparency (30) to avoid obscuring first trace
  tgreen <- rgb(0,100,0,60,maxColorValue=255)
  line.colors=c('blue', tgreen, 'red')
  line.widths <- c(1,1,2)
  line.types <- c(1,3,2)
  plotWAC (data[, c("Time", "WDC", "IWD")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  hline (0); hline (90); hline (180); hline (270); hline (360)
  plotWAC (data[, c("Time", "WSC", "IWS")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "WIC")])
  title (sprintf ("flight-average vertical wind: %.02f", 
                  mean (data$WIC, na.rm=TRUE)), cex.main=0.75)
  hline (2); hline (-2)
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  DF <- data[, c("Time", "UXC")]
  DF$IUX <- data$IWS * sin (data$IWD*pi/180)
  # must recalculate this; UXC seems erroneous?
  DF$UXC <- data$WSC * sin (data$WDC*pi/180)
  plotWAC (DF, col=line.colors, lwd=line.widths, lty=line.types, 
           ylab="easterly wind")
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "VYC")]
  DF$IVY <- -data$IWS * cos (data$IWD*pi/180)
  DF$VYC <- -data$WSC * cos (data$WDC*pi/180)
  plotWAC (DF, col=line.colors, lwd=line.widths, lty=line.types, 
           ylab="southerly wind")
}

#---------------------------------------------------------------------------
### plot 10: Schuler oscillation
RPlot10 <- function (data) {
  ## needs GGVEW, GGVNS, VEW, VNS, GGQUAL
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,3))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "GGVEW", "VEW")]
  DF$DifferenceX50 <- (data$GGVEW-data$VEW)*50
  DF$GGVEW <- data$GGVEW
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.widths <- c(1,1,1)
  line.types <- c(1, 9, 1, 2)
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  DF <- data[, c("Time", "GGVNS", "VNS")]
  DF$DifferenceX50 <- (data$GGVNS-data$VNS)*50
  DF$GGVNS <- data$GGVNS
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "GGQUAL")]
  plotWAC(DF, ylim=c(0,5))
}

#---------------------------------------------------------------------------
### plot 11: attack, sideslip
RPlot11 <- function (data) {
  ## needs AKRD, PITCH, SSRD, WSC, WDC, GGVEW, GGVNS, VSPD, TASX, THDG, 
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "AKRD", "PITCH")]
  DF$AOAREF <- DF$PITCH - (180/pi) * data$VSPD / data$TASX
  plotWAC (DF, lwd=c(2,1,1), lty=c(1,2,1))
  title (sprintf ("mean diff AKRD-AOAREF = %.02f", 
                  mean (DF$AKRD-DF$AOAREF, na.rm=TRUE)), cex.main=0.75)
  hline (1); hline (3)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "SSRD")]
  u <- -1. * data$WSC * sin (data$WDC*pi/180)
  v <- -1. * data$WSC * cos (data$WDC*pi/180)
  DF$SSREF <- -data$THDG + atan2((data$GGVEW-u), (data$GGVNS-v)) * 180/pi
  DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] <- 
    DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] + 360.
  plotWAC (DF)
  title (sprintf ("mean diff SSRD-SSREF = %.02f", 
                  mean (DF$SSRD-DF$SSREF, na.rm=TRUE)), cex.main=0.75)
  hline (-2); hline (2)
}

#----------------------------------------------------------------------------
### Plot 12: IRU comparisons
RPlot12 <- function (data) {
  ## needs PITCH, ROLL, THDG, PITCH_IRS2, ROLL_IRS2, THDG_IRS2
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "PITCH", "PITCH_IRS2")]
  DF$DifferenceX50 <- (DF$PITCH - DF$PITCH_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-10.,10.), ylab="PITCH [deg.]",
           col=line.colors, lty=line.types)
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f +/- %.2f", 
                  mean (data$PITCH-data$PITCH_IRS2, na.rm=TRUE),
                  sd   (data$PITCH-data$PITCH_IRS2, na.rm=TRUE)))
  DF <- data[, c("Time", "ROLL", "ROLL_IRS2")]
  DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylab="ROLL [deg.]",
           col=line.colors, lty=line.types)
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f sd %.2f", 
                  mean (data$ROLL-data$ROLL_IRS2, na.rm=TRUE),
                  sd   (data$ROLL-data$ROLL_IRS2, na.rm=TRUE)))
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "THDG", "THDG_IRS2")]
  DF$DifferenceX50 <- (DF$THDG - DF$THDG_IRS2) * 500 + 180
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-60,390), ylab="THDG [deg.]",
           col=line.colors, lty=line.types)
  hline (180-25, 'red'); hline (180+25, 'red')
  hline (180-125, 'green', lwd=2)
  hline (90, 'lightblue'); hline (180, 'lightblue')
  hline (270, 'lightblue'); hline (360, 'lightblue'); hline (0, 'lightblue')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
         box.col='red', text.col='red', cex=0.5)
  #title( sprintf ("mean difference: %.2f", 
  #mean (data$THDG-data$THDG_IRS2, na.rm=TRUE)))
}

#----------------------------------------------------------------------------
### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data) {
  ## needs ACINS, ACINS_IRS2, FSPD, FSPD_A, GGVSPD_NVTL, GGALT, GGALT, ALT_A, ALT_A2
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "ACINS", "ACINS_IRS2")]
  plotWAC (DF, ylab="ACINS")
  title (sprintf ("mean vertical acceleration: %.3f", mean (data$ACINS, na.rm=TRUE)))
  plotWAC (data[, c("Time", "VSPD", "VSPD_A", "GGVSPD_NVTL")])
  title (sprintf ("mean vertical velocity: %.3f (IRS) and %.3f (GPS)",
                  mean (data$VSPD, na.rm=TRUE), mean (data$GGVSPD_NVTL, na.rm=TRUE)))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "GGALT", "GGALT", "ALT_A", "ALT_A2")],
           legend.position = "topright")
}

#----------------------------------------------------------------------------
### plot 14: UHSAS
RPlot14 <- function (data) {}
# if (testPlot (14)) {
# op <- par (mfrow=c(1,1), mar=c(4,5,2,2)+0.1)
# plot (Time, data$CONCU_RWO, ylab="CONCU", type='l', log='y')
# points (Time, data$CONCU100_RWO, type='l', col="green")
# points (Time, data$CONCU500_RWO, type='l', col="red")
# }

#----------------------------------------------------------------------------
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
           logxy='y', ylim=c(100,1.e6))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "CONCD_LPC", "CONCF_LPO", "CONC3_RPO", "CONC1DC_LPI")], 
           logxy='y', ylim=c(0.001,1e4), ylab='CONCy')
}

#----------------------------------------------------------------------------
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

#----------------------------------------------------------------------------
### Plot 17: all-flight Skew-T
RPlot17 <- function (data) {
  ## needs PSFC, ATX, DPXC
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  DF <- data[, c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, AverageInterval=5, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, whole-flight-average", Flight)))
}

#----------------------------------------------------------------------------
### plot 18: plot skew-T for individual climbs and descents:
RPlot18 <- function (data) {
  ## needs PSFC, ATX, DPXC, GGALT
  # search for soundings: 3 min climb/descent > tol
  del <- 180
  tol <- 3.5
  DataT <- data[!is.na(data$Time) & !is.na(data$GGALT), ]
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
        print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
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
  print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  startSounding <- endSounding + 1
  endSounding <- length(s)
  DF <- DataT[s[startSounding:endSounding], c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  # this is just to illustrate how the sounding-finder works; eventually suppress
  # op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  # plotWAC (DF <- DataT[, c("Time", "GGALT")])
  # lines (DF[r, ], lwd=2, col='red')
  # title ("red: segments selected for Skew-T soundings")
}

#----------------------------------------------------------------------------
### plot 19: potential-temperature plots
RPlot19 <- function (data) {
  ## needs THETA, THETAV, THETAE, THETAP, THETAQ, PSFC
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (data[, c("Time", "THETA", "THETAV")], ylab="potential temperatures",
           legend.position = "topright")
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "THETAE", "THETAP", "THETAQ")], 
           ylab="ad. pot. temperatures", legend.position = "top")
  # plots vs pressure:
  layout(matrix(2:1, ncol = 2), widths = c(5,5), heights = 1)
  op <- par (mar=c(5,2,1,1)+0.1)
  yl <- c(max (data$PSFC, na.rm=TRUE), min (data$PSFC, na.rm=TRUE))
  plot (data[, c("THETAP", "PSFC")], type='l', col='blue', 
        xlab='Ad. Pot. T. [K]', ylab='P [hPa]', xlim=c(300,400), ylim=yl)
  points (data$THETAE, data$PSFC, type='l', col='green')
  points (data$THETAQ, data$PSFC, type='l', col='red')
  legend ("topright", legend=c("THETAP", "THETAE", "THETAQ"), 
          lwd=1, col=c('blue', 'green', 'red'), cex=0.75)
  op <- par (mar=c(5,5,1,1)+0.1)
  plot (data[, c("THETA", "PSFC")], type='l', col='blue', xlab='Pot. T. [K]', 
        ylim=yl, ylab='P [hPa]')
  points (data$THETAV, data$PSFC, type='l', col='green')
  legend ("topleft", legend=c("THETA", "THETAV"), lwd=1, 
          col=c('blue', 'green'), cex=0.75)
}

#----------------------------------------------------------------------------
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

#----------------------------------------------------------------------------
### This is a model for adding a function:
RPlotn <- function(data) {
  ## be sure that the variables are in 'VarList'. If not, where VarList
  ## is defined, add the new variable to the variable names or follow the
  ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
  # next just resets geometry, in case previous plot used multiple panes
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  ## simplest plot:
  plotWAC (data[, c("Time", "Var1", "Var2")])
}

#----------------------------------------------------------------------------
### this section searches for speed runs and other maneuvers
SpeedRunSearch <- function (data) {
  ## needs TASX, GGALT
  del <- 75
  tol <- .3
  delz <- 20
  DataT <- data[!is.na(data$Time) & !is.na(data$TASX), ]
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
      print (sprintf( "Speed run candidate, times: %s--%s", 
                      strftime(startTime, format="%H%M%S", tz='UTC'),
                      strftime (endTime, format="%H%M%S", tz='UTC')))
      startTime <- DataT$Time[s[j+1]]
    }
  }
}

#----------------------------------------------------------------------------



## ----fig-captions--------------------------------------------------------

SE <- getStartEnd (Data$Time)
i <- getIndex (Data$Time, SE[1])
RPlot1aCap <- sprintf("Flight track for %s flight %s on %s, %d--%d UTC", Project, Flight,
                      strftime(Data$Time[i], format="20%y-%m-%d", tz='UTC'), SE[1], SE[2])
RPlot1bCap <- sprintf("Height vs. time for %s flight %s on %s, %d--%d UTC", Project, Flight,
                      strftime(Data$Time[i], format="20%y-%m-%d", tz='UTC'), SE[1], SE[2])
RPlot3Cap <- "Air temperature (ATy) determined from the different temperature sensors."
RPlot4Cap <- "Comparisons by pairs of temperature sensors. Red continuous lines show the temperature difference multiplied by 50 as a function of the temperature on the abscissa, and dashed red lines show 1C tolerances."
RPlot5Cap <- c("Measurements of dew point from the available measurements.",
               "Comparison of available measurements to DP\\_DPB as a reference.",
               "Measured pressures in the cavities of the dew-point sensors, plotted with the ambient pressure (PSXC) for reference.", 
               "Measurements of water vapor pressure, mixing ratio and relative humidity.")
RPlot6Cap <- "History of available pressure measurements, both uncorrected (top) and after application of the LAMS-derived pressure corrections (bottom)."
RPlot7Cap <- c("History of available measurements of dynamic pressure, both uncorrected (top) and after application of the LAMS-derived pressure corrections (bottom).",
               "History of redundant measurements of true airspeed and Mach number.")
RPlot8Cap <- "Total pressure measurements obtained by adding the independent pairs of measurements PSFD+QCF and PSFRD+QCFR (i.e., the uncorrected measurements). Exactly the same plot is obtained for the corrected measurements because the pressure-correction function is applied to pairs of PS and QC measurements with opposite sign, positive for PS and negative for QC."
RPlot9Cap1 <- "Wind measurements, horizontal (WDC/WSC) and vertical (WIC). See the next plot for the horizontal wind plotted in terms of easterly and southerly components."
RPlot9Cap2 <- "Easterly and southerly components of the horizontal wind [m/s]. The preceding plot shows the wind in terms of direction and speed."
RPlot10Cap <- "Comparison of ground-speed components measured by the GPS and IRU units (blue and dashed orange lines), along with the difference (red line). Dashed red lines show $\\pm$1\\,m/s limits."
RPlot11Cap <- "Measurements of the angles of attack and sideslip (blue traces, [deg.]), plotted with the reference values used for calibration that depend on the assumption of zero vertical wind or zero real sideslip not caused by wind gusts."
RPlot12Cap <- "Comparison of attitude angles measured by the two duplicate research inertial systems on the C-130, with dashed reference lines indicating the uncertainty limits quoted by the manufacturer for the angle measurements. The red lines show the differences multiplied by 50 for pitch and roll and by 500 for heading, with a further offset of 180$^{\\circ}$ for heading to center the difference plot."
RPlot13Cap <- "Vertical acceleration (top), vertical aircraft velocity (middle), and aircraft altitude (bottom) for the available redundant measurements."
RPlot14Cap <- "UHSAS"
RPlot15Cap <- "Particle concentrations from the CN counter and PCASP aerosol distrometer (top, [cm$^{-3}$) and hydrometeor concentrations from the CDP, SPP100, FSSP300, and 2DC (bottom, [cm$^{-3}$] exc. 2DC [liter${^-1}$], bottom)." 
RPlot16Cap <- c("Mean diameters [$\\mu$m] measured by the CDP and FSSP (top), the FSSP300 and PCASP (middle), and 2DC [bottom].", 
                "Measurements of liquid water content from the SPP100, CDP, and King probe (top); the power required to maintain King-probe temperature (middle), and the extimated liquid water content from the 2DC probe with the assumption that all particles are liquid.",
                "Additional characteristics of the FSSP: percentage of DOF-accepted particles (top), percentage of TOF-accepted particles (middle), and the laser power (bottom).")
RPlot17Cap <- "Plot of all temperature and pressure measurements, averaged in 5 hPa intervals, for the flight. The only restriction on data is that only measurements where TASX exceeded 60 m/s were used."
RPlot18Cap <- "Skew-T plot of data from the time interval indicated at the top of the plot, with all points plotted and without averaging."
RPlot19Cap <- c("Measurements of potential and virtual potential temperature (top panel) and of Bolton-formula equivalent potential temperature (THETAE), Davies-Jones pseudoadiabatic potential temperature (THETAP), and wet-equivalent potential temperature (THETAQ). All units are kelvin.", 
                "Vertical profiles of the same measurements shown in the preceding figure.")
RPlot20Cap <- "Size distributions measured by the CDP and FSSP, each representing 1-s of measurements." 



# this code is for use in Review.R where the text of this memo is not included:
for (np in 1:2) {
  if (testPlot(np)) {eval(parse(text=sprintf("RPlot%d(Data)", np)))}
}
for (np in 3:nps) {
  if (testPlot(np)) {eval(parse(text=sprintf("RPlot%d(DataV)", np)))}
}



SpeedRunSearch (DataV)
if (SavePlotsToFiles) {
  dev.off()
  system (sprintf ("evince %s", plotfile))
}
 


