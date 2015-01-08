require(Ranadu)
require(maps)
require(mapdata)
require(mapproj)

### general structure and instructions:
# This program uses a set of functions to make plots that are designed to
# provide a starting point for quality assessment for C-130 projects, esp.
# WINTER. This structure makes it easy to change the set of plots for a
# particular project, to refine the time range for plots, to re-generate
# a subset of the plots, to add new plots, and otherwise to tailor the
# output to project and personal needs.
# 
# The program can be used in different ways. The simplest is to source("Review.R")
# from an R console, including the console in RStudio. The program asks for
# a project and flight, uses a standard set of plots or allows a user to change
# that set interactively, and then produces a PDF file that is a concatenation
# of the plots produced. PNG files could also be produced and would provide a
# much smaller output size, but PDFs have the advantage of maintaining quality
# when magnified (and are also easier to concatenate) so that has been selected
# even though that format produces large files (of typical size 10 MB).
# 
# Other ways to use the Review.R script and routines include:
#   1. After running the script, the individual plot functions are available
#      and the assignment of plots to the PDF file has been cancelled, so you
#      can re-generate any selected plots for console or RStudio display,
#      optionally with variable or time-segment modifications.
#   2. 'Review.R' can be incorporated into an Rnw document where comments can
#      be added to provide a record of problems identified or needing further
#      study, and this might be useful documentation to preserve and distribute.
#   3. It may be useful to make the set of functions available as a package so
#      they can be used easily without needing the full structure of 'Review.R'
# 



### configuration:
SavePlotsToFiles <- TRUE # if TRUE plots are saved to a file, and not displayed
Flight <- "rf09"
Project <- "FRAPPE"
x <- readline(sprintf("Project is %s; CR to accept or enter new project name: ", Project))
if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format): ", 
                      Flight))
if (nchar(x) > 1) {Flight <- x}
print(sprintf("Flight is %s", Flight))
nplots=0
print (sprintf("Plots desired %s: CR to accept, or enter new spec", nplots))
x <- readline ("new spec: (0 => all, -1 => project set, n => plot n, can be a sequence): ")
if (nchar(x) > 0) {nplots <- eval(parse(text=paste('c(',x,')', sep='')))}
print (nplots)

### get data
fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
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
# data: select only points where TASX > 60, and optionally limit time range
StartTime <- 0; EndTime <- 0   ## cab set start and end times for plot
DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
DataV <- DataV[DataV$TASX > 60, ]

# functions used later:
hline <- function(y, col='black') {
  ## note: 'Data' is a 'free variable' and needs to exist in the calling environment 
  SE <- getStartEnd(Data$Time)
  lines (c(Data$Time[getIndex(Data$Time, SE[1])], Data$Time[getIndex (Data$Time, SE[2])]), 
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

########################## start of plot functions  ###########################

### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA) {  
  ## needs LATC, LONC, WDC, WSC, GGALT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotTrack(data, .Spacing=60, .WindFlags=10)
  title (Flight)
  plotWAC(data$Time, data$GGALT/0.3048, ylab="Altitude [ft]")
}

### plot 2: construct one-per-hour track plots
RPlot2 <- function (data, Flight=NA) {  
  ## needs LATC, LONC, WDC, WSC, GGALT
  SE <- getStartEnd(data$Time)
  for (hr in 0:32) {
    if (hr*10000 < SE[2] && (hr+1)*10000 > SE[1]) {
      Start <- ifelse ((hr*10000 < SE[1]), SE[1], hr*10000)
      End   <- ifelse (((hr+1)*10000 > SE[2]), SE[2], (hr+1)*10000)
      plotTrack (data, .Range=setRange(data$Time, Start, End), .Spacing=15, .WindFlags=10)
    }
  }
}

### plot 3: plot all temperatures, one plot
RPlot3 <- function (data) {  
  ## needs ATHL1, ATHL2, ATRL, AT_A
  plotWAC (data[, c("Time", "ATHL1", "ATHL2", "ATRL", "AT_A")],
           ylab='ATx', lty=c(1,1,2,1), lwd=c(2,1.5,1,2), 
           legend.position='bottomright')
  title(sprintf("Means L1-L2: %.2f; L2-RL: %.2f; L2-_A: %.2f", 
                mean (data$ATHL1-data$ATHL2, na.rm=TRUE), 
                mean (data$ATHL2-data$ATRL, na.rm=TRUE),
                mean (data$ATHL2-data$AT_A, na.rm=TRUE)), cex.main=0.8)
}

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
    t <- sprintf ("ATHL2=%.3f(ATHL1)+%.3f\nmean diff ATHL2-ATHL1=%.2f +/-%.2f", coef[2], 
                  coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
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
    t <- sprintf ("ATRL=%.3f(ATHL2)+%.3f\n mean diff ATRL-ATHR2=%.2f +/-%.2f", coef[2], 
                  coef[1], mean(data$ATRL-data$ATHL2, na.rm=TRUE), 
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
    t <- sprintf ("AT_A=%.3f(ATHL2)+%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", coef[2], 
                  coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE), 
                  sd(data$AT_A-data$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.8)
}

### plot 5: humidity
RPlot5 <- function (data) {  
  ## needs DP_DPB, DP<DPT, DP_UVH, EW_DPT, EX_UVH, MR, MR_UVH, ATX, 
  ## PSFC, CAVP_DPB, CAVP_DPT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotWAC (DF <- data[, c("Time", "DP_DPB", "DP_DPT", "DP_UVH")], 
           ylab='DPx', lty=c(1,1,2), lwd=c(2,1.5,1), 
           legend.position='bottomright', 
           col=c('blue', 'red', 'darkgreen'))
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
  # vapor pressure and mixing ratio
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (data[, c("Time", "EW_DPB", "EW_DPT", "EW_UVH")], ylab="EWy", 
           logxy='y', ylim=c(1e-2, 100))
  lineWAC (data$Time, MurphyKoop (data$ATX, data$PSFC), col='cyan', lty=2)
  title ("cyan line: equilibrium vapor pressure at ATX", cex.main=0.75)
  plotWAC (data[, c("Time", "MR", "MR_UVH")], ylab="MRy", 
           logxy='y', ylim=c(0.01, 100))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "RHUM")], ylab="relative humidity [%]")
  hline (100)
}

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
          legend=c("PSFD", "PSFRD", "(PSFD-PSFRD)*50+600", "+/-2 hPa"),
          lty=c(1,1,1,1,2), cex=0.35,
          col=c('blue', 'skyblue', 'red', 'red'))
  
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "PSFDC", "PSFC", "PS_A")], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab="PSyC")
  points (data$Time, (data$PSFDC-data$PSFC)*50+700, type='l', col='red')
  hline (650, 'red'); hline (750, 'red')
  legend ("bottomleft", 
          legend=c("PSFDC", "PSFC", "PS_A", "(PSFDC-PSFC)*50+700", "+/-1 hPa"),
          lty=c(1,1,1,1,2), cex=0.35,
          col=c('blue', 'skyblue', 'darkgreen', 'red', 'red'))
}

### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data) {  
  ## needs QCF, QCFR, QCR, QCFC, QCFRC, QCRC, QC_A, TASF, TASR, TAS_A, 
  ## MACHF, MACHR, MACH_A
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (data[, c("Time", "QCF", "QCFR", "QCR")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='QCy', 
           legend.position='bottom')
  points (data$Time, (data$QCF-data$QCFR)*10+80, type='l', col='red')
  hline (60, col='red'); hline (100, col='red')
  title("red: (QCF-QCFR)*10+80; dashed red: +/- 2 hPa [diff]", cex.main=0.85)
  
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "QCFC", "QCFRC", "QCRC", "QC_A")], col=c('blue', 'darkorange', 'darkgreen', 'brown'), ylab='QCyC',
           legend.position='bottom')
  points (data$Time, (data$QCFC-data$QCFRC)*10+80, type='l', col='red')
  hline (100, col='red'); hline (60, col='red')
  title("red: (QCFC-QCFRC)*10+80; dashed red: +/- 2 hPa [diff]", cex.main=0.85)
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (data[, c("Time", "TASF", "TASR", "TAS_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='TASy', 
           legend.position='bottom')
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "MACHF", "MACHR", "MACH_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
  
}

### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data) {  
  ## needs PSFD, PSFRD, QCF, QCFR
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  DF <- data[, c("Time", "PSFD", "PSFRD")]
  DF$PSFD <- DF$PSFD + data$QCF
  DF$PSFRD <- DF$PSFRD + data$QCFR
  colnames(DF) <- c("Time", "PtotLeft", "PtotRight")
  plotWAC (DF, col=c('blue', 'darkgreen'), ylab='Ptot')
  
  op <- par (mar=c(5,4,1,1)+0.1)
  DF$PtotLeft <- DF$PtotLeft-DF$PtotRight
  DF$PtotRight <- NULL
  colnames (DF) <- c("Time", "PtotDiff")
  plotWAC (DF, ylim=c(-0.5, 0.5), ylab="Ptot(left) - Ptot(right)")
  hline (0.2); hline (-0.2)
}

### plot 9: wind
RPlot9 <- function (data) {
  ## needs WDC, WSC, WIC, IWD, IWS
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # set high transparency (30) to avoid obscuring first trace
  tgreen <- rgb(0,100,0,60,maxColorValue=255)
  line.colors=c('blue', tgreen)
  line.widths <- c(1,1)
  line.types <- c(1,3)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (data[, c("Time", "WDC", "IWD")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  plotWAC (data[, c("Time", "WSC", "IWS")], 
           col=line.colors, lwd=line.widths, lty=line.types)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "WIC")])
  title (sprintf ("flight-average vertical wind: %.02f", 
                  mean (data$WIC, na.rm=TRUE)), cex.main=0.75)
  hline (2); hline (-2)
}


### plot 10: Schuler oscillation
RPlot10 <- function (data) {
  ## needs GGVEW_NVTL, GGVNS_NVTL, VEW, VNS, GGQUAL
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,3))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "GGVEW", "VEW")]
  DF$DifferenceX50 <- (data$GGVEW-data$VEW)*50
  DF$GGVEW_NVTL <- data$GGVEW_NVTL
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.widths <- c(1,1,1)
  line.types <- c(1, 9, 1, 2)
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  DF <- data[, c("Time", "GGVNS", "VNS")]
  DF$DifferenceX50 <- (data$GGVNS-data$VNS)*50
  DF$GGVNS_NVTL <- data$GGVNS_NVTL
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "GGQUAL")]
  plotWAC(DF, ylim=c(0,5))
}

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
  DF <- data[, c("Time", "ROLL", "ROLL_IRS2")]
  DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS2) * 50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylab="ROLL [deg.]",
           col=line.colors, lty=line.types)
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "THDG", "THDG_IRS2")]
  DF$DifferenceX50 <- (DF$THDG - DF$THDG_IRS2) * 500 + 180
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-60,390), ylab="THDG [deg.]",
           col=line.colors, lty=line.types)
  hline (180-25, 'red'); hline (180+25, 'red')
  hline (90, 'lightblue'); hline (180, 'lightblue')
  hline (270, 'lightblue'); hline (360, 'lightblue'); hline (0, 'lightblue')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
         box.col='red', text.col='red', cex=0.5)
}
  
### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data) {
  ## needs ACINS, ACINS_IRS2, FSPD, FSPD_A, GGVSPD_NVTL, GGALT, GGALT_NVTL, ALT_A, ALT_A2
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "ACINS", "ACINS_IRS2")]
  plotWAC (DF, ylab="ACINS")
  plotWAC (data[, c("Time", "VSPD", "VSPD_A", "GGVSPD_NVTL")])
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "GGALT", "GGALT_NVTL", "ALT_A", "ALT_A2")],
           legend.position = "topright")
}

### plot 14: UHSAS
RPlot14 <- function (data) {}
# if (testPlot (14)) {
#   op <- par (mfrow=c(1,1), mar=c(4,5,2,2)+0.1)
#   plot (Time, data$CONCU_RWO, ylab="CONCU", type='l', log='y')
#   points (Time, data$CONCU100_RWO, type='l', col="green")
#   points (Time, data$CONCU500_RWO, type='l', col="red")
# }


### plot 15: CN, FSSP, CDP, F300, CONCP
RPlot15 <- function(data) {
  ## needs CONCN, CNTS, CONCD_LPC, CONCF_LPO, CONC3_RPO, CONCP_RPI
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  # remove zeroes for log plot:
  data$CONCN[!is.na(data$CONCN) & (data$CONCN <= 0)] <- NA
  data$CONCD_LPC[!is.na(data$CONCD_LPC) & (data$CONCD_LPC <= 0)] <- NA
  data$CONCF_LPO[!is.na(data$CONCF_LPO) & (data$CONCF_LPO <= 0)] <- NA
  data$CONC3_RPO[!is.na(data$CONC3_RPO) & (data$CONC3_RPO <= 0)] <- NA
  data$CONCP_RPI[!is.na(data$CONCP_RPI) & (data$CONCP_RPI <= 0)] <- NA
  plotWAC (data[, c("Time", "CNTS", "CONCN")], 
           logxy='y', ylim=c(100,1.e6))
  plotWAC (data[, c("Time", "CONCD_LPC", "CONCF_LPO", "CONC3_RPO")], 
           logxy='y', ylim=c(0.001,1e4), ylab='CONCy')
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "CONCP_RPI")], 
           logxy='y', ylim=c(10,1e5))
}

### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data) {
  ## needs DBARD_LPC, DBARF_LPO, DBAR3_RPO, DBARP_RPI, DBAR1DC_LPI
  ##       PLWCF_LPO, PLWCD_LPC, PLWC, PLWCC, PLWC1DC_LPI
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

### Plot 17: all-flight Skew-T
RPlot17 <- function (data) {
  ## needs PSFC, ATX, DPXC
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  DF <- data[, c("PSFC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, AverageInterval=5)
        +ggtitle(sprintf("Flight %s, whole-flight-average", Flight)))
}
  
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
  plot (data[, c("THETA", "PSFC")], type='l', col='blue', xlab='Pot. T. [K]', ylab='P [hPa]')
  points (data$THETAV, data$PSFC, type='l', col='green')
  legend ("bottomleft", legend=c("THETA", "THETAV"), lwd=1, col=c('blue', 'green'), cex=0.75)
  op <- par (mar=c(5,5,1,1)+0.1)
  plot (data[, c("THETAP", "PSFC")], type='l', col='blue', 
        xlab='Ad. Pot. T. [K]', ylab='P [hPa]', xlim=c(300,400))
  points (data$THETAE, data$PSFC, type='l', col='green')
  points (data$THETAQ, data$PSFC, type='l', col='red')
  legend ("bottomright", legend=c("THETAP", "THETAE", "THETAQ"), 
          lwd=1, col=c('blue', 'green', 'red'), cex=0.75)
}

# plot 20: CDP/SP100 size distributions
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
      print (sprintf( "Speed run times: %s--%s", strftime(startTime, format="%H%M%S", tz='UTC'),
                      strftime (endTime, format="%H%M%S", tz='UTC')))
      startTime <- DataT$Time[s[j+1]]
    }
  }
}

########################## start of plot generation ###########################
nps <- 20
### plot 1: construct flight track with map
### plot 2: construct one-per-hour track plots
### plot 3: plot all temperatures, one plot
### plot 4: plot differences, individual pairs of temperatures
### plot 5: humidity
### plot 6: ambient pressures
### plot 7: dynamic pressure; also TAS and MACH
### plot 8: total pressure (static + dynamic)
### plot 9: wind
### plot 10: Schuler oscillation
### plot 11: attack, sideslip
### Plot 12: IRU comparisons
### plot 13: IRU continued, ACINS, VSPD
### plot 14: UHSAS
### plot 15: CN, FSSP, CDP, F300, CONCP
### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
### Plot 17: all-flight Skew-T
### plot 18: plot skew-T for individual climbs and descents:
### plot 19: potential-temperature plots
### plot 20: CDP/SP100 size distributions

if (testPlot(1)) {RPlot1(Data, Flight)}
if (testPlot(2)) {RPlot2 (Data)}
for (np in 3:nps) {
  if (testPlot(np)) {eval(parse(text=sprintf("RPlot%d(DataV)", np)))}
}

### speed-run search
SpeedRunSearch (DataV)

if (SavePlotsToFiles) {
  dev.off()
  system (sprintf ("evince %s", plotfile))
}




