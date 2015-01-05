require(Ranadu)

SavePlotsToFiles <- TRUE # if TRUE plots are saved to a file, not displayed
Flight <- "rf09"
Project <- "FRAPPE"
x <- readline(sprintf("Project is %s; CR to accept or enter new project name:$", Project))
if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format):", 
                      Flight))
if (nchar(x) > 1) {Flight <- x}
print(sprintf("Flight is %s", Flight))


fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
print (fname)
VarList <- c("ACINS", "ACINS_IRS2", "ADIFR", "BDIFR", "AKRD", "SSRD", "ATHL1",
             "ATHL2", "ATRL", "AT_A", "AT_A2", "TASF", "TASR", "TAS_A", "TASX",
             "CAVP_DPT", "CAVP_DPB", "CNTS", "FCN", "FCNC", "ATX", "DPXC",
             "CONCF_LPO", "CONCD_LPC", "CONC3_RPO", "CONCN","CONCP_RPI",
             "CORAW_AL", "DBARF_LPO", "DBARD_LPC", "DBAR3_RPO", "DP_UVH",
             "DP_DPB", "DP_DPT", "DPXC", "DVALUE", "EWX", "EW_DPB", "EW_DPT",
             "EW_UVH", "FCN", "FCNC", "GGALT", "GGALT_NVTL", "PALT", "ALT",
             "ALT_A", "ALT_A2", "GGLAT", "GGLON", "GGSPD", "GGQUAL", "GGVEW", 
             "GGVNS", "GGVEW_NVTL", "GGVNS_NVTL", "GGVSPD_NVTL",
             "GSPD", "GSPD_A", "GVEW_A", "GVNS_A", "IWD", "IWS", "WDC", "WSC",
             "LAT", "LON", "LATC", "LONC", "LAT_A", "LON_A", "MACHX", "MACHR",
             "MACH_A", "MR", "MR_UVH", "MR", "MR_UVH", "PALT_A", "PITCH",
             "ROLL", "THDG", "PITCH_IRS2", "ROLL_IRS2", "THDG_IRS2",
             "PLWCD_LPC", "PLWCF_LPO", "PSFD", "PSFRD", "PSFDC", "PSFC", "PSXC",
             "QCF", "QCFC", "QCFR", "QCFRC", "QCR", "QCRC", "QC_A", "PS_A",
             "REJDOF_LPO", "REJAT_LPO", "RHODT", "RHODT_UVH", "RHUM", "RICE",
             "RSTB", "RTHL1", "RTHL2", "RTRL", "RT_A", "THETA", "THETAP",
             "THETAE", "THETAQ", "THETAV", "TKAT", "TRSTB", "TVIR", "VEW",
             "VNS", "VEWC", "VNSC", "VSPD", "VSPD_A", "WIC")

Data <- getNetCDF (fname, VarList)

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

if (SavePlotsToFiles) {
  #png (filename=sprintf("~/RStudio/%s/%s%sPlot%%02d.png", Project, Project, Flight))
  plotfile = sprintf("~/RStudio/%s/%s%sPlots.pdf", Project, Project, Flight)
  pdf (file = plotfile)
  print (sprintf ("plots saved in file %s", plotfile))
}

# construct flight track with map
require(maps)
require(mapdata)
require(mapproj)
op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
#plotTrack (LONC, LATC, Time, WDC=WDC, WSC=WSC, .Spacing=60, .WindFlags=10)
plotTrack(Data, .Spacing=60, .WindFlags=10)
title (Flight)
plotWAC(Data$Time, Data$GGALT/0.3048, ylab="Altitude [ft]")
# plot track one hour per plot
SE <- getStartEnd(Data$Time)
for (hr in 0:32) {
  if (hr*10000 < SE[2] && (hr+1)*10000 > SE[1]) {
    Start <- ifelse ((hr*10000 < SE[1]), SE[1], hr*10000)
    End   <- ifelse (((hr+1)*10000 > SE[2]), SE[2], (hr+1)*10000)
    plotTrack (Data, .Range=setRange(Data$Time, Start, End), .Spacing=15, .WindFlags=10)
  }
}

# limit time range
DataV <- Data
#DataV <- Data[setRange(Data$Time, 185900, 190500), ]
# eliminate points where TASX < 60:
DataV <- DataV[DataV$TASX > 60, ]
# attach(DataV)

# check temperatures
plotWAC (DataV[, c("Time", "ATHL1", "ATHL2", "ATRL", "AT_A")],
         ylab='ATx', lty=c(1,1,2,1), lwd=c(2,1.5,1,2), 
         legend.position='bottomright')
title(sprintf("Means L1-L2: %.2f; L2-RL: %.2f; L2-_A: %.2f", 
              mean (DataV$ATHL1-DataV$ATHL2, na.rm=TRUE), 
              mean (DataV$ATHL2-DataV$ATRL, na.rm=TRUE),
              mean (DataV$ATHL2-DataV$AT_A, na.rm=TRUE)), cex.main=0.8)

#ATHL1,2 section
plot (DF <- DataV[, c("ATHL1", "ATHL2")], pch=20)
lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
DF$ATHL2 <- (DataV$ATHL2 - DataV$ATHL1)*5
points (DF, col='red', type='l', lwd=2)
lines (c(-70,30), c(5,5), col='red', lty=2)
lines (c(-70,30), c(-5,-5), col='red', lty=2)
legend ("bottomright", legend=c("red: y=(ATHL2-ATHL1)*5", 
                                "dashed lines: +/-1C error bands"), 
        box.col='red', text.col='red')
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
title(t, cex.main=0.8)

#ATRL section:
plot (DF <- DataV[, c("ATHL2", "ATRL")], pch=20)
lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
DF$ATRL <- (DataV$ATHL2 - DataV$ATRL)*5
points (DF, col='red', type='l', lwd=2)
lines (c(-70,30), c(5,5), col='red', lty=2)
lines (c(-70,30), c(-5,-5), col='red', lty=2)
legend ("bottomright", legend=c("red: y=(ATHL2-ATRL)*5", 
                                "dashed lines: +/-1C error bands"), 
        box.col='red', text.col='red')
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
title(t, cex.main=0.8)

#AT_A section:
plot (DF <- DataV[, c("ATHL2", "AT_A")], pch=20)
lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
DF$AT_A <- (DataV$ATHL2 - DataV$AT_A)*5
points (DF, col='red', type='l', lwd=2)
lines (c(-70,30), c(5,5), col='red', lty=2)
lines (c(-70,30), c(-5,-5), col='red', lty=2)
legend ("bottomright", legend=c("red: y=(ATHL2-AT_A)*5", 
                                "dashed lines: +/-1C error bands"), 
        box.col='red', text.col='red')
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



#humidity
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

#pressure
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

#dynamic pressure
op <- par (mar=c(2,4,1,1)+0.1)
layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
plotWAC (DF <- DataV[, c("Time", "QCF", "QCFR", "QCR")], 
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
# legend ("bottom", legend=c("QCFC", "QC_A", "QCFRC",
#                         "QCRC", "(QCFC-QCFRC)*10+60", "+/-1 hPa"),
#         lty=c(1,1,1,1,1,2), cex=0.35,
#         col=c('blue', 'green', 'cyan', 'orange', 'red', 'red'))

#wind
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
hline(Data$Time, 2); hline (Data$Time, -2)


#Schuler oscillation
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

# attack, sideslip
layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
op <- par (mar=c(2,4,1,1)+0.1)
plotWAC (DataV[, c("Time", "AKRD", "PITCH")], lwd=c(2,1), lty=c(1,2))
hline (Data$Time, 1); hline (Data$Time, 3)
op <- par (mar=c(5,4,1,1)+0.1)
plotWAC (DataV[, c("Time", "SSRD")])
hline (Data$Time, -2); hline (Data$Time, 2)


# IRU
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
hline (Data$Time, 90); hline (Data$Time, 180)
hline (Data$Time, 270); hline (Data$Time, 360); hline (Data$Time, 0)
legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
       box.col='red', text.col='red', cex=0.5)

# IRU continued: ACINS, VSPD
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
op <- par (mar=c(2,4,1,1)+0.1)
DF <- DataV[, c("Time", "ACINS", "ACINS_IRS2")]
plotWAC (DF, ylab="ACINS")
plotWAC (DataV[, c("Time", "VSPD", "VSPD_A", "GGVSPD_NVTL")])
op <- par (mar=c(5,4,1,1)+0.1)
plotWAC (DataV[, c("Time", "GGALT", "GGALT_NVTL", "ALT_A", "ALT_A2")])

# #UHSAS
# op <- par (mfrow=c(1,1), mar=c(4,5,2,2)+0.1)
# plot (Time, CONCU_RWO, ylab="CONCU", type='l', log='y')
# points (Time, CONCU100_RWO, type='l', col="green")
# points (Time, CONCU500_RWO, type='l', col="red")


#CN
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
op <- par (mar=c(2,4,1,1)+0.1)
plotWAC (DataV[, c("Time", "CNTS", "CONCN")], 
         logxy='y', ylim=c(100,1.e6))
plotWAC (DataV[, c("Time", "CONCD_LPC", "CONCF_LPO", "CONC3_RPO")], 
         logxy='y', ylim=c(0.001,1e4), ylab='CONCy')
op <- par (mar=c(5,4,1,1)+0.1)
plotWAC (DataV[, c("Time", "CONCP_RPI")], 
         logxy='y', ylim=c(10,1e5))

# Skew-T
op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
DF <- DataV[, c("PSFC", "ATX", "DPXC")]
colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
print(SkewTSounding (DF, AverageInterval=5)
      +ggtitle(sprintf("Flight %s, whole-flight-average", Flight)))
# search for climbs and descents:
plotWAC(DataV[, c("Time", "GGVSPD_NVTL")])
plotWAC (DataV[, c("Time", "GGALT")])
# search for soundings: (3 min climb/descent > tol)
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

op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
plotWAC (DF <- DataT[, c("Time", "GGALT")])
lines (DF[r, ], col='red')

# netCDFfile = open.ncdf(fname)
# CUHSAS <- get.var.ncdf(netCDFfile, "CUHSAS_RWO")
# CellSizes <- att.get.ncdf (netCDFfile, "CUHSAS_RWO", "CellSizes")
# CellLimits <- CellSizes$value
# 
# 
# for (j in seq(1400,1420,2)) {
#   UHSAS <- vector ("numeric", 100)
#   for (k in 0:60) {
#     UHSAS <- UHSAS + CUHSAS[,getIndex(Time, j*100+k)]
#   }
#   UHSAS <- UHSAS / 60.
#   UHSAS[UHSAS <= 0.] <- 1.e-3
#   plot (CellLimits, UHSAS, type='l', ylim=c(1.e-3,1.e1), xlab="Diameter [um]", log="xy")
#   lines (c(0.5,0.5), c(1.e-4, 1.e3), lty=2, col="darkgreen")
#   lines (c(0.1,0.1), c(1.e-4, 1.e3), lty=2, col="darkgreen")
#   title (sprintf ("1-min average starting at %d", j))
# }
# close.ncdf (netCDFfile)

if (SavePlotsToFiles) {
  dev.off()
}
#detach (DataV)



