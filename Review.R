
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

SavePlotsToFiles <- TRUE # if TRUE plots are saved to a file, and not displayed
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


### This section loops through plot functions, first loading them from 'PlotFunctions'
### and then running them with the appropriate data.
for (np in 1:2) {
  if (testPlot(np)) {
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
    eval(parse(text=sprintf("RPlot%d(Data, Flight)", np)))
  }
}
for (np in 3:nps) {
  if (testPlot(np)) {
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
    eval(parse(text=sprintf("RPlot%d(DataV)", np)))
  }
}



SpeedRunSearch (DataV)
if (SavePlotsToFiles) {
  dev.off()
  system (sprintf ("evince %s&", plotfile))
}
 


