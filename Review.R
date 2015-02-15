
## ----initialization,echo=FALSE,include=FALSE-----------------------------

## testing testing 123

library(knitr)
opts_chunk$set(echo=FALSE, include=TRUE, fig.lp="fig:")
opts_chunk$set(fig.width=9.5, fig.height=7, fig.align="center", 
               digits=4, fig.show="asis", fig.path="Figures/WR-")
thisFileName <- "Review"
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(maps, quietly=TRUE)
require(mapdata, quietly=TRUE)
require(mapproj, quietly=TRUE)
require(ggplot2)
require(grid)
require(ggthemes)
setwd ("~/RStudio/WINTER")

## if command arguments are supplied, via 'Rscript Review.R "rf01" "-1" then
## these will over-ride the interactive commands below. Arguments are all strings:
##  1 Flight (string, e.g., "rf01")
##  2 plots desired. Project-default is "-1". Can also use "1,3,5" or "1:9,13"
##  3 ShowPlots: 3rd argument 0 suppresses saving; any other entry saves them
##  4 Start time for the plots, in HHMMSS format. If >240000, use e.g. 250000 for 10000
##  5 End time for plots, HHMMSS format, as for Start time
## For example, 'Rscript Review.R rf03 17 0 220000 230000' displays plot 17 for
## the listed times on an Xwindows screen.
##  Possible way to include this in a batch script?
##  cd ~/RStudio/WINTER; Rscript Review.R rf01 -1
run.args <- commandArgs (TRUE)

## ----configuration, include=TRUE-----------------------------------------

SavePlotsToFiles <- 1 # if != 0 plots are saved to a file
                      # 2 => display via evince when finished.
if (length (run.args) > 2) {
  SavePlotsToFiles <- as.numeric(run.args[3])
  if (SavePlotsToFiles == 0) {
    x11 ()
  }
}
print (sprintf (" length of run.args is %d, SavePlotsToFiles is %d", 
                length (run.args), SavePlotsToFiles))
nps <- 30 
Flight <- "rf01"
Project <- "WINTER"
# x <- readline(sprintf("Project is %s; CR to accept or enter new project name: ", Project))
# if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
## find max rf in data directory, use as default if none supplied via command line:
Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project), 
                        sprintf ("%srf...nc", Project)), decreasing = TRUE)[1]
Flight <- sub (Project, '',  sub (".nc", '', Fl))

if (length (run.args) > 0) {
  Flight <- run.args[1]
} else {
  x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format): ", 
                        Flight))
  if (nchar(x) > 1) {Flight <- x}
}
print(sprintf("Flight is %s", Flight))
nplots=c(1, 3:17, 19:nps,30)    # project default
if (length (run.args) > 1) {
  if (run.args[2] != "-1") {
    nplots <- eval (parse (text=paste('c(', run.args[2],')', sep='')))
  }  # else leave unchanged, using project default
  if (run.args[2] == "0") {
    nplots <- 1:nps 
  } 
} else {
  print ("Plots desired: CR to accept, or enter new spec")
  x <- readline ("new spec: (0 => all, -1 => project set, n => plot n, can be a sequence): ")
  if (nchar(x) > 0) {nplots <- eval(parse(text=paste('c(',x,')', sep='')))}
}
print (nplots)
StartTime <- 0; EndTime <- 0 ## can set start and end times for plot
if (length(run.args) > 3) {
  StartTime <- as.numeric (run.args[4])
  if (length (run.args) > 4) {
    EndTime <- as.numeric (run.args[5])
  }
}

### get data
fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
print (sprintf ("processing %s", fname))
# VarList must include all variable names that are used in this routine
## VarList is a file that contains all the variables needed. It loads
## 'VarList' as a vector of those names. Add new names to that file.
source("./VarList")
Data <- getNetCDF (fname, VarList)

# data: select only points where TASX > 60, and optionally limit time range
DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
DataV <- DataV[(!is.na (DataV$TASX)) & (DataV$TASX > 60), ]

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
  ## skip if there are fewer than 100 measurements
  if (length (x[!is.na(x)]) < 100) {return (x)}
  d <- zoo::na.approx (as.vector(x), maxgap=100, na.rm = FALSE)
  d[is.na(d)] <- 0
  return (signal::filter(signal::sgolay(3, 61), d))
}

if (SavePlotsToFiles) {
  if (SavePlotsToFiles == 2) {
    plotfile = sprintf("~/RStudio/%s/%s%sPlotsP.pdf", Project, Project, Flight)
  } else {
    plotfile = sprintf("~/RStudio/%s/%s%sPlots.pdf", Project, Project, Flight)
  }
  cairo_pdf (filename = plotfile, onefile=TRUE)
  ## enable the next to get individual png files instead of one large pdf
  #### png (file = sprintf ("./Figures/WINTER%s-%%02d.png", Flight))
  print (sprintf ("saving plots to file %s", plotfile))
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
### this section loads functions for searches for maneuvers
source ("./PlotFunctions/SpeedRunSearch.R")
source ("./PlotFunctions/CircleSearch.R")
source ("./PlotFunctions/PitchSearch.R")
source ("./PlotFunctions/YawSearch.R")

## ----fig-captions--------------------------------------------------------

SE <- getStartEnd (Data$Time)
i <- getIndex (Data$Time, SE[1])
FigFooter=sprintf("%s %s %s %s-%s UTC,",Project,toupper(Flight),strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),strftime(Data$Time[getIndex(Data$Time,SE[2])], format="%H:%M:%S", tz='UTC'))
FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
RPlot1aCap <- sprintf("Flight track for %s flight %s on %s, %d--%d UTC", Project, Flight,
                      strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'), SE[1], SE[2])
RPlot1bCap <- sprintf("Height vs. time for %s flight %s on %s, %d--%d UTC", Project, Flight,
                      strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'), SE[1], SE[2])
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
RPlot15Cap <- "Particle concentrations from the CN counter and PCASP aerosol distrometer (top, [cm$^{-3}$) and hydrometeor concentrations from the CDP, SPP100, and 2DC (bottom, [cm$^{-3}$] exc. 2DC [liter${^-1}$], bottom)." 
RPlot16Cap <- c("Mean diameters [$\\mu$m] measured by the CDP and FSSP (top), the PCASP (middle), and 2DC [bottom].", 
                "Measurements of liquid water content from the SPP100, CDP, and King probe (top); the power required to maintain King-probe temperature (middle), and the extimated liquid water content from the 2DC probe with the assumption that all particles are liquid.",
                "Additional characteristics of the FSSP: percentage of DOF-accepted particles (top), percentage of TOF-accepted particles (middle), and the laser power (bottom).")
RPlot17Cap <- "Plot of all temperature and pressure measurements, averaged in 5 hPa intervals, for the flight. The only restriction on data is that only measurements where TASX exceeded 60 m/s were used."
RPlot18Cap <- "Skew-T plot of data from the time interval indicated at the top of the plot, with all points plotted and without averaging."
RPlot19Cap <- c("Measurements of potential and virtual potential temperature (top panel) and of Bolton-formula equivalent potential temperature (THETAE), Davies-Jones pseudoadiabatic potential temperature (THETAP), and wet-equivalent potential temperature (THETAQ). All units are kelvin.", 
                "Vertical profiles of the same measurements shown in the preceding figure.")
RPlot20Cap <- "Size distributions measured by the CDP and FSSP, each representing 1-s of measurements." 
RPlot21Cap <- "Radiometric temperatures, RSTB (top panel, surface temperature) and RSTT (bottom panel, sky or cloud base temperature."

## ----plot-loop-----------------------------------------------------------

### This section loops through plot functions, first loading them from 'PlotFunctions'
### and then running them with the appropriate data.
for (np in 1:2) {
  if (testPlot(np)) {
    print(paste('Plot',np))
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
    eval(parse(text=sprintf("RPlot%d(Data, Flight)", np)))
  }
}
for (np in 3:nps) {
  if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
    if (testPlot(np)) {
      print(paste('Plot',np))
      eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
      eval(parse(text=sprintf("RPlot%d(DataV)", np)))
    }
  }
}
print('done plotting')
## ----manuever-search-----------------------------------------------------

PitchSearch (DataV)
YawSearch (DataV)
SpeedRunSearch (DataV)
CircleSearch (DataV)

if (SavePlotsToFiles) {
  dev.off()
  print (sprintf ("Plots are ready in file %s", plotfile))
  print (Sys.time())
  if (SavePlotsToFiles == 2) {
    system (sprintf ("evince %s&", plotfile))
  }

## make the html file, but only for SavePlotsToFiles == 1
  if (SavePlotsToFiles == 1) {
    plothtml <- sprintf ("WINTER%sPlots.html", Flight)
    syscmd <- paste ("Rscript -e 'commandArgs(TRUE);knitr::spin (\"Review.R\",",
                     "format=\"Rmd\")'", sprintf ("%s -1 3 ", Flight), sep=' ')
    system (syscmd, wait=TRUE)
    system (sprintf ("mv Review.html WINTER%sPlots.html", Flight))
  }
} else {
  ## message ("press Enter (with focus here) to dismiss the plot and end routine")
  ## invisible (readLines ("stdin", n=1))
}

 


