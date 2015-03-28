
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

