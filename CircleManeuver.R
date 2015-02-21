
## ----initialize, echo=FALSE, include=FALSE-------------------------------
require(Ranadu) 
library(knitr) 
require(maps)
require(mapdata)
require(mapproj)
# set global chunk options 
opts_chunk$set(fig.path='figure/', echo=FALSE, fig.align='center', fig.show='hold', fig.width=6, fig.height=5, fig.lp="fig:")
options(replace.assign=TRUE,width=49, digits=2)
opts_knit$set(eval.after = "fig.cap")
thisFileName <- "CircleManeuver"

#### this section specifies defaults, changed by run-time arguments or user interaction
Flight <- "tf01"        # this was the test flight with a circle maneuver
Project <- "WINTER"
startCircles <- 195200  # use 1955 to exclude some problem areas in the first circle
endCircles <- 200455
UserInteraction <- TRUE
recalculateWind <- FALSE
findWindFromGPS <- FALSE
#### ------

run.args <- commandArgs (TRUE)
if (length (run.args) > 0) {
  Project <- run.args[1]
} else {
  if (UserInteraction) {
   x <- readline(sprintf("Project is %s; CR to accept or enter new Project name (e.g., WINTER): ",  Project))
    print (x)
    if (nchar(x) > 1) {Project <- x}
  }
}

if (length (run.args) > 1) {
  Flight <- run.args[2]
} else {
  if (UserInteraction) {
    x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format): ", Flight))
    print (x)
    if (nchar(x) > 1) {Flight <- x}
  }
}

print(sprintf("Project is %s, Flight is %s", Project, Flight))

if (length (run.args) > 2) {
  startCircles <- as.numeric(run.args[3])
} else {
  if (UserInteraction) {
    x <- readline (sprintf ("start time for circle maneuver is %.0f;\nCR to accept or enter new time, format HHMMSS:", startCircles))
    print(x)
    if (nchar(x) > 1) {startCircles <- as.numeric(x)}
  }
}

if (length (run.args) > 3) {
  endCircles <- as.numeric(run.args[4])
} else {
  if (UserInteraction) {
   x <- readline (sprintf ("end time for circle maneuver is %.0f;\nCR to accept or enter new time, format HHMMSS: ", endCircles))
    print(x)
    if (nchar(x) > 1) {endCircles <- as.numeric(x)}
  }
}

## undocumented options: 5th argument == 1 sets recalculateWind TRUE, 
## 6th sets findWindFromGPS TRUE
if (length (run.args) > 4) {
  recalculateWind <- as.numeric (run.args[5])
}

if (length (run.args) > 5) {
  findWindFromGPS <- as.numeric (run.args[6])
}

ReloadData <- FALSE     # if FALSE, use data previously saved (faster)
ReloadData <- TRUE      # if TRUE, load data from netCDF files again
if (ReloadData) {
  fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
  # fname <- "/home/Data/WINTER/WINTERtf01hr.nc"
  ## check file:
  while (!file.exists(fname)) {
    x <- readline(sprintf ("file %s not found; enter full-path file name or CR to quit:", fname))
    if (nchar(x) > 1) {
      fname <- x
    } else {
      quit (save="no")
    }
  }
  VarNames <- c("GGALT","LATC", "LONC", "PSXC", "QCXC",
              "WDC", "WSC", "GGVEW", "GGVNS", "VEW", "VNS", "TASX",
              "ADIFR", "SSLIP", "PITCH", "ATTACK",
              "ROLL", "THDG", "BDIFR", "EWX", "GGVSPD", "PSFD", "PSFRD", "QCF", "QCFR",
              "PSFDC", "PSFC", "QCFRC",
              "WIC", "GGVEW", "GGVNS", "VSPD", "ATX", "VNSC", "VEWC")
  D <- getNetCDF (fname, VarNames)      # this handles high-rate data also
  save(D, file="~/RStudio/DEEPWAVE/CircleData.Rdata")
} else {
  load(file="~/RStudio/DEEPWAVE/CircleData.Rdata") # loads D as previously saved
}


## ----search-for-circles--------------------------------------------------

if (startCircles < 0 || endCircles < 0) {
  if (file.exists("./chunks/CircleSearch.R")) {
    source ("./chunks/CircleSearch.R")
    SE <- CircleSearch (D)
    startCircles <- as.numeric (SE[1])
    endCircles   <- as.numeric (SE[2])
  }
  if (startCircles < 0 || endCircles < 0) {
    print (sprintf ("no circle times available; aborting..."))
    quit (save="no")
  }
  print (sprintf ("new circle start and end times: %.0f--%.0f", startCircles, endCircles))
}


## ----plot-track, echo=FALSE, include=TRUE, fig.cap="Portion of the flight track from WINTER tf01, 19:52:00 to 20:04:55, showing the circle maneuver flown at that time. The top panel shows the geographic position of the track, and the lower panel shows the track in a reference frame drifting with the wind.", fig.height=4----

r <- setRange(D$Time, startCircles,endCircles)   # set this to span the circle maneuver
DC <- D[r, ]
plotTrack (DC, .Spacing=2, .WindFlags=0.02)                 # plot the flight track
plotTrack (DC, xc=NA, .Spacing=2)          # xc=NA is flag to do drifting plot


## ----recalculate-wind, eval=FALSE----------------------------------------

if (recalculateWind) {
  ## special section: recalculate winds because there was a problem with QCF
  ## tf01 winds were based on PSFRD/QCFR, but there were errors still in AKRD/SSRD
  ## initial processing: no GGVEW time lag, no cal applied to heading
  D$ATTACK <- D$AKRD <- 4.8596 + 14.141572 * D$ADIFR / D$QCFR
  D$SSLIP <- D$SSRD <- 1.610 + 13.4098 * D$BDIFR / D$QCFR
  ##                   -0.57 ## to remove the offset found here.
  D$TASX <- TrueAirspeed (MachNumber (D$PSFC, D$QCFRC, D$EWX), D$ATX, D$EWX/D$PSFC)
  ##        -0.14  ## to remove indicated correction from fits in this routine
  ## D$THDG <- D$THDG - 0.14   ## this correction would remove the heading error
  ##startCircles = 195500        ## this is also SPECIAL for this case; exclude normally
  D <- WindProcessor (D)    ## adds WSN, WDN, WIN and applies pitch correction algorithm
  D$WSC <- D$WSN       # make substitutions to leave code below intact
  D$WDC <- D$WDN
  r <- setRange(D$Time, startCircles,endCircles)   # set this to span the circle maneuver
  DC <- D[r, ]
}



## ----get-mean-wind-and-RL-subsets----------------------------------------

## subsets that are just left-turn circles and just right-turn circles
DCL <- DC[DC$ROLL < -24, ]
DCR <- DC[DC$ROLL >  24, ]
# rL <- setRange(DC$Time, 195230,195730)     # period of left turns
# rL <- setRange(DC$Time, 195500,195730)     # period of left turns
# rR <- setRange(DC$Time, 195930,200410)     # period of right turns
ROLLbarL <-mean(DCL$ROLL)
ROLLsdL <- sd(DCL$ROLL)
ROLLbarR <- mean(DCR$ROLL)
ROLLsdR <- sd(DCR$ROLL)
WDM <- DC$WDC * pi / 180.
WSM <- DC$WSC
WEW <- WSM * sin (WDM)
WNS <- WSM * cos (WDM)
WEWbar <- mean (WEW, na.rm=TRUE)
WNSbar <- mean (WNS, na.rm=TRUE)
WDMbar <- atan2 (WEWbar, WNSbar) * 180. / pi
WSMbar <- mean (sqrt(WEW^2+WNS^2), na.rm=TRUE)
Cradeg = pi/180.
# print (sprintf ("mean of measured wind in circles: %.1f deg. / %.1f m/s", WDMbar, WSMbar))



## ----find-wind-direction-from-GPS, include=TRUE, eval=FALSE--------------

if (findWindFromGPS) {
  # define a chisquare error function:
  csq <- function (x, .d) {
    roll <- .d$ROLL * pi / 180
    gamma <- (.d$THDG + x[4] + .d$SSLIP*cos(roll) - .d$ATTACK*sin(roll)) * pi / 180 
    dvx <- (.d$TASX+x[3])*sin(gamma) -x[1] - .d$GGVEW
    dvy <- (.d$TASX+x[3])*cos(gamma) -x[2] - .d$GGVNS
    chisq <- sum (dvx**2 + dvy**2)
  }
  
  NL <- nrow(DC$GGVEW)
  wx <- 10.
  wy <- 10.
  dV <- 0.5
  dG <- 0.5
  A <- nlm (csq, c(wx, wy, dV, dG), DC, hessian=TRUE)
  rms <- sqrt(A$minimum / nrow (DC))
  bestFit <- A$estimate
  print (bestFit)
  
  # best-fit wind:
  bestWD <- atan2(-bestFit[1], -bestFit[2]) / Cradeg + 180. %% 360
  if (bestWD > 360) {bestWD <- bestWD - 360}
  bestWS <- sqrt(bestFit[1]**2 + bestFit[2]**2)
  DC$WDC[DC$WDC < 180] <- DC$WDC[DC$WDC < 180] + 360
  aveWD <- mean (DC$WDC, na.rm=TRUE) %% 360
  aveWS <- mean (DC$WSC, na.rm=TRUE)
  print (sprintf ("wind determined from drift: %.1f / %.1f", bestWD, bestWS))
  print (sprintf ("Mean measured wind:         %.1f / %.1f", WDMbar, WSMbar))
}

## ----substitute-measured-mean-wind, include=TRUE-------------------------

if (!exists("aveWD")) {
  cat (sprintf ("GPS-drift wind section suppressed; \nmeasured mean wind (%.1f/%.1f) is used.", WDMbar, WSMbar))
  bestWD <- WDMbar
  bestWS <- WSMbar
}



## ----plot-sine, fig.lp="fig:", fig.cap="The measured wind speed as a function of the difference between heading and mean wind direction, for the measurements from the flight segment shown in the preceding figure. Top: before correction (with dashed orange line showing the correction function); bottom: after correction.", fig.height=4, include=TRUE----

DCR$Ang <- (DCR$THDG - bestWD + 360) %% 360
DCL$Ang <- (DCL$THDG - bestWD + 360) %% 360
DD <- rbind (DCR, DCL)
pmin <- min(DD$WSC, na.rm=TRUE)
pmax <- max (DD$WSC, na.rm=TRUE)
plot (DCL$Ang, DCL$WSC, xlim=c(0,360), pch=19, col='blue', xlab="heading - wind direction [deg.]", ylab="measured wind speed [m/s]", ylim=c(pmin, pmax))
points (DCR$Ang, DCR$WSC, pch=19, col='darkgreen')
legend("bottomright", legend=c("left turns", "right turns"), col=c("blue", "darkgreen"), pch=16)
lines(c(180,180), c(0.,50.), col='red', lwd=2)
lines(c(90,90), c(0.,50.), col='red', lwd=2)
lines(c(270,270), c(0.,50.), col='red', lwd=2)
lines(c(360,360), c(0.,50.), col='red', lwd=2)
lines(c(0,0), c(0.,50.), col='red', lwd=2)
# print(mean(DD$WDC,na.rm=TRUE))

DD$Ang <- DD$Ang * Cradeg
fm <- lm (WSC~I(cos(Ang))+I(TASX*sin(Ang)), data=DD)
cf <- coefficients(fm)
xp <- 0:360
TASAVG <- mean (DD$TASX, na.rm=TRUE)
yp <- cf[1] + cf[2] * cos(xp*Cradeg) + cf[3] * TASAVG * sin(xp*Cradeg)
lines (xp, yp, col='darkorange', lty=2, lwd=4)
# find rms error after adjustment:
DD$WSCorr <- DD$WSC - cf[2] * cos(DD$Ang) - cf[3] * DD$TASX * sin(DD$Ang)
plot (DD$Ang, DD$WSCorr, col='blue', type='p', pch=20, ylim=c(pmin,pmax), xlab="heading - wind direction [deg.]", ylab="corrected wind speed [m/s]")
print (sprintf ("rms before and after fit adjustment: %.2f and %.2f m/s", sd(DD$WSC, na.rm=TRUE), sd(DD$WSCorr, na.rm=TRUE)))



## ----sideslip-correction-------------------------------------------------

dbeta <- (DD$PITCH-DD$ATTACK*cos(DD$ROLL*pi/180))/sin(DD$ROLL*pi/180)-DD$SSLIP
# hist (dbeta)
dbm <-mean (dbeta, na.rm=TRUE)
dbsd <- sd   (dbeta, na.rm=TRUE) / sqrt (length(dbeta))

cat (sprintf ("Fit results: wind speed %.2f m/s, TASX correction %.2f \nSS correction %.2f deg., heading correction %.2f deg.\n", cf[1], cf[2], dbm, cf[3]*180/pi-dbm))

## ----plotSS, fig.lp="fig:", fig.cap="Sideslip measured turning the circle maneuvers, first turning left (19:52:30--19:57:30) and then turning right (19:59:30--20:04:10). Mean values for these segments are indicated by dashed orange lines.", echo=FALSE, include=FALSE----

# plotWAC(DC[, c("Time", "SSLIP")], ylab="Sideslip [deg.]")
# # lines(c(DC$Time[1], DC$Time[length(DC$Time)]), c(0.3,0.3), col='red', lty=2, lwd=4)
# lines(c(DC$Time[1], DC$Time[length(DC$Time)]), c(0.,0.), col='red', lty=2, lwd=4)
# yl <- mean(DCL$SSLIP)
# lines(c(DCL$Time[1], DCL$Time[length(DCL$Time)]), c(yl,yl), col='darkorange', lty=2, lwd=4)
# yr <- mean(DCR$SSLIP)
# lines(c(DCR$Time[1], DCR$Time[length(DCR$Time)]), c(yr,yr), col='darkorange', lty=2, lwd=4)







