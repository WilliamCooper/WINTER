
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
               "Comparison of available measurements to DP\\_VXL as a reference.",
               "(top) Measured pressures in the cavities of the dew-point sensors, plotted with the ambient pressure (PSXC) for reference, and (bottom) the indicator of laser intensity for the VCSEL, with normal limits indicated by the dashed red lines.", 
               "Measurements of water vapor pressure, mixing ratio and relative humidity. The cyan line in the top plot is the equilibrium water vapor pressure for the measured temperature ATX and is shown to indicate the expected upper limit to measurements of vapor pressure.")
RPlot6Cap <- "History of available pressure measurements, both uncorrected (top) and after application of the LAMS-derived pressure corrections (bottom)."
RPlot7Cap <- c("History of available measurements of dynamic pressure, both uncorrected (top) and after application of the LAMS-derived pressure corrections (bottom).",
               "History of redundant measurements of true airspeed and Mach number.")
RPlot8Cap <- "Total pressure measurements obtained by adding the independent pairs of measurements PSFD+QCF and PSFRD+QCFR (i.e., the uncorrected measurements). Exactly the same plot is obtained for the corrected measurements because the pressure-correction function is applied to pairs of PS and QC measurements with opposite sign, positive for PS and negative for QC."
RPlot9Cap1 <- "Wind measurements, horizontal (WDC/WSC) and vertical (WIC). See the next plot for the horizontal wind plotted in terms of easterly and southerly components. The pale green lines in the top two panels are from the inertial reference unit without GPS updating."
RPlot9Cap2 <- "Easterly and southerly components of the horizontal wind [m/s]. The preceding plot shows the wind in terms of direction and speed."
RPlot10Cap <- "Comparison of ground-speed components measured by the GPS and IRU units (blue and dashed orange lines), along with the difference (red line). Dashed red lines show $\\pm$1\\,m/s limits."
RPlot11Cap <- "Measurements of the angles of attack and sideslip (blue traces, [deg.]), plotted with the reference values used for calibration that depend on the assumption of zero vertical wind or zero real sideslip not caused by wind gusts."
RPlot12Cap <- "Comparison of attitude angles measured by the two duplicate research inertial systems on the C-130, with dashed reference lines indicating the uncertainty limits quoted by the manufacturer for the angle measurements. The red lines show the differences multiplied by 50 for pitch and roll and by 500 for heading, with a further offset of 180$^{\\circ}$ for heading to center the difference plot."
RPlot13Cap <- "Vertical acceleration (top), vertical aircraft velocity (middle), and aircraft altitude (bottom) for the available redundant measurements."
RPlot14Cap <- "UHSAS"
RPlot15aCap <- "Particle concentrations from the CN counter and PCASP aerosol distrometer (top, [cm$^{-3}$) and hydrometeor concentrations from the CDP, SPP100, and 2DC (bottom, [cm$^{-3}$] exc. 2DC [liter${^-1}$], bottom)." 
RPlot15bCap <- "Measured flows and laser voltages for the UHSAS, PCASP, and CN counters. Legends describe how to interpret the dashed lines indicating expected limits."
RPlot16Cap <- c("Mean diameters [$\\mu$m] measured by the CDP and FSSP (top), the PCASP (middle), and 2DC [bottom].", 
                "Measurements of liquid water content from the SPP100, CDP, and King probe (top); the power required to maintain King-probe temperature (middle), and the extimated liquid water content from the 2DC probe with the assumption that all particles are liquid.",
                "Additional characteristics of the FSSP: percentage of DOF-accepted particles (top), percentage of TOF-accepted particles (middle), and the laser power (bottom).")
RPlot17Cap <- "Plot of all temperature and pressure measurements, averaged in 5 hPa intervals, for the flight. The only restriction on data is that only measurements where TASX exceeded 60 m/s were used."
RPlot18Cap <- "Skew-T plot of data from the time interval indicated at the top of the plot, with all points plotted and without averaging."
RPlot19Cap <- c("Measurements of potential and virtual potential temperature (top panel) and of Bolton-formula equivalent potential temperature (THETAE), Davies-Jones pseudoadiabatic potential temperature (THETAP), and wet-equivalent potential temperature (THETAQ). All units are kelvin.", 
                "Vertical profiles of the same measurements shown in the preceding figure.")
RPlot20Cap <- "Size distributions measured by the CDP and FSSP, each representing 1-s of measurements." 
RPlot21Cap <- "Radiometric temperatures, RSTB (top panel, surface temperature) and RSTT (bottom panel, sky or cloud base temperature."
RPlot22Cap <- "Size distributions measured by the UHSAS and PCASP. Each plot represents a 1-s average."
RPlot30aCap <- "CO$_2$ and CH$_4$ measurements, uncorrected and corrected, from the Picarro 2311."
RPlot30bCap <- "Expanded view of the measurements in the preceding plot."
RPlot30cCap <- "(top) Picarro-measured water vapor mole fraction compared to the measurement from the VCSEL hygrometer. (bottom) The Picarro time offset, ''original'' (dark green) and ''new'' (light green)."
RPlot30dCap <- "Comparison of the mole fraction of water vapor measured by the Picarro PIC2311 to that measured by the VCSEL hygrometer. In this data file there is no correction for the time offset, which enhances the scatter in this plot."
RPlot30eCap <- "Corrections applied to measurements of CO$_2$ and CH$_4$ from the Picarro 2311 instrument to correct for the effect of water vapor."
RPlot30fCap <- "The uncorrected measurement of CO."
RPlot30gCap <- "Flow and inlet pressure in the CO instrument."

