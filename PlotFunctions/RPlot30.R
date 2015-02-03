### plot 30: chemistry (CO2, CH4, H2O, CO)

# identify and strip out cals
# plot CO2, CH4, and CO (x2) calibration timeseries
# plot CO2, CH4, and CO (x2) calibration periods
# apply CO calibration

RPlot30 <- function (data) { 
  ## needs CO2_PIC2311, CO2C_PIC2311, CH4_PIC2311, CH4C_PIC2311, H2O_PIC2311,
  ## TIME_PIC2311, Time, COFLOW_AL, CORAW_AL, INLETP_AL, VMR_VXL
  op <- par (mfrow=c(2,1), mar=c(5,5,2,2)+0.1)

  # fix PICARRO time offset
  #### not presently working because PICARRO time stamp has a problem
  data$TIME_PIC2311=as.POSIXlt(data$TIME_PIC2311,origin='1970-01-01',tz='UTC')
  #temp2=data[,grep("PIC",colnames(data))]
  #temp2$mrgtime=as.numeric(temp2$TIME_PIC2311,units='secs')
  #data=data[,grep("PIC",colnames(data),invert=T)]
  #data$mrgtime=as.numeric(data$Time,units='secs')
  #data=merge(data,temp2,by="mrgtime",all.x=T)
  
  # plot raw and corrected CO2 and CH4
  plotWAC (DF <- data[, c("Time", "CO2_PIC2311", "CO2C_PIC2311")], 
           ylab=expression (paste (CO[2]," [", mu, "mol/mole]")), 
           lty=c(1,1), lwd=c(1.5,2), legend.position='top', 
           col=c('dark red', 'red'))
  title(expression(paste("Picarro 2311 Dry Air Mole Fraction ",CO[2])), cex.main=0.8)
  data$CH4_PIC2311=data$CH4_PIC2311*1000
  data$CH4C_PIC2311=data$CH4C_PIC2311*1000
  plotWAC (DF <- data[, c("Time", "CH4_PIC2311", "CH4C_PIC2311")], 
           ylab=expression (paste (CH[4]," [nmol/mole]")), 
           lty=c(1,1), lwd=c(1.5,2), legend.position='top', 
           col=c('dark blue', 'blue'))
  title(expression(paste("Picarro 2311 Dry Air Mole Fraction ",CH[4])), cex.main=0.8)
  
  # plot H2O and VXL
  data$H2O_PIC2311=data$H2O_PIC2311*10000
  plotWAC (DF <- data[, c("Time", "H2O_PIC2311", "VMR_VXL")], 
           ylab=expression (paste (H[2],"O [", mu, "mol/mole]")), 
           lty=c(1,1), lwd=c(1.5,2), legend.position='top', 
           col=c('green', 'purple'))
  title(expression(paste("Mole Fraction ",H[2],"O")), cex.main=0.8)

  # plot PICARRO time offset
  data$timediff=difftime(data$TIME_PIC2311,data$Time,units='secs')
  plotWAC (DF <- data[, c("Time", "timediff")], 
           ylab="seconds",
           lty=c(1,1), lwd=c(1.5,2), legend.position='bottomright', 
           col=c('dark green'))
  title("Picarro 2311 Time Offset", cex.main=0.8)
  
    op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  # plot H2O vs VXL
  ### presently looks bad b/c time correction not possible
  plot(DF <- data[, c("VMR_VXL", "H2O_PIC2311")], pch=20, col='green', 
       ylab="H2O_PIC2311")
  title(expression(paste("Mole Fraction ",H[2],"O [", mu, "mol/mole]")), cex.main=0.8)

  #lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  #lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  #title("dashed orange lines: +/-1C error bands", cex.main=0.8)

  # plot XY of CO2 and CH4 corrections against H2O
  op <- par (mfrow=c(1,1), mar=c(5,5,2,4)+0.1)
  data$co2corr=data$CO2C_PIC2311-data$CO2_PIC2311
  data$ch4corr=data$CH4C_PIC2311-data$CH4_PIC2311
  plot(DF <- data[, c("H2O_PIC2311", "co2corr")], pch=20, col='red', 
#       ylab=expression(paste(Delta," ",mu,"mole/mole")))
  axes=F,ylab='')
  box();axis(2,col='red',col.ticks='red',col.axis='red')
  mtext(expression(paste(Delta," ",mu,"mole/mole")),2,2,col='red')
  title("Picarro Water Vapor Corrections", cex.main=0.8)
  par(new=T)
  plot(DF <- data[, c("H2O_PIC2311", "ch4corr")], pch=20, col='blue', axes=F,
       ylab='',xlab='')
  axis(4,col='blue',col.ticks='blue',col.axis='blue')
  mtext(expression(paste(Delta," nmole/mole")),4,2,col='blue')
  legend('bottomright', legend=c("y=CO2 Corr.", "y=CH4 Corr."), 
         pch=20, col=c('red', 'blue'))
  
  


  
}

