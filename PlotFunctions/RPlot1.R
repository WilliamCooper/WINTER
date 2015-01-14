### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA) { 
  ## needs LATC, LONC, WDC, WSC, GGALT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  plotTrack(data, .Spacing=60, .WindFlags=10)
  SE <- getStartEnd (data$Time)
  i <- getIndex (data$Time, SE[1])
  title (sprintf("%s %s %d-%d", Flight,
                 strftime(data$Time[i], format="20%y-%m-%d", tz='UTC'), 
                 SE[1], SE[2]), cex=0.75, font.main=1)
  plotWAC(data$Time, data$GGALT/0.3048, ylab="Altitude [ft]")
}

