### plot 3: plot all temperatures, one plot
RPlot3 <- function (data) { 
  ## needs ATHL1, ATHL2, AT_A
  ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
  plotWAC (data[, c("Time", "ATHL2", "ATHL1", "AT_A")],
           ylab=ylb, lty=c(1,1,2,1), 
           lwd=c(2,1.5,1,2), legend.position='bottomright')
  title(sprintf("Means L1-L2: %.2f; L2-_A: %.2f", 
                mean (data$ATHL1-data$ATHL2, na.rm=TRUE), 
                mean (data$ATHL2-data$AT_A, na.rm=TRUE)), cex.main=0.8)
}

