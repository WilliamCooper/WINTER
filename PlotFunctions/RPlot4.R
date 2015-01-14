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
    t <- sprintf ("ATHL2=%.3f(ATHL1)+%.3f\nmean diff ATHL2-ATHL1=%.2f +/-%.2f", 
                  coef[2], coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
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
    t <- sprintf ("ATRL=%.3f(ATHL2)+%.3f\n mean diff ATRL-ATHR2=%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$ATRL-data$ATHL2, na.rm=TRUE), 
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
    t <- sprintf ("AT_A=%.3f(ATHL2)+%.3f\n mean diff AT_A-ATHL2%.2f +/-%.2f", 
                  coef[2], coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE), 
                  sd(data$AT_A-data$ATHL2, na.rm=TRUE))
  }
  title(t, cex.main=0.8)
}

