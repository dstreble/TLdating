shinyPlot_TL.SAR_DP <- function(
  
  eval.Tmin,
  eval.Tmax,
  
  temperatures,
  
  DP.Q.line,
  DP.Q.line.error,
  
  Q.DP,
  Q.DP.error,
  
  TxTn,
  
  rejection.values,
  
  plotting.parameters=list(plot.Tmin=0,
                           plot.Tmax=NA)
){
  Tmax <- max(temperatures)
  nPoints <- length(temperatures)
  Tstep <- Tmax/nPoints
  eval.min <- ceiling(eval.Tmin/Tstep)
  eval.max <-floor(eval.Tmax/Tstep)
  
  
  recycling.ratio <- rejection.values$recycling.ratio
  recuperation.rate <- rejection.values$recuperation.rate
  Lx.error.max <- rejection.values$Lx.error.max
  Tx.error.max <- rejection.values$Tx.error.max
  test.recycling <- rejection.values$test.recycling
  test.recuperation <- rejection.values$test.recuperation
  test.Lx.error <- rejection.values$test.Lx.error
  test.Tx.error <- rejection.values$test.Tx.error
  
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  
  #----------------------------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------------------------
  
  
  old.par <- par( no.readonly = TRUE )
  par( oma = c( 0, 0, 3, 0 ) )
  
  # --------------------------------------------------------------------
  #page 2
  # --------------------------------------------------------------------
  
  #Layout
  layout(matrix(c(1,2), 1, 2, byrow = TRUE))
  
  # Plotting  Palaeodose (DP.Q.line) ----------------------------------------
  
  plot.DP.Q.line.max <- max(DP.Q.line[eval.min:eval.max])*1.5
  
  par(mar=c(5,4,4,1))
  plot(x=temperatures,
       y=DP.Q.line,
       xlim=c(plot.Tmin, plot.Tmax),
       ylim=c(0, plot.DP.Q.line.max),
       main = "Palaeodose (DP)",
       sub = paste("D\u2091 =",
                   round(Q.DP, digits = 2), "\u00b1", round(Q.DP.error, digits = 2),
                   paste( "(", round(Q.DP.error/Q.DP*100, digits = 2), "%)",sep = "")),
       xlab = "Temperature (\u00b0C)",
       ylab = "Dose (s)",
       type="b",
       lty=2,
       pch=18,
       col=6)
  
  par(new = TRUE)
  
  abline(v=eval.Tmin,col=3,lty=3)
  abline(v=eval.Tmax,col=2,lty=3)
  
  arrows(temperatures,
         DP.Q.line-DP.Q.line.error,
         temperatures,
         DP.Q.line+DP.Q.line.error,
         length=0.05,
         angle=90,
         code=3)
  par(new = FALSE)
  
  # Plotting  Testdose response ----------------------------------------
  
  temp.x <- seq(from = 1,to = length(TxTn),by = 1)
  
  par(mar=c(5,4,4,2))
  plot(x=temp.x,
       y=TxTn,
       type="o",
       col=1,
       xlim=c(0, max(temp.x)),
       ylim=c(0, max(TxTn)),
       main = "Testdose response",
       xlab = "SAR cycle",
       ylab = "Tx/Tn"
  )
  par(new = TRUE)
  abline(h=1,col=2,lty=3)
  par(new = FALSE)
  
  # --------------------------------------------------------------------
  # Clean layout
  par(old.par)
  layout(matrix(c(1), 1, 1, byrow = TRUE))
  
}