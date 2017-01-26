shinyPlot_TL.MAAD_DP <- function(
  temperatures,
  eval.Tmin,
  eval.Tmax,
  DP.Q.line,
  DP.Q.line.error,
  DP.I.line,
  DP.I.line.error,
  Q.DP,
  Q.DP.error,
  I.DP,
  I.DP.error,
  plotting.parameters=list(plot.Tmin=0,
                           plot.Tmax=NA)
){
  Tmax <- max(temperatures)
  nPoints <- length(temperatures)
  Tstep <- Tmax/nPoints
  eval.min <- ceiling(eval.Tmin/Tstep)
  eval.max <-floor(eval.Tmax/Tstep)
  
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  
  #----------------------------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------------------------
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  

  #Layout
  layout(matrix(c(1,2), 1, 2, byrow = TRUE))
  
  # Plotting  Palaeodose (Q) ----------------------------------------
  
  if(length(DP.Q.line) > 0){
    
    plot.DP.Q.line.max <- max(DP.Q.line[eval.min:eval.max],na.rm = TRUE)*1.5
    
    plot(x=temperatures,
         y=DP.Q.line,
         xlim=c(plot.Tmin, plot.Tmax),
         ylim=c(0, plot.DP.Q.line.max),
         xlab = "Temperature (\u00b0C)",
         ylab = "Dose (s)",
         main = "D\u2091 plateau - Palaeodose (Q)",
         sub = paste("Q =",
                     round(Q.DP, digits = 2), "\u00b1", round(Q.DP.error, digits = 2),
                     paste( "(", round(Q.DP.error/Q.DP*100, digits = 2), "%)",sep = "")
         ),
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
    
  }else{
    #Empty space
    textplot(" ")
    title("D\u2091 plateau - Palaeodose (Q)")
  }
  
  # Plotting supralinerarity correction (I) -----------------------------------------
  
  if(length(DP.I.line) > 0){
    
    plot.DP.I.line.max <- max(DP.I.line[eval.min:eval.max],na.rm = TRUE)*2
    
    plot(x=temperatures,
         y=DP.I.line,
         xlim=c(plot.Tmin, plot.Tmax),
         ylim=c(0, plot.DP.I.line.max),
         main = "D\u2091 plateau - supralinearity corr. (I)",
         sub = paste("I =",
                     round(I.DP, digits = 2), "\u00b1", round(I.DP.error, digits = 2),
                     paste( "(", round(I.DP.error/I.DP*100, digits = 2), "%)",sep = "")
         ),
         xlab = "Temperature (\u00b0C)",
         ylab = "Dose (s)",
         type="b",
         lty=2,
         pch=18,
         col=4)
    
    par(new = TRUE)
    
    abline(v=eval.Tmin,col=3,lty=3)
    abline(v=eval.Tmax,col=2,lty=3)
    
    arrows(temperatures,
           DP.I.line-DP.I.line.error,
           temperatures,
           DP.I.line+DP.I.line.error,
           length=0.05,
           angle=90,
           code=3)
    
    par(new = FALSE)
    
  }else{
    #Empty space
    textplot(" ")
    title("D\u2091 plateau - supralinearity corr. (I)")
  }
  
  
  #clean layout...
  par(old.par)
  layout(matrix(c(1), 1, 1, byrow = TRUE))
  
}