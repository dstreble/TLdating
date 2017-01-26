shinyPlot_TL.AP_peak <- function(
  
  temperatures,
  TL,
  Tx,
  pos.peak,
  plotting.parameters
  
){
  Tmax <- max(temperatures)
  nPoints <- length(temperatures)
  
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  
  # -------------------------------
  Tstep <- Tmax/nPoints
  
  plot.min <- ceiling(plot.Tmin/Tstep)
  plot.max <-floor(plot.Tmax/Tstep)
  
  #----------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------
  
  #Layout
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  layout(matrix(c(1,2), 1, 2, byrow = TRUE))
  
  #Plot not aligned
  #Boundary
  
plot.TL.max <- max(TL[plot.min:plot.max,],na.rm = TRUE)
  
  #color
  colors <- 1:ncol(TL)
  
  for(i in 1 : ncol(TL)){  
    temp.TL <- TL[,i]
    temp.color <- colors[i]
    
    if(i == 1) {
      plot(x=temperatures, 
           y=temp.TL, 
           xlim=c(0,Tmax),
           ylim=c(0,plot.TL.max), 
           xlab="Temperature (\u00b0C)",
           ylab = "Luminescence signal",
           main="TL before peaks alignement",
           type="l", 
           col=temp.color)
      
      par(new = TRUE)
      
    }else{
      lines(x=temperatures, 
            y=temp.TL, 
            col=temp.color, 
            xlim=c(0,Tmax),
            ylim=c(0,plot.TL.max)
      )
    }
  }
  par(new = FALSE)
  
  #Plot Reference TL (testdose)
  #Boundary
  plot.TL.max <- max(Tx[plot.min:plot.max,],na.rm = TRUE)
  
  #color
  colors <- 1:ncol(Tx)
  
  for(i in 1 : ncol(Tx)){  
    temp.TL <- Tx[,i]
    temp.color <- colors[i]
    
    if(i == 1) {
      plot(x=temperatures, 
           y=temp.TL, 
           xlim=c(0,Tmax),
           ylim=c(0,plot.TL.max), 
           xlab="Temperature (\u00b0C)",
           ylab = "Luminescence signal (Tx)",
           main="Peak position",
           type="l", 
           col=temp.color)
      
      par(new = TRUE)
      
    }else{
      lines(x=temperatures, 
            y=temp.TL, 
            col=temp.color, 
            xlim=c(0,Tmax),
            ylim=c(0,plot.TL.max)
      )
    }
  }    
  abline(v=pos.peak,col=2,lty=3)
  par(new = FALSE)

  
  #clean layout...
  layout(1)
  par(old.par)
}