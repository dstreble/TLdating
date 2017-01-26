shinyPlot_TL.HP <- function(
  plot.name,
  temperatures,
  names,
  doses,
  Lx,
  Lx.plateau,
  eval.Tmin,
  eval.Tmax, 
  plotting.parameters=list(plot.Tmin=0,
                           plot.Tmax=NAE)
){
  # ------------------------------------------------------------------------------
  # Integrity Check
  # ------------------------------------------------------------------------------
  
  #...
  
  Tmax <- max(temperatures)
  nPoints <- length(temperatures)
  nCurves <- ncol(Lx)
  Tstep <- Tmax/nPoints
  
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  plot.legend <- plotting.parameters$legend
  
  eval.min <- ceiling(eval.Tmin/Tstep)
  eval.max <- ceiling(eval.Tmax/Tstep)
  
  # ------------------------------------------------------------------------------
  # Check Value
  # ------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------------------------
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  
  #Layout
  layout(matrix(c(1,2), 1, 2, byrow = TRUE))
  
  #color
  if("N" %in% names){
    ref_colors <- rainbow(n=nCurves-1)
    colors <- seq(nCurves)
    names(colors) <- names
    
    colors[names(colors) == "N" ] <- 1
    colors[names(colors) != "N" ] <- ref_colors[1:nCurves-1]
  }else{
    colors <- rainbow(n=nCurves)
    names(colors) <- names
  }
  
  #Lx
  plot.Lx.max <- max(Lx[eval.min:eval.max,],na.rm = TRUE)*1.1
  
  for(i in 1 : ncol(Lx)){
    temp.curve <- Lx[,i]
    temp.name <- names[i]
    temp.color <- colors[temp.name]
    
    if(i == 1) {
      plot(x=temperatures,
           y=temp.curve,
           type="l",
           col=temp.color,
           xlim=c(plot.Tmin,plot.Tmax),
           ylim=c(0,plot.Lx.max),
           main = "Signal",
           xlab = "Temperature (\u00b0C)",
           ylab = "Luminescence signal"
      )
      par(new = TRUE)
      
    }else{
      lines(x=temperatures,
            y=temp.curve,
            col=temp.color,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,plot.Lx.max)
      )
    }
  }
  abline(v=eval.Tmin,col=3,lty=3)
  abline(v=eval.Tmax,col=2,lty=3)
  
  par(new = FALSE)
  
  #Lx.plateau
  plot.Lx.plateau.max <- max(Lx.plateau[eval.min:eval.max,],na.rm = TRUE)*1.2
  
  for(i in 1 : ncol(Lx.plateau)){
    temp.curve <- Lx.plateau[,i]
    temp.name <- colnames(Lx.plateau)[i]
    temp.color <- colors[temp.name]
    
    if(i == 1) {
      plot(x=temperatures,
           y=temp.curve,
           type="l",
           col=temp.color,
           xlim=c(plot.Tmin,plot.Tmax),
           ylim=c(0,plot.Lx.plateau.max),
           main = "Heating plateau",
           xlab = "Temperature (\u00b0C)",
           ylab = "Lx/Ln"
      )
      par(new = TRUE)
      
    }else{
      lines(x=temperatures,
            y=temp.curve,
            col=temp.color,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,plot.Lx.plateau.max)
      )
    }
  }
  abline(v=eval.Tmin,col=3,lty=3)
  abline(v=eval.Tmax,col=2,lty=3)
  
  par(new = FALSE)
  
  #Page title
  page.title <- paste(plot.name)
  mtext(page.title, outer=TRUE,font = 2) 
  
  #clean layout...
  par(old.par)
  layout(matrix(c(1), 1, 1, byrow = TRUE))
}