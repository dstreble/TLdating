shinyPlot_TL.PH_TL <- function(
  
  TL.signal,
  TL.temperatures  

){

  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  
  #Plot TL
  if(length(TL.signal) > 0)
  {
    #Boundary
    plot.TL.Tmax <- max(TL.temperatures)
    plot.TL.Lmax <- max(TL.signal)
    
    #color
    colors <- 1:ncol(TL.signal)
    
    for(i in 1 : ncol(TL.signal)){
      temp.temperatures <- TL.temperatures[,i]
      temp.TL <- TL.signal[,i]
      temp.color <- colors[i]
      
      if(i == 1) {
        plot(main= "Thermoluminescence signal",
             x=temp.temperatures,
             y=temp.TL,
             xlim=c(0,plot.TL.Tmax),
             ylim=c(0,plot.TL.Lmax),
             xlab="Temperature (\u00b0C)",
             ylab = "Luminescence signal (TL)",
             type="l",
             col=temp.color
        )
        par(new = TRUE)
        
      }else{
        lines(x=temp.temperatures,
              y=temp.TL,
              xlim=c(0,plot.TL.Tmax),
              ylim=c(0,plot.TL.Lmax),
              col=temp.color
        )
      }
    }
    par(new = FALSE)
  }

  #clean layout...
  layout(matrix(c(1), 1, 1, byrow = TRUE))
  par(old.par)
  
}