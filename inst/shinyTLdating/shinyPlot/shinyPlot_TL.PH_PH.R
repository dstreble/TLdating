shinyPlot_TL.PH_PH <- function(
  
  PH.signal,
  PH.temperatures,
  PH.times
  
){
  
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  
  #Plot preheat
  if(length(PH.signal) > 0)
  {
    #Boundary
    plot.PH.Tmax <- max(PH.temperatures)
    plot.PH.Smax <- max(PH.times)
    plot.PH.Lmax <- max(PH.signal)
    
    #color
    colors <- 1:ncol(PH.signal)
    
    for(i in 1 : ncol(PH.signal)){
      temp.temperatures <- PH.temperatures[,i]
      temp.times <- PH.times[,i]
      temp.PH <- PH.signal[,i]
      temp.color <- colors[i]
      
      if(i == 1) {
        par(mar = c(5,5,4,5) )
        #Temperature
        plot(x=temp.times,
             y=temp.temperatures,
             xlim=c(0,plot.PH.Smax),
             ylim=c(0,plot.PH.Tmax),
             yaxt = "n",
             xaxt = "n",
             xlab = "",
             ylab = "",
             type="l",
             lty=2
        )
        axis(4)
        mtext(side = 4,
              text = "Temperature (\u00b0C)",
              line = 2.5,
              cex = 0.8
        )
        
        par(new = TRUE)
        
        plot(main= "Preheat signal",
             x=temp.times,
             y=temp.PH,
             xlim=c(0,plot.PH.Smax),
             ylim=c(0,plot.PH.Lmax),
             xlab="Time (s)",
             ylab = "Luminescence signal (PH)",
             type="l",
             col=temp.color
        )
        par(new = TRUE)
        
      }else{
        lines(x=temp.times,
              y=temp.PH,
              xlim=c(0,plot.PH.Smax),
              ylim=c(0,plot.PH.Lmax),
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