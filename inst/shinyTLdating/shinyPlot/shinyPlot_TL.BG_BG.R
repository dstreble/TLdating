shinyPlot_TL.BG_BG <- function(
  TL,
  BG,
  temperatures
){
  #Layout
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  layout(matrix(c(1,2), 1, 2, byrow = TRUE))
  
  #Plot TL (TL)
  #Boundary
  plot.Tmax <- max(temperatures)
  plot.TL.max <- max(TL)
  
  #color
  colors <- 1:ncol(TL)
  
  for(i in 1 : ncol(TL)){
    temp.TL <- TL[,i]
    temp.color <- colors[i]
    
    if(i == 1) {
      plot(main="TL before background substraction",
           x=temperatures,
           y=temp.TL,
           xlim=c(0,plot.Tmax),
           ylim=c(0,plot.TL.max),
           xlab="Temperature (C)",
           ylab="Luminescence signal (TL)",
           type="l",
           col=temp.color)
      
      par(new = TRUE)
      
    }else{
      lines(x=temperatures,
            y=temp.TL,
            xlim=c(0,plot.Tmax),
            ylim=c(0,plot.TL.max),
            col=temp.color)
    }
  }
  par(new = FALSE)
  
  #Plot BG
  #Boundary
  plot.Tmax <- max(temperatures)
plot.TL.max <- max(TL)
  
  #color
  colors <- 1:ncol(BG)
  
  for(i in 1 : ncol(BG)){
    temp.TL <- BG[,i]
    temp.color <- colors[i]
    
    if(i == 1) {
      plot(main="Background",
           x=temperatures,
           y=temp.TL,
           xlim=c(0,plot.Tmax),
           ylim=c(0,plot.TL.max),
           xlab="Temperature (C)",
           ylab="Luminescence signal (BG)",
           type="l",
           col=temp.color)
      
      par(new = TRUE)
      
    }else{
      lines(x=temperatures,
            y=temp.TL,
            xlim=c(0,plot.Tmax),
            ylim=c(0,plot.TL.max),
            col=temp.color)
    }
  }
  par(new = FALSE)
  
  #clean layout...
  layout(matrix(c(1), 1, 1, byrow = TRUE))
  par(old.par)
}