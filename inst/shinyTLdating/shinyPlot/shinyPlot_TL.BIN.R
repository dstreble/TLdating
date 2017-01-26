shinyPlot_TL.BIN <- function(
  
  dTypes,
  
  temperatures,
  
  TL
  
){
  
  plot.Tmin <- 0
  plot.Tmax <- max(unlist(temperatures),na.rm = TRUE)
  
  TL.max <- max(unlist(TL),na.rm = TRUE)
  
  nRecords <- length(temperatures)
  
  colors <- seq(nRecords)
  
  
  plot(x=NULL,
       y=NULL,
       type="l",
       xlim=c(plot.Tmin,plot.Tmax),
       ylim=c(0,TL.max),
       main = "TL signal",
       xlab = "Temperature (\u00b0C)",
       ylab = "Luminescence signal")
  par(new = TRUE)
  
  for(i in 1:nRecords){
    
    temp.temperature <- unlist(temperatures[[i]])
    temp.curve <- unlist(TL[[i]])
    temp.color <- colors[i]
    temp.dType <- dTypes[i]
    
    if(temp.dType == "Natural"){
      lines(x=temp.temperature,
            y=temp.curve,
            col=1,
            lwd=2,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,TL.max))
    }else if(temp.dType == "Preheat"){
      lines(x=temp.temperature,
            y=temp.curve,
            col=temp.color,
            lty=2,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,TL.max))
    }else if(temp.dType == "Background"){
      lines(x=temp.temperature,
            y=temp.curve,
            col=temp.color,
            lty=3,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,TL.max))
    }else{
      lines(x=temp.temperature,
            y=temp.curve,
            col=temp.color,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,TL.max))
    }
    
  }
  
  par(new = FALSE)
}