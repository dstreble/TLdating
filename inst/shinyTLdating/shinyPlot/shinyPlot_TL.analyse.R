shinyPlot_TL.analyse <- function(
  
  eval.Tmin = NA,
  eval.Tmax = NA,
  
  dTypes,
  
  temperatures,
  
  TL,
  
  plotting.parameters=list(plot.Tmin=0,
                           plot.Tmax=NA)
  
){
  nRecords <- ncol(temperatures)
  
  Tmax <- max(temperatures,na.rm = TRUE)
  nPoints <- nrow(temperatures)
  Tstep <- Tmax/nPoints
    
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  
  plot.min <- ceiling(plot.Tmin/Tstep)
  plot.max <-floor(plot.Tmax/Tstep)
  
  plot.TLmax <- max(TL[plot.min:plot.max,],na.rm = TRUE)
  
  
  colors <- seq(nRecords)

    
  plot(x=NULL,
       y=NULL,
       type="l",
       xlim=c(plot.Tmin,plot.Tmax),
       ylim=c(0,plot.TLmax),
       main = "TL signal",
       xlab = "Temperature (\u00b0C)",
       ylab = "Luminescence signal")
  par(new = TRUE)
  
  for(i in 1:nRecords){
    
    temp.temperature <- temperatures[,i]
    temp.curve <- TL[,i]
    temp.color <- colors[i]
    temp.dType <- dTypes[i]
    
    if(temp.dType == "Natural"){
      lines(x=temp.temperature,
            y=temp.curve,
            col=1,
            lwd=2,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,plot.TLmax))
    }else{
      lines(x=temp.temperature,
            y=temp.curve,
            col=temp.color,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,plot.TLmax))
    }

  }
    
    abline(v=eval.Tmin,col=3,lty=3)
    abline(v=eval.Tmax,col=2,lty=3)
    
    par(new = FALSE)
}