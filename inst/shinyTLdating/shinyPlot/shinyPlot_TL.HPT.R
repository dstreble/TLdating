shinyPlot_TL.HPT <- function(
  names,
  doses,
  temperatures,
  Lx,
  Lx.a,
  Lx.plateau,
  plotting.parameters=list(plateau.Tmin=0,
                           plateau.Tmax=NA,
                           plot.Tmin=0,
                           plot.Tmax=NA)
){
  
  nDoses <- ncol(Lx.a)
  Tmax <- max(temperatures)
  nPoints <- length(temperatures)
  Tstep <- Tmax/nPoints
  
  plateau.Tmin <- plotting.parameters$plateau.Tmin
  plateau.Tmax <- plotting.parameters$plateau.Tmax
  plot.Tmin <- plotting.parameters$plot.Tmin
  plot.Tmax <- plotting.parameters$plot.Tmax
  
  plateau.min <- ceiling(plateau.Tmin/Tstep)
  plateau.max <- ceiling(plateau.Tmax/Tstep)
  
  uNames <- unique(names)
  uDoses <- unique(doses)
  
  #----------------------------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------------------------
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )
  
  #---------------------------------------------------------------------------
  #Page 1
  #---------------------------------------------------------------------------
  if(length(Lx) > 0){
    #Layout
    layout(matrix(c(1,2), 1, 2, byrow = TRUE))
    
    #color
    ref_colors <- rainbow(n=length(uNames)-1)
    colors <- seq(length(uNames))
    names(colors) <- uNames
    colors[names(colors)=="N"] <- 1
    colors[names(colors)!="N"] <- ref_colors
    
    
    #Lx (additive)
    plot.Lx.max <- max(Lx[plateau.min:plateau.max,],na.rm = TRUE)*1.1
    
    for(i in 1 : length(uDoses)){
      temp.curve <- Lx.a[,i]
      temp.name <- colnames(Lx.a)[i]
      temp.color <- colors[temp.name]
      
      if(i == 1) {
        plot(x=temperatures,
             y=temp.curve,
             type="l",
             col=temp.color,
             xlim=c(plot.Tmin,plot.Tmax),
             ylim=c(0,plot.Lx.max),
             main = "Lx",
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
    
    for(i in 1: length(doses)){
      temp.curve <- Lx[,i]
      temp.name <- colnames(Lx)[i]
      temp.color <- colors[temp.name]
      
      lines(x=temperatures, type = "p",
            y=temp.curve,
            col=temp.color,
            xlim=c(plot.Tmin,plot.Tmax),
            ylim=c(0,plot.Lx.max),
            pch=18)
    }
    
    par(new = FALSE)
    
    #Lx.plateau
    plot.Lx.plateau.max <- max(Lx.plateau[plateau.min:plateau.max,],na.rm = TRUE)*1.2
    
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
             main = "Plateau test (Lx)",
             xlab = "Temperature (\u00b0C)",
             ylab = "Luminescence signal"
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
    par(new = FALSE)
    
    #Legend
    legend.size <- length(uDoses)
    legend.text <- c(uNames, uDoses)
    legend.color <- matrix(data = colors,
                           nrow = 2,
                           ncol = legend.size,
                           byrow=TRUE
    )
    
    legend <- matrix(data = legend.text,
                     nrow = 2,
                     ncol = legend.size,
                     byrow=TRUE,
                     dimnames = list(c("Names", "Doses (s)"),
                                     vector(mode = "character",length = legend.size)
                     )
    )
  }
  
  #clean layout...
  layout(1)
  par(old.par)
}