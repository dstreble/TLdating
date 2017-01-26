shinyPlot_TL.SAR_GC <- function(

  fitting.parameters=list(fit.method="LIN",
                          fit.weighted=FALSE,
                          fit.rDoses.min=NA,
                          fit.rDoses.max=NA),
  
  eval.Tmin,
  eval.Tmax,
  
  temperatures,
  
  names,
  names.duplicated,
  doses,
  
  GC.Q.line,
  GC.Q.LxTx,
  GC.Q.LxTx.error,
  GC.Q.slope,
  
  Q.DP,
  Q.DP.error,
  Q.GC,
  Q.GC.error,
  
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
  
  fit.method <- fitting.parameters$fit.method
  fit.weighted <- fitting.parameters$fit.weighted
  
  #----------------------------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------------------------
  
  
  old.par <- par( no.readonly = TRUE )
  par( oma = c( 0, 0, 3, 0 ) )
  
  # --------------------------------------------------------------------
  #page 2
  # --------------------------------------------------------------------
  
  #Layout
  layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))
  
  # Plotting  Growth curve (GC) ----------------------------------------
  doses.nat <- doses[names=="N"]
  LxTx.nat <- GC.Q.LxTx[names=="N"]
  LxTx.nat.error <- GC.Q.LxTx.error[names=="N"]
  
  doses.0 <- doses[names!="N" & doses==0]
  LxTx.0 <- GC.Q.LxTx[names!="N" & doses==0]
  LxTx.0.error <- GC.Q.LxTx.error[names!="N" & doses==0]
  
  doses.dupl <- doses[names %in% names.duplicated]
  LxTx.dupl <- GC.Q.LxTx[names %in% names.duplicated]
  LxTx.dupl.error <- GC.Q.LxTx.error[names %in% names.duplicated]
  
  doses.r <- doses[doses!=0 & !(names %in% names.duplicated)]
  LxTx.r <- GC.Q.LxTx[doses!=0 & !(names %in% names.duplicated)]
  LxTx.r.error <- GC.Q.LxTx.error[doses!=0 & !(names %in% names.duplicated)]
  
  #LxTx
  par(mar=c(5,4,4,1))
  plot(x=doses.r,
       y=LxTx.r,
       xlim=c(0, max(doses)),
       ylim=c(0, max(LxTx.r)),
       main = "Growth curve",
       sub = paste("D\u2091 (GC) =",
                   round(Q.GC, digits = 2), "\u00b1", round(Q.GC.error, digits = 2),
                   paste( "(", round(Q.GC.error/Q.GC*100, digits = 2), "%)",sep = "")),
       xlab = "Doses",
       ylab = "Lx/Tx",
       type="p",
       pch=18,
       col=1)
  par(new = TRUE)
  
  # error
  arrows(doses.r,
         LxTx.r-LxTx.r.error,
         doses.r,
         LxTx.r+LxTx.r.error,
         length=0, #0.05,
         angle=90,
         code=3)
  
  # R0
  points(x=doses.0,
         y=LxTx.0,
         pch=5,
         col=1
  )
  # error
  arrows(doses.0,
         LxTx.0-LxTx.0.error,
         doses.0,
         LxTx.0+LxTx.0.error,
         length=0, #0.05,
         angle=90,
         code=3)
  
  
  # Duplicated
  points(x=doses.dupl,
         y=LxTx.dupl,
         pch=2)
  # error
  arrows(doses.dupl,
         LxTx.dupl-LxTx.dupl.error,
         doses.dupl,
         LxTx.dupl+LxTx.dupl.error,
         length=0, #0.05,
         angle=90,
         code=3)
  
  # Natural
  points(x=0,
         y=LxTx.nat,
         pch=18,
         col=3)
  # error
  arrows(x0 = 0,
         y0 = LxTx.nat-LxTx.nat.error,
         x1 = 0,
         y1 = LxTx.nat+LxTx.nat.error,
         length = 0, #0.05,
         angle = 90,
         code=3)
  
  # Q.GC
  points(x=Q.GC,
         y=0,
         pch=18,
         col=3)
  # error
  arrows(Q.GC-Q.GC.error,
         0,
         Q.GC+Q.GC.error,
         0,
         length=0, #0.05,
         angle=90,
         code=3)
  
  # Q
  points(x=Q.DP,
         y=0,
         pch=1,
         col=2)
  
  #Growth curve
  abline(GC.Q.line)
  
  # crossing
  points(x=Q.GC,
         y=LxTx.nat,
         pch=18,
         col=3)
  
  segments(x0=0,
           y0=LxTx.nat,
           x1=Q.GC,
           y1=LxTx.nat,
           col=3)
  
  segments(x0=Q.GC,
           y0=LxTx.nat,
           x1=Q.GC,
           y1=0,
           col=3)
  
  # Legend
  legend(x = "topleft",
         legend = c("REG points", "REG point repeated", "REG point 0", "D\u2091 (GC)", "D\u2091"),
         pch = c(18,2,5,18,5),
         col = c(1,1,1,3,2),
         bty = "n")
  
  par(new = FALSE)
  
  # Plotting  rejection criteria ----------------------------------------
  rejection.title <- "Rejection criteria"
  
  #rejection test
  rejection.text <- vector()
  for(i in 1: length(recycling.ratio)){
    if(i==1){
      temp.text <- "Recycling ratio:"
    }else{
      temp.text <- ""
    }
    
    temp.ratio <- format(recycling.ratio[i],digits = 2)
    
    temp.name <-  paste("(", names(recycling.ratio[i]), ")", sep="")
    
    rejection.text <- c(rejection.text, temp.text, temp.ratio, temp.name)
  }
  
  for(i in 1: length(recuperation.rate)){
    if(i==1){
      temp.text <- "Recuperation rate:"
    }else{
      temp.text <- ""
    }
    
    temp.rate <- paste(format(recuperation.rate[i]*100,digits = 2),"%",sep="")
    
    temp.name <-  paste("(", names(recuperation.rate[i]), ")", sep="")
    
    rejection.text <- c(rejection.text, temp.text, temp.rate, temp.name)
  }
  
  rejection.text <- c(rejection.text,
                      "Lx error (max):",
                      paste(format(Lx.error.max*100,digits = 2),"%",sep = ""),
                      "")
  
  rejection.text <- c(rejection.text,
                      "Tx error (max):",
                      paste(format(Tx.error.max*100,digits = 2),"%",sep = ""),
                      "")
  
  #rejection color
  data.color <- vector()
  
  for(i in 1: length(test.recycling)){
    if(test.recycling[i]){
      temp.color <- 1
    }else{
      temp.color <- 2
    }
    data.color <- c(data.color,temp.color,temp.color,temp.color)
  }
  
  for(i in 1: length(test.recuperation)){
    if(test.recuperation[i]){
      temp.color <- 1
    }else{
      temp.color <- 2
    }
    data.color <- c(data.color,temp.color,temp.color,temp.color)
  }
  
  if(test.Lx.error){
    temp.color <- 1
  }else{
    temp.color <- 2
  }
  data.color <- c(data.color,temp.color,temp.color,temp.color)
  
  if(test.Tx.error){
    temp.color <- 1
  }else{
    temp.color <- 2
  }
  data.color <- c(data.color,temp.color,temp.color,temp.color)
  
  
  rejection.color <- matrix(data = data.color,
                            nrow = length(data.color)/3,
                            ncol = 3,
                            byrow=TRUE)
  
  rejection <- matrix(data = rejection.text,
                      nrow = length(rejection.text)/3,
                      ncol = 3,
                      byrow = TRUE
  )
  
  textplot(object=rejection,
           col.data=rejection.color,
           cex=1.2,
           halign="center",
           valign="top",
           show.colnames= FALSE,
           show.rownames= FALSE
  )
  title(rejection.title)
  
  # Curve fitting -----------------------------------------------------
  
  if(fit.method == "LIN"){
    fitting.title <- paste("Curve fitting (GC):", "\n",
                           "Linear", if(fit.weighted){"(weighted)"}, "\n",
                           "y = a + bx")
    
    fitting.text <- c("a =",
                      paste(format(GC.Q.slope$a, digits = 3, scientific = TRUE),
                            "\u00b1",
                            format(GC.Q.slope$a.error, digits = 3, scientific = TRUE)),
                      "b =",
                      paste(format(GC.Q.slope$b, digits = 3, scientific = TRUE),
                            "\u00b1",
                            format(GC.Q.slope$b.error, digits = 3, scientific = TRUE)))
    
    fitting <- matrix(data = fitting.text,
                      nrow = 2,
                      ncol = 2,
                      byrow=TRUE)
    
    fitting.color <- matrix(data = c(1,1,
                                     1,1),
                            nrow = 2,
                            ncol = 2,
                            byrow=TRUE)
    
  }else if(fit.method == "EXP"){
    fitting.title <- paste("Curve fitting (GC):", "\n",
                           "Exponential", if(fit.weighted){"(weighted)"}, "\n",
                           "y = a.(1-exp(-(x+c)/b))")
    
    fitting.text <- c("a =",
                      paste(format(GC.Q.slope$a, digits = 3, scientific = TRUE),
                            "\u00b1",
                            format(GC.Q.slope$a.error, digits = 3, scientific = TRUE)),
                      "b =",
                      paste(format(GC.Q.slope$b, digits = 3, scientific = TRUE),
                            "\u00b1",
                            format(GC.Q.slope$b.error, digits = 3, scientific = TRUE)),
                      "c =",
                      paste(format(GC.Q.slope$c, digits = 3, scientific = TRUE),
                            "\u00b1",
                            format(GC.Q.slope$c.error, digits = 3, scientific = TRUE)))
    
    
    fitting <- matrix(data = fitting.text,
                      nrow = 3,
                      ncol = 2,
                      byrow=TRUE)
    
    fitting.color <- matrix(data = c(1,1,
                                     1,1,
                                     1,1),
                            nrow = 3,
                            ncol = 2,
                            byrow=TRUE)
  }else{
    fitting.title <- paste("Curve fitting (GC):", "\n",
                           "Unknown")
    
    textplot("")
    
    title(main = fitting.title)
  }
  
  textplot(object= fitting,
           col.data = fitting.color,
           cex = 1.2,
           halign = "center",
           valign="top",
           show.colnames= FALSE,
           show.rownames= FALSE)
  
  title(main = fitting.title)
  
  # --------------------------------------------------------------------
  # Clean layout
  par(old.par)
  layout(matrix(c(1), 1, 1, byrow = TRUE))
  
}