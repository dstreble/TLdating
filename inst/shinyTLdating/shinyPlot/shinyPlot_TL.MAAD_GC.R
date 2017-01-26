shinyPlot_TL.MAAD_GC <- function(
    temperatures,
    eval.Tmin,
    eval.Tmax,
    GC.Q.line,
    GC.Q.slope,
    GC.Q.LxTx,
    GC.Q.LxTx.error,
    GC.Q.doses,
    GC.Q.names,
    GC.I.line,
    GC.I.slope,
    GC.I.LxTx,
    GC.I.LxTx.error,
    GC.I.doses,
    GC.I.names,
    Q.DP,
    Q.DP.error,
    Q.GC,
    Q.GC.error,
    I.DP,
    I.DP.error,
    I.GC,
    I.GC.error,
    rejection.values,
    fitting.parameters,
    plotting.parameters=list(plot.Tmin=0,
                             plot.Tmax=NA)
  ){
    Tmax <- max(temperatures)
    nPoints <- length(temperatures)
    Tstep <- Tmax/nPoints
    eval.min <- ceiling(eval.Tmin/Tstep)
    eval.max <-floor(eval.Tmax/Tstep)
    
    uDoses <- unique(c(GC.Q.doses,GC.I.doses))
    
    fit.method <- fitting.parameters$fit.method
    fit.weighted <- fitting.parameters$fit.weighted
    fit.rDoses.min <- fitting.parameters$fit.rDoses.min
    fit.rDoses.max <- fitting.parameters$fit.rDoses.max
    
    plot.Tmin <- plotting.parameters$plot.Tmin
    plot.Tmax <- plotting.parameters$plot.Tmax
    
    #----------------------------------------------------------------------------------------------------------------
    #Plot results
    #----------------------------------------------------------------------------------------------------------------
    old.par <- par( no.readonly = TRUE )
    par( oma = c(0.5, 0, 3, 0 ) )
    
    #Layout
    layout(matrix(c(1,1,2,3), 1, 4, byrow = TRUE))
    
    # Plotting  GC (Q&I) ----------------------------------------
    
    # ylim & xlim
    
    if(length(GC.Q.LxTx) == 0 && length(GC.I.LxTx) == 0){
      plot.GC.ymax <- 0
    }else if(length(GC.Q.LxTx) == 0){
      plot.GC.ymax <- max(GC.I.LxTx+GC.I.LxTx.error,na.rm = TRUE)
    }else if(length(GC.I.LxTx) == 0){
      plot.GC.ymax <- max(GC.Q.LxTx+GC.Q.LxTx.error,na.rm = TRUE)
    }else{
      plot.GC.ymax <- max(GC.I.LxTx+GC.I.LxTx.error, GC.Q.LxTx+GC.Q.LxTx.error, na.rm = TRUE)
    }
    
    plot.GC.xmin <- -1.1*(Q.GC+Q.GC.error)
    plot.GC.xmax <- max(uDoses,na.rm = TRUE)
    
    # Additive curve
    
    plot(x=NA,
         y=NA,
         xlim = c(plot.GC.xmin,plot.GC.xmax),
         ylim = c(0,plot.GC.ymax),
         main = "D\u2091 growth curves",
         sub = paste(if(length(GC.Q.LxTx) > 0){paste("Q (GC) =",
                                               paste(round(Q.GC, digits = 2),
                                                     "\u00b1",
                                                     round(Q.GC.error, digits = 2)),
                                               paste("(",
                                                     round(Q.GC.error/Q.GC*100, digits = 2),
                                                     "%)",
                                                     sep = ""))},
                     if(length(GC.Q.LxTx) > 0 && length(GC.I.LxTx) > 0){"|"},
                     if(length(GC.I.LxTx) > 0){paste("I (GC) = ",
                                               paste(round(I.GC, digits = 2),
                                                     "\u00b1",
                                                     round(I.GC.error, digits = 2)),
                                               paste("(",
                                                     round(I.GC.error/I.GC*100, digits = 2),
                                                     "%)",
                                                     sep = ""))}),
         xlab = "Dose (s)",
         ylab = "Signal (Lx/Tx)",
         type = "p",
         pch = 18,
         col = 1)
    
    par(new = TRUE)
    
    if(length(GC.Q.LxTx)>0){
      
      temp.bool <- GC.Q.doses != 0
      points(x = GC.Q.doses[temp.bool],
             y = GC.Q.LxTx[temp.bool],
             pch=18,
             col=1)
      
      par(new = TRUE)
      
      # Natural
      temp.bool <- GC.Q.doses == 0
      points(x = GC.Q.doses[temp.bool],
             y = GC.Q.LxTx[temp.bool],
             pch = 18,
             col = 8)
      
      # error on aLxTx
      arrows(GC.Q.doses,
             GC.Q.LxTx-GC.Q.LxTx.error,
             GC.Q.doses,
             GC.Q.LxTx+GC.Q.LxTx.error,
             length=0, #0.05,
             angle=90,
             code=3)
      
      # linear regression
      if(length(GC.Q.line)>0){
        abline(GC.Q.line)
        
        # Q.GC
        points(x=-Q.GC,
               y=0,
               pch=18,
               col=3)
        # error on Q.GC
        arrows(-Q.GC-Q.GC.error,
               0,
               -Q.GC+Q.GC.error,
               0,
               length=0.05,
               angle=90,
               code=3)
        
        # Q.DP
        points(x=-Q.DP,
               y=0,
               pch=18,
               col=2)
      }
      
    }else{
      plot(x=NA,
           y=NA,
           xlim = c(plot.GC.xmin,plot.GC.xmax),
           ylim = c(0,plot.GC.ymax),
           main = "Palaeodose (s)",
           xlab = "Dose (s)",
           ylab = "Signal (Lx/Tx)",
           type = "p",
           pch = 18,
           col = 1)
      
      par(new = TRUE)
    }
    
    # Regenerative curve
    if(length(GC.I.LxTx)>0){
      # rLxTx
      temp.bool <- GC.I.doses < fit.rDoses.min | GC.I.doses > fit.rDoses.max
      points(x = GC.I.doses[temp.bool],
             y = GC.I.LxTx[temp.bool],
             pch = 18,
             col = 5)
      
      temp.bool <- !temp.bool
      points(x = GC.I.doses[temp.bool],
             y = GC.I.LxTx[temp.bool],
             pch = 18,
             col = 4)
      
      #error on rLxTx
      arrows(GC.I.doses,
             GC.I.LxTx-GC.I.LxTx.error,
             GC.I.doses,
             GC.I.LxTx+GC.I.LxTx.error,
             length=0, #0.05,
             angle=90,
             code=3)
      
      # linear regression
      if(length(GC.I.line)>0){
        abline(GC.I.line)
        
        # I.GC
        points(x = I.GC,
               y = 0,
               pch = 18,
               col = 3)
        # error on I.GC
        arrows(I.GC-I.GC.error,
               0,
               I.GC+I.GC.error,
               0,
               length = 0.05,
               angle = 90,
               code = 3)
        
        # I
        points(x = I.DP,
               y = 0,
               pch = 18,
               col = 2)
      }
    }
    
    # Legend
    legend(x = "topleft",
           legend = c(if(length(GC.Q.LxTx)>0){c("Natural", "Natural + \u03b2")},
                      if(length(GC.I.LxTx)>0){c("REG points (not used)", "REG points (used)")},
                      if(length(GC.Q.LxTx)>0){c("Q (DP)", "Q (GC)")},
                      if(length(GC.I.LxTx)>0){c("I (DP)", "I (GC)")}),
           pch = c(if(length(GC.Q.LxTx)>0){c(18, 18)},
                   if(length(GC.I.LxTx)>0){c(18, 18)},
                   if(length(GC.Q.LxTx)>0){c(18, 18)},
                   if(length(GC.I.LxTx)>0){c(18, 18)}),
           col = c(if(length(GC.Q.LxTx)>0){c(8,1)},
                   if(length(GC.I.LxTx)>0){c(5,4)},
                   if(length(GC.Q.LxTx)>0){c(2,3)},
                   if(length(GC.I.LxTx)>0){c(2,3)}),
           bty = "n")
    
    par(new = FALSE)
    
    
    #Rejection criteria ----------------------------------------
    rejection.title <- "Rejection criteria"
    
    rejection.text <- c("Q:",
                        "",
                        "Lx error (max):",
                        paste(round(rejection.values$aLx.error.max*100,digits = 2),"%",sep = ""),
                        "Tx error (max):",
                        paste(round(rejection.values$aTx.error.max*100,digits = 2),"%",sep = ""),
                        "I:",
                        "",
                        "Lx error (max):",
                        paste(round(rejection.values$rLx.error.max*100,digits = 2),"%",sep = ""),
                        "Tx error (max):",
                        paste(round(rejection.values$rTx.error.max*100,digits = 2),"%",sep = ""))
    
    rejection <- matrix(data = rejection.text,
                        nrow = 6,
                        ncol = 2,
                        byrow=TRUE)
    
    rejection.color <- matrix(data = c(6,6,
                                       if(rejection.values$test.aLx.error){1}else{2}, if(rejection.values$test.aLx.error){1}else{2},
                                       if(rejection.values$test.aTx.error){1}else{2}, if(rejection.values$test.aTx.error){1}else{2},
                                       4,4,
                                       if(rejection.values$test.rLx.error){1}else{2}, if(rejection.values$test.rLx.error){1}else{2},
                                       if(rejection.values$test.rTx.error){1}else{2}, if(rejection.values$test.rTx.error){1}else{2}),
                              nrow = 6,
                              ncol = 2,
                              byrow=TRUE)
    
    textplot(object=rejection,
             col.data=rejection.color,
             cex=1.2,
             halign="center",
             valign="top",
             show.colnames= FALSE,
             show.rownames= FALSE)
    
    title(rejection.title)
    
    # Curve fitting -----------------------------------------------------
    
    if(fit.method == "LIN"){
      fitting.title <- paste("Curve fitting (GC):",
                             "Linear",
                             if(fit.weighted){"(weighted)"},
                             "\n",
                             "y = a + bx")
      
      if(length(GC.Q.line)>0){
        fitting.text <- c("a (Q) =",
                          paste(format(GC.Q.slope$a, digits = 3, scientific = TRUE), "\u00b1", format(GC.Q.slope$a.error, digits = 3, scientific = TRUE)),
                          "b (Q) =",
                          paste(format(GC.Q.slope$b, digits = 3, scientific = TRUE), "\u00b1", format(GC.Q.slope$b.error, digits = 3, scientific = TRUE))
        )
        fitting <- matrix(data = fitting.text,
                          nrow = 2,
                          ncol = 2,
                          byrow=TRUE)
        
        fitting.color <- matrix(data = c(1,1,
                                         1,1),
                                nrow = 2,
                                ncol = 2,
                                byrow=TRUE)
        
      }else if(length(GC.I.line)>0){
        fitting.text <- c("a (I) =",
                          paste(format(GC.I.slope$a, digits = 3, scientific = TRUE), "\u00b1", format(GC.I.slope$a.error, digits = 3, scientific = TRUE)),
                          "b (I) =",
                          paste(format(GC.I.slope$b, digits = 3, scientific = TRUE), "\u00b1", format(GC.I.slope$b.error, digits = 3, scientific = TRUE))
        )
        fitting <- matrix(data = fitting.text,
                          nrow = 2,
                          ncol = 2,
                          byrow=TRUE)
        
        fitting.color <- matrix(data = c(1,1,
                                         1,1),
                                nrow = 2,
                                ncol = 2,
                                byrow=TRUE)
      }
    }
    
    textplot(object= fitting,
             col.data = fitting.color,
             cex = 1.2,
             halign = "center",
             valign="top",
             show.colnames= FALSE,
             show.rownames= FALSE)
    
    title(main = fitting.title)

    
    #clean layout...
    par(old.par)
    layout(matrix(c(1), 1, 1, byrow = TRUE))
    
  }