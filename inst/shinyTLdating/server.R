
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny)
require(shinyjs)
require(gplots)
require(DT)
require(xtable)

require(Luminescence)
require(TLdating)
#require(tools)

source(file = "shinyPlot_TL.R")


shinyServer(function(input, output, session) {
  
  #Remove error when closing app...
  session$onSessionEnded(stopApp)
  
  ##############################################################################################
  # DATA
  ##############################################################################################
  
  DATA.values <- reactiveValues()

  ##############################################################################################
  # Sample information
  ##############################################################################################

  output$infoPage <- renderUI({
    sidebarLayout(
      sidebarPanel(h4("General information"),
                   textInput(inputId = "sa_project",
                             label = "Project",
                             placeholder = "required"),
                   textInput(inputId = "sa_site",
                             label = "Site",
                             placeholder = "required"),
                   dateInput(inputId = "sa_date",
                             label = "Sampling year",
                             format = "yyyy",
                             startview = "decade"),
                   textInput(inputId = "sa_sample",
                             label = "Sample name",
                             placeholder = "required")

                   ),
      mainPanel(div("Welcome in ShinyTLdating")
                )
      )
  })
  
  ##############################################################################################
  # De Estimation
  ##############################################################################################

  output$dePage <- renderUI({
    tabsetPanel(id = "dePage",
                tabPanel("File",
                         uiOutput(outputId = "de_fileTab")),
                tabPanel("Pretreatment",
                         uiOutput(outputId = "de_pretreatmentTab")),
                tabPanel("Analyse",
                         uiOutput(outputId = "de_analyseTab")),
                tabPanel("D\u2091",
                         uiOutput(outputId = "de_deTab"))
    )
  })

  #############################################
  # File
  #############################################

  output$de_fileTab <- renderUI({
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "de_fileSidebar")
      ),
      mainPanel(
        uiOutput(outputId = "de_fileMain")
      )
    )
  })

  output$de_fileSidebar <- renderUI({
    fluidRow(column(width = 12,
                    selectInput(inputId = "de_protocol",
                                label = "Protocol",
                                choices = list("HPT", "MAAD", "SAR"),
                                selected = "MAAD"),

                    selectInput(inputId = "de_extension",
                                label = "File extension",
                                choices = list(".binx", ".bin"),
                                selected = ".binx"),

                    fileInput(inputId = "de_file",
                              label = "Import file",
                              accept = c(".bin", ".binx")),

                    numericInput(inputId = "de_k",
                                 label = "Parameter for the uncertainties (k)",
                                 value = 1,
                                 step = 0.1,
                                 min = 0),

                    actionButton(inputId = "de_loadButton",
                                 label = "Generate data"),
                    
                    uiOutput(outputId = "de_fileText")
                    ))
  })


  output$de_fileMain <- renderUI({
    
    bin <- de_DATA.bin()

    if(!is(bin,"TLum.Analysis")){
      return(NULL)
    }

    dTypes <- character()
    temperatures <- list()
    TL <- list()
    
    records <- bin@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      
      temp.temperatures <- list(temp.record@temperatures)
      temp.TL <- list(temp.record@data)
      
      dTypes <- c(dTypes,temp.dTypes)
      temperatures <- c(temperatures, temp.temperatures)
      TL <- c(TL, temp.TL)
    }
    
    plotData <- list(dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BIN,plotData)})
    ))

  })

  output$de_fileText <- renderUI({
    
    risoe <- de_DATA.Risoe()
    bin <- de_DATA.bin()

    if(is.null(risoe)){
      helpText("No data")
      
    }else if(is.null(bin)){
      helpText("Waiting for data generation")

    }else{
      helpText("Data generated")
    }
  })

  de_DATA.bin <- reactive({

    temp <- input$de_loadButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- de_generate.data()
    }
    return(data)
  })

  de_generate.data <- eventReactive(input$de_loadButton,{

    old.bin <- de_DATA.Risoe()

    if(is.null(old.bin)){
      return(NULL)
    }

    k <- as.numeric(input$de_k)
    protocol <- as.character(input$de_protocol)

    plotting.parameters <- list(no.plot=TRUE)

    bin <- Risoe.BINfileData2TLum.BIN.File(object = old.bin,
                                           k = k)

    data <- TLum.BIN.File2TLum.Analysis(object = bin,
                                        protocol = protocol)

    data <- mod_extract.TL(data,
                           plotting.parameters = plotting.parameters)

    data <- mod_update.dType(data)

    return(data)
  })

  
  de_DATA.Risoe <- reactive({

    path <- input$de_file$datapath
    ext <- as.character(input$de_extension)

    if(is.null(path)){
      return(NULL)
    }

    new.path <- paste(path, ext, sep="")

    file.rename(from = path,
                to = new.path)

    new.bin <- read_BIN2R(new.path)

    return(new.bin)

  })

  de_DATA.info <- reactive({

    protocol <- as.character(input$de_protocol)
    
    if(is.null(protocol)){
      return(NULL)
    }
    
    data <- de_DATA.bin()

    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    nRecords <- length(data@records)
    
    Tmin <- 0
    Tmax <- 0
    
    Da.min <- numeric()
    Da.max <- numeric()
    Dr.min <- numeric()
    Dr.max <- numeric()
    
    positions <- numeric()
    s2Gy <- numeric()
    s2Gy_err <- numeric()

    for(i in 1:nRecords){
      temp.record <- data@records[[i]]
      temp.metadata <- temp.record@metadata

      temp.Tmax <- max(temp.record@temperatures)
      temp.position <- as.numeric(temp.metadata$POSITION)

      temp.dtype <- as.character(temp.metadata$DTYPE)
      temp.dose <- as.numeric(temp.metadata$IRR_TIME)
      
      temp.s2Gy <- as.numeric(temp.metadata$IRR_DOSERATE)
      temp.s2Gy_err <- as.numeric(temp.metadata$IRR_DOSERATEERR)
      
      Tmax <- max(Tmax, temp.Tmax)
      positions <- c(positions,temp.position)
      
      if(protocol == "MAAD"){
        if(temp.dtype == "Natural"){
          Da.min <- 0
          Da.max <- max(Da.max, 0)
        }else if(temp.dtype == "N+dose"){
          Da.min <- min(Da.min, temp.dose)
          Da.max <- max(Da.max, temp.dose)
          
        }else if(temp.dtype == "Bleach"){
          Dr.min <- 0
          Dr.max <- max(Dr.max, 0)
        }else if(temp.dtype == "Bleach+dose"){
          Dr.min <- min(Dr.min, temp.dose)
          Dr.max <- max(Dr.max, temp.dose)
        }

      }else if(protocol == "SAR"){
        if(temp.dtype == "Dose"){
          Dr.min <- min(Dr.min, temp.dose)
          Dr.max <- max(Dr.max, temp.dose)
        }
      }
      
      s2Gy <- c(s2Gy, temp.s2Gy)
      s2Gy_err <- c(s2Gy_err, temp.s2Gy_err)
    }

    positions <- unique(positions)
    nDiscs <- length(positions)

    s2Gy <- mean(s2Gy)
    s2Gy_err <- mean(s2Gy_err)
    
    info <- list(protocol = protocol,
                 nRecords = nRecords,
                 nDiscs = nDiscs,
                 positions = positions,
                 Tmin = Tmin,
                 Tmax = Tmax,
                 Da.min = Da.min,
                 Da.max = Da.max,
                 Dr.min = Dr.min,
                 Dr.max = Dr.max,
                 s2Gy = s2Gy,
                 s2Gy_err = s2Gy_err)

    return(info)
  })


  #############################################
  # Pretreatment
  #############################################

  output$de_pretreatmentTab <- renderUI({
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "de_interestSlider"),

        checkboxGroupInput(inputId = "de_pretreatmentSelection",
                           label = "Preatreatments",
                           choices = c("remove disc", "remove preheat", "substract background", "align peak"),
                           selected = c("remove preheat", "substract background", "align peak")
                              ),

        uiOutput(outputId = "de_removeOption"),

        uiOutput(outputId = "de_alignOption"),

        actionButton(inputId = "de_pretreatmentButton",
                     label = "Apply pretreatment"),

        uiOutput(outputId = "de_pretreatmentText")
      ),

      mainPanel(
        tabsetPanel(id = "de_pretreatmentTabset",
                    tabPanel("Data",
                             uiOutput(outputId = "de_pretreatmentPlot")),

                    tabPanel("Remove preheats",
                             uiOutput(outputId = "de_preheatPlot")),
                    tabPanel("Substract background",
                             uiOutput(outputId = "de_backgroundPlot")),
                    tabPanel("Align peaks",
                             uiOutput(outputId = "de_alignPlot"))
                    )
        )
      )
  })

  output$de_interestSlider <- renderUI({

    info <- de_DATA.info()

    if(is.null(info)){
      sliderInput("de_interestSlider",
                  label = "Region of interest [°C]",
                  min = 0,
                  max = 500,
                  value = c(0,500))
    }else{
      temp.T <- c(info$Tmin, info$Tmax)

      if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
        Tmin <- 0
        Tmax <- 500
      }else{
        Tmin <- temp.T[1]
        Tmax <- temp.T[2]
      }

      sliderInput("de_interestSlider",
                  label = "Region of interest [°C]",
                  min = Tmin,
                  max = Tmax,
                  value = c(Tmin,Tmax))
    }


  })

  output$de_removeOption <- renderUI({

    pretreatments <- input$de_pretreatmentSelection

    if("remove disc" %in% pretreatments){
      fluidRow(column(width = 11,
                      offset = 1,
                      textInput("de_removeList",
                                label = "discs to remove",
                                placeholder = "1 2 3 4 5 6 7 8 9 0 , ; -")
      ))
    }
  })

  output$de_alignOption <- renderUI({

    pretreatments <- input$de_pretreatmentSelection

    temp.T <- as.numeric(input$de_interestSlider)

    if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
      Tmin <- 0
      Tmax <- 500
    }else{
      Tmin <- temp.T[1]
      Tmax <- temp.T[2]
    }

    if("align peak" %in% pretreatments){
      fluidRow(column(width = 11,
                      offset = 1,
                      sliderInput("de_alignSlider",
                                  label = "Peak position [°C]",
                                  min = Tmin,
                                  max = Tmax,
                                  value = c(Tmin,Tmax)),
                      checkboxInput("de_alignTest",
                                    label = "Do NOT rely on the test doses for the alignment.",
                                    value = FALSE))
               )
    }

  })

  de_remove.list <- reactive({

    list.IN <- input$de_removeList

    temp.list <- strsplit(list.IN,split = c(",", ";", " "))[[1]]

    new.list <- numeric()

    for(i in 1:length(temp.list)){
      temp.val <- temp.list[i]

      if(grepl("-", temp.val)){
        temp.val <- strsplit(temp.val,split = "-")[[1]]
        temp.min <- min(as.numeric(temp.val))
        temp.max <- max(as.numeric(temp.val))

        temp.out <- seq(temp.min, temp.max)
      }else{
        temp.out <- as.numeric(temp.val)
      }

      new.list <- c(new.list, temp.out)
    }

    return(new.list)
  })

  de_DATA.pretreatment <- reactive({

    temp <- input$de_pretreatmentButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- de_generate.pretreatment()
    }
    return(data)
  })
  
  de_generate.pretreatment <- eventReactive(input$de_pretreatmentButton,{

    data <- de_DATA.bin()

    if(is.null(data)){
      return(NULL)
    }

    pretreatments <- input$de_pretreatmentSelection

    # Parameters
    plot.Tmin <- input$de_interestSlider[1]
    plot.Tmax <- input$de_interestSlider[2]

    plotting.parameters <- list(plot.Tmin=plot.Tmin,
                                plot.Tmax=plot.Tmax,
                                no.plot=TRUE)

    if("remove disc" %in% pretreatments){
      remove.list <- de_remove.list()

      data <- mod_remove.aliquot(data,
                                 list = remove.list)
    }

    if("remove preheat" %in% pretreatments){
      data <- mod_remove.preheat(data,
                                 plotting.parameters = plotting.parameters)
      
      updateTabsetPanel(session,"de_pretreatmentTabset","Remove preheats")
    }

    if("substract background" %in% pretreatments){
      data <- mod_substract.background(data,
                                       plotting.parameters = plotting.parameters)
      
      updateTabsetPanel(session,"de_pretreatmentTabset","Substract background")
    }

    if("align peak" %in% pretreatments){
     peak.Tmin <- input$de_alignSlider[1]
     peak.Tmax <- input$de_alignSlider[2]

     no.testdose <- input$de_alignTest

     aligning.parameters <- list(peak.Tmin=peak.Tmin,
                                 peak.Tmax=peak.Tmax,
                                 no.testdose=no.testdose)

     data <- mod_align.peaks(data,
                             aligning.parameters = aligning.parameters,
                             plotting.parameters = plotting.parameters)
     
     updateTabsetPanel(session,"de_pretreatmentTabset","Align peaks")
    }
    
    return(data)
  })

  output$de_pretreatmentPlot <- renderUI({

    bin <- de_DATA.bin()
    
    if(!is(bin,"TLum.Analysis")){
      return(NULL)
    }
    
    dTypes <- character()
    temperatures <- list()
    TL <- list()
    
    records <- bin@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      
      temp.temperatures <- list(temp.record@temperatures)
      temp.TL <- list(temp.record@data)
      
      dTypes <- c(dTypes,temp.dTypes)
      temperatures <- c(temperatures, temp.temperatures)
      TL <- c(TL, temp.TL)
    }
    
    plotData <- list(dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BIN,plotData)})
    ))
  })

  output$de_preheatPlot <- renderUI({
    data <- de_DATA.pretreatment()

    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }

    history <- data@history

    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]

      if(temp.mod == "mod_remove.preheat"){
        plotData <- data@plotHistory[[i]]
      }
    }

    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.PH <- list(PH.signal = plotData$PH.signal,
                    PH.temperatures = plotData$PH.temperatures,
                    PH.times = plotData$PH.times)
    
    plotData.TL <- list(TL.signal = plotData$TL.signal,
                    TL.temperatures = plotData$TL.temperatures)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.PH_PH,plotData.PH)}),
                    renderPlot({do.call(shinyPlot_TL.PH_TL,plotData.TL)})
                    ))
  })

  output$de_backgroundPlot <- renderUI({
    data <- de_DATA.pretreatment()

    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    history <- data@history
    
    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]
      
      if(temp.mod == "mod_substract.background"){
        plotData <- data@plotHistory[[i]]
      }
    }
    
    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.BG <- list(TL = plotData$old.TL,
                        BG = plotData$BG,
                        temperatures = plotData$temperatures)
    
    plotData.TL <- list(TL = plotData$new.TL,
                        temperatures = plotData$temperatures)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BG_BG,plotData.BG)}),
                    renderPlot({do.call(shinyPlot_TL.BG_TL,plotData.TL)})
    ))
  })

  output$de_alignPlot <- renderUI({

    data <- de_DATA.pretreatment()

    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    history <- data@history
    
    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]
      
      if(temp.mod == "mod_align.peaks"){
        plotData <- data@plotHistory[[i]]
      }
    }
    
    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.peak <- list(temperatures = plotData$temperatures,
                          TL = plotData$old.TL,
                          Tx = plotData$ref.TL,
                          pos.peak = plotData$pos.peak,
                          plotting.parameters = plotData$plotting.parameters)
    
    plotData.TL <- list(temperatures = plotData$temperatures,
                        TL = plotData$new.TL,
                        pos.peak = plotData$pos.peak,
                        plotting.parameters = plotData$plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.AP_peak,plotData.peak)}),
                    renderPlot({do.call(shinyPlot_TL.AP_TL,plotData.TL)})
    ))
  })

  output$de_pretreatmentText <- renderUI({
    
    bin <- de_DATA.bin()
    pretreatment <- de_DATA.pretreatment()

    if(is.null(bin)){
      helpText("No data")

    }else if(is.null(pretreatment)){
      helpText("Waiting for pretreatment")

    }else{
      helpText("Pretreatment done")
    }
  })

  #############################################
  # De
  #############################################

  output$de_analyseTab <- renderUI({

    protocol <- as.character(input$de_protocol)

    if(protocol == "HPT"){
      sidebarLayout(
        sidebarPanel(
          h4(input$de_protocol),
          
          uiOutput(outputId = "de_evalSlider"),
          
          actionButton(inputId = "de_analyseButton",
                       label = "Start heating plateau test"),

          uiOutput(outputId = "de_analyseText")
        ),
        mainPanel(
          uiOutput(outputId = "de_hptPanel"))
      )
    }else if(protocol == "MAAD"){
      sidebarLayout(
        sidebarPanel(
          h4(input$de_protocol),

          uiOutput(outputId = "de_evalSlider"),

          uiOutput("de_fittingParametersText"),

          uiOutput(outputId = "de_fittingMethod"),
          uiOutput(outputId = "de_weightCheck"),
          uiOutput(outputId = "de_slopeCheck"),
          uiOutput(outputId = "de_aDoseSlider"),
          uiOutput(outputId = "de_rDoseSlider"),

          uiOutput("de_rejectionCriteriaText"),

          uiOutput(outputId = "de_paleodoseError"),
          uiOutput(outputId = "de_testdoseError"),

          actionButton(inputId = "de_analyseButton",
                       label = "Start De estimation"),

          uiOutput(outputId = "de_analyseText")
        ),
        mainPanel(
          uiOutput(outputId = "de_MAADpanel"))
      )

    }else if(protocol == "SAR"){
      sidebarLayout(
        sidebarPanel(
          h4(input$de_protocol),

          uiOutput(outputId = "de_evalSlider"),

          uiOutput("de_fittingParametersText"),

          uiOutput(outputId = "de_fittingMethod"),
          uiOutput(outputId = "de_weightCheck"),
          uiOutput(outputId = "de_rDoseSlider"),

          uiOutput("de_rejectionCriteriaText"),

          uiOutput(outputId = "de_recyclingRatio"),
          uiOutput(outputId = "de_recuparationRate"),
          uiOutput(outputId = "de_paleodoseError"),
          uiOutput(outputId = "de_testdoseError"),

          actionButton(inputId = "de_analyseButton",
                       label = "Start De estimation"),

          uiOutput(outputId = "de_analyseText")

        ),
        mainPanel(
          uiOutput(outputId = "de_SARpanel"))
      )

    }else{
      sidebarLayout(
        sidebarPanel(
          h5(paste(input$de_protocol,"is not yet supported"))
        ),
        mainPanel(
          h5("")
        )
      )
    }
  })

  output$de_evalSlider <- renderUI({

    temp.T <- as.numeric(input$de_interestSlider)

    if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
      Tmin <- 0
      Tmax <- 500
    }else{
      Tmin <- temp.T[1]
      Tmax <- temp.T[2]
    }

    sliderInput("de_evalSlider",
                label = "Integration interval [°C]",
                min = Tmin,
                max = Tmax,
                value = c(Tmin,Tmax))

  })

  output$de_fittingParametersText <- renderUI({
    h5(tags$b("Fitting parameters"))

  })
  output$de_fittingMethod <- renderUI({
    selectInput("de_fittingMethod",
                label = "Fitting method",
                choices = list("LIN", "EXP"),
                selected = "LIN")

  })

  output$de_weightCheck <- renderUI({
    checkboxInput("de_weightCheck",
                  label = "Weight fitting",
                  value = TRUE)
  })

  output$de_slopeCheck <- renderUI({
    checkboxInput("de_slopeCheck",
                  label = "Reuse Q slope for I",
                  value = TRUE)
  })

  output$de_aDoseSlider <- renderUI({

    info <- de_DATA.info()

    if(is.null(info)){
      Dmin <- 0
      Dmax <- 1000

    }else{
      Dmin <- as.numeric(info$Da.min)
      Dmax <- as.numeric(info$Da.max)
    }

    sliderInput("de_aDoseSlider",
                label = "Additive dose interval [s]",
                min = Dmin,
                max = Dmax,
                value = c(Dmin,Dmax))
  })

  output$de_rDoseSlider <- renderUI({

    info <- de_DATA.info()

    if(is.null(info)){
      Dmin <- 0
      Dmax <- 1000
    }else{
      Dmin <- as.numeric(info$Dr.min)
      Dmax <- as.numeric(info$Dr.max)
    }

    sliderInput("de_rDoseSlider",
                label = "Regenerative dose interval [s]",
                min = Dmin,
                max = Dmax,
                value = c(Dmin,Dmax))
  })

  output$de_rejectionCriteriaText <- renderUI({
      h5(tags$b("Rejection criteria"))
  })

  output$de_recyclingRatio <- renderUI({
    numericInput("de_recyclingRatio",
                 label = "Recycling ratio [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })

  output$de_recuparationRate <- renderUI({
    numericInput("de_recuparationRate",
                 label = "Recuparation rate [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })

  output$de_paleodoseError <- renderUI({
    numericInput(inputId = "de_paleodoseError",
                 label = "Maximum palaeodose error [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })

  output$de_testdoseError <- renderUI({
    numericInput(inputId = "de_testdoseError",
                 label = "Maximum testdose error [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })

  output$de_analyseText <- renderUI({
    
    pretreatment <- de_DATA.pretreatment()
    analyse <- de_DATA.analyse()

    if(is.null(pretreatment)){
      helpText("No data")
    
    }else if(is.null(analyse)){
      helpText("Waiting for analysis")
    
    }else{
      helpText("Analyse done")
    }
  })

  output$de_hptPanel <- renderUI({
    
    tabsetPanel(id = "de_analyseTabset",
                tabPanel("data",
                         uiOutput(outputId = "de_hptDataTab")),
                tabPanel("Results",
                         uiOutput(outputId = "de_hptResultTab")
                         )
    )
  })

  output$de_hptDataTab <- renderUI({

    pretreatment <- de_DATA.pretreatment()
    
    if(is.null(pretreatment)){
      return(NULL)
    }
    
    # Eval
    eval.Tmin <- as.numeric(input$de_evalSlider[1])
    eval.Tmax <- as.numeric(input$de_evalSlider[2])
    
    #Plot
    plot.Tmin <- input$de_interestSlider[1]
    plot.Tmax <- input$de_interestSlider[2]
    
    plotting.parameters=list(plot.Tmin=plot.Tmin,
                             plot.Tmax=plot.Tmax)
    
    dTypes <- character()
    temperatures <- numeric()
    TL <- numeric()
    
    records <- pretreatment@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      
      temp.temperatures <- temp.record@temperatures
      temp.TL <- temp.record@data
      
      dTypes <- c(dTypes,temp.dTypes)
      temperatures <- cbind(temperatures, temp.temperatures)
      TL <- cbind(TL, temp.TL)
    }
    
    plotData <- list(eval.Tmin = eval.Tmin,
                     eval.Tmax = eval.Tmax,
                     dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL,
                     plotting.parameters = plotting.parameters)

    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.analyse, plotData)})
                    ))
  })
  
  output$de_hptResultTab <- renderUI({
    
    data <- de_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    originator <- data[[1]]@originator
    
    if(originator != "analyse_TL.plateau"){
      return(NULL)
    }
    
    plotData <- data[[1]]@plotData
    
    Lx.data <- list(names = plotData$names,
                    doses = plotData$doses,
                    temperatures = plotData$temperatures,
                    Lx = plotData$Lx,
                    Lx.a = plotData$Lx.a,
                    Lx.plateau = plotData$Lx.plateau,
                    plotting.parameters = plotData$plotting.parameters)
    
    LxTx.data <- list(names = plotData$names,
                      doses = plotData$doses,
                      temperatures = plotData$temperatures,
                      Lx = plotData$LxTx,
                      Lx.a = plotData$LxTx.a,
                      Lx.plateau = plotData$LxTx.plateau,
                      plotting.parameters = plotData$plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HPT, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HPT, LxTx.data)})
    ))
  })
  
  output$de_MAADpanel <- renderUI({

    tabsetPanel(id = "de_analyseTabset",
                tabPanel("data",
                         uiOutput("de_maadDataTab")),
                tabPanel("Additive doses",
                         uiOutput(outputId = "de_maadAdditiveTab")),
                tabPanel("Regenerative doses",
                         uiOutput(outputId = "de_maadRegenerativeTab")),
                tabPanel("Results",
                         uiOutput(outputId = "de_maadResultTab")))

  })

  output$de_maadDataTab <- renderUI({
    
    pretreatment <- de_DATA.pretreatment()
    
    if(is.null(pretreatment)){
      return(NULL)
    }
    
    # Eval
    eval.Tmin <- as.numeric(input$de_evalSlider[1])
    eval.Tmax <- as.numeric(input$de_evalSlider[2])
    
    #Plot
    plot.Tmin <- input$de_interestSlider[1]
    plot.Tmax <- input$de_interestSlider[2]
    
    plotting.parameters=list(plot.Tmin=plot.Tmin,
                             plot.Tmax=plot.Tmax)
    
    temperatures <- numeric()
    TL <- numeric()
    dTypes <- character()
    
    records <- pretreatment@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      temp.temperatures <- temp.record@temperatures
      temp.TL <- temp.record@data
      
      dTypes <- c(dTypes, temp.dTypes)
      temperatures <- cbind(temperatures, temp.temperatures)
      TL <- cbind(TL, temp.TL)
    }
    
    plotData <- list(eval.Tmin = eval.Tmin,
                     eval.Tmax = eval.Tmax,
                     dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL,
                     plotting.parameters = plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.analyse, plotData)})
    ))
  })
  
  output$de_maadAdditiveTab <- renderUI({
    data <- de_DATA.analyse()

    if(is.null(data)){
      return(NULL)
    }

    plotData <- data[[1]]@plotData

    Lx.data <- list(plot.name="Lx",
                     temperatures = plotData$temperatures,
                     names = plotData$aNames,
                     doses = plotData$aDoses,
                     Lx = plotData$aLx,
                     Lx.plateau = plotData$aLx.plateau,
                     eval.Tmin = plotData$eval.Tmin,
                     eval.Tmax = plotData$eval.Tmax,
                     plotting.parameters = plotData$plotting.parameters)

    Tx.data <- list(plot.name="Tx",
                    temperatures = plotData$temperatures,
                    names = plotData$aNames,
                    doses = plotData$aDoses,
                    Lx = plotData$aTx,
                    Lx.plateau = plotData$aTx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)

    LxTx.data <- list(plot.name="LxTx",
                      temperatures = plotData$temperatures,
                      names = plotData$aNames,
                      doses = plotData$aDoses,
                      Lx = plotData$aLxTx,
                      Lx.plateau = plotData$aLxTx.plateau,
                      eval.Tmin = plotData$eval.Tmin,
                      eval.Tmax = plotData$eval.Tmax,
                      plotting.parameters = plotData$plotting.parameters)

    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })

  output$de_maadRegenerativeTab <- renderUI({
    data <- de_DATA.analyse()

    if(is.null(data)){
      return(NULL)
    }

    plotData <- data[[1]]@plotData

    Lx.data <- list(plot.name="Lx",
                     temperatures = plotData$temperatures,
                     names = plotData$rNames,
                     doses = plotData$rDoses,
                     Lx = plotData$rLx,
                     Lx.plateau = plotData$rLx.plateau,
                     eval.Tmin = plotData$eval.Tmin,
                     eval.Tmax = plotData$eval.Tmax,
                     plotting.parameters = plotData$plotting.parameters)

    Tx.data <- list(plot.name="Tx",
                     temperatures = plotData$temperatures,
                     names = plotData$rNames,
                     doses = plotData$rDoses,
                     Lx = plotData$rTx,
                     Lx.plateau = plotData$rTx.plateau,
                     eval.Tmin = plotData$eval.Tmin,
                     eval.Tmax = plotData$eval.Tmax,
                     plotting.parameters = plotData$plotting.parameters)

    LxTx.data <- list(plot.name="LxTx",
                       temperatures = plotData$temperatures,
                       names = plotData$rNames,
                       doses = plotData$rDoses,
                       Lx = plotData$rLxTx,
                       Lx.plateau = plotData$rLxTx.plateau,
                       eval.Tmin = plotData$eval.Tmin,
                       eval.Tmax = plotData$eval.Tmax,
                       plotting.parameters = plotData$plotting.parameters)

    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })


  output$de_maadResultTab <- renderUI({
    data <- de_DATA.analyse()

    if(is.null(data)){
      return(NULL)
    }

    plotData <- data[[1]]@plotData

    ## Plot ##

    plotData.DP <- list(temperatures = plotData$temperatures,
                        eval.Tmin = plotData$eval.Tmin,
                        eval.Tmax = plotData$eval.Tmax,
                        DP.Q.line = plotData$DP.Q.line,
                        DP.Q.line.error = plotData$DP.Q.line.error,
                        DP.I.line = plotData$DP.I.line,
                        DP.I.line.error = plotData$DP.I.line.error,
                        Q.DP = plotData$Q.DP,
                        Q.DP.error = plotData$Q.DP.error,
                        I.DP = plotData$I.DP,
                        I.DP.error = plotData$I.DP.error,
                        plotting.parameters= plotData$plotting.parameters)
    
    plotData.GC <- list(temperatures = plotData$temperatures,
                        eval.Tmin = plotData$eval.Tmin,
                        eval.Tmax = plotData$eval.Tmax,
                        GC.Q.line = plotData$GC.Q.line,
                        GC.Q.slope = plotData$GC.Q.slope,
                        GC.Q.LxTx = plotData$GC.Q.LxTx,
                        GC.Q.LxTx.error = plotData$GC.Q.LxTx.error,
                        GC.Q.doses = plotData$GC.Q.doses,
                        GC.Q.names = plotData$GC.Q.names,
                        GC.I.line = plotData$GC.I.line,
                        GC.I.slope = plotData$GC.I.slope,
                        GC.I.LxTx = plotData$GC.I.LxTx,
                        GC.I.LxTx.error = plotData$GC.I.LxTx.error,
                        GC.I.doses = plotData$GC.I.doses,
                        GC.I.names = plotData$GC.I.names,
                        Q.DP = plotData$Q.DP,
                        Q.DP.error = plotData$Q.DP.error,
                        Q.GC = plotData$Q.GC,
                        Q.GC.error = plotData$Q.GC.error,
                        I.DP = plotData$I.DP,
                        I.DP.error = plotData$I.DP.error,
                        I.GC = plotData$I.GC,
                        I.GC.error = plotData$I.GC.error,
                        rejection.values = plotData$rejection.values,
                        fitting.parameters = plotData$fitting.parameters,
                        plotting.parameters= plotData$plotting.parameters)    
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.MAAD_DP, plotData.DP)}),
                    renderPlot({do.call(shinyPlot_TL.MAAD_GC, plotData.GC)})
                    ))
  })

  output$de_SARpanel <- renderUI({

    info <- de_DATA.info()

    if(is.null(info)){
      return(NULL)
    }

    nAliquot <- info$nDiscs

    fluidRow(column(width = 12,
                    numericInput(inputId = "de_selectAliquot",
                                 label = "Aliquot selection",
                                 value = 1,
                                 min = 1,
                                 max = nAliquot,
                                 step = 1),
                    uiOutput(outputId = "de_sarDiscPanel")))
  })

  output$de_sarDiscPanel <- renderUI({

    tabsetPanel(id = "de_analyseTabset",
                tabPanel("Data",
                         uiOutput(outputId = "de_sarDataTab")),
                tabPanel("Regenerative doses",
                         uiOutput(outputId = "de_sarRegenerativeTab")),
                tabPanel("Results",
                         uiOutput(outputId = "de_sarResultsTab")))
  })

  output$de_sarDataTab <- renderUI({
    
    pretreatment <- de_DATA.pretreatment()
    info <- de_DATA.info()
    
    if(is.null(pretreatment)){
      return(NULL)
    }
    
    # Eval
    eval.Tmin <- as.numeric(input$de_evalSlider[1])
    eval.Tmax <- as.numeric(input$de_evalSlider[2])
    
    #Plot
    plot.Tmin <- input$de_interestSlider[1]
    plot.Tmax <- input$de_interestSlider[2]
    
    plotting.parameters=list(plot.Tmin=plot.Tmin,
                             plot.Tmax=plot.Tmax)
    
    
    index <- input$de_selectAliquot
    
    aliquots <- info$positions
    
    dTypes <- character()
    temperatures <- numeric()
    TL <- numeric()
    
    records <- pretreatment@records
    
    check <- FALSE
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.aliquot <- temp.record@metadata$POSITION
      
      if(temp.aliquot == aliquots[index]){
        
        check <- TRUE
        
        temp.dTypes <- temp.record@metadata$DTYPE
        
        temp.temperatures <- temp.record@temperatures
        temp.TL <- temp.record@data
        
        dTypes <- c(dTypes,temp.dTypes)
        temperatures <- cbind(temperatures, temp.temperatures)
        TL <- cbind(TL, temp.TL)
        
      }
    }
    
    if(check){
      plotData <- list(eval.Tmin = eval.Tmin,
                       eval.Tmax = eval.Tmax,
                       dTypes = dTypes,
                       temperatures = temperatures,
                       TL = TL,
                       plotting.parameters = plotting.parameters)
      
      fluidRow(column(width = 12,
                      renderPlot({do.call(shinyPlot_TL.analyse, plotData)})
      ))
    }else{
      fluidRow(column(width = 12,
                      helpText("This aliquot has been removed.")
      ))
    }
    
    
  })
  
  output$de_sarRegenerativeTab <- renderUI({
    
    data <- de_DATA.analyse()

    if(is.null(data)){
      return(NULL)
    }

    aliquot <- input$de_selectAliquot
    plotData <- data[[aliquot]]@plotData

    Lx.data <- list(plot.name="Lx",
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    doses = plotData$doses,
                    Lx = plotData$Lx,
                    Lx.plateau = plotData$Lx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)

    Tx.data <- list(plot.name="Tx",
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    doses = plotData$doses,
                    Lx = plotData$Tx,
                    Lx.plateau = plotData$Tx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)

    LxTx.data <- list(plot.name="LxTx",
                      temperatures = plotData$temperatures,
                      names = plotData$names,
                      doses = plotData$doses,
                      Lx = plotData$LxTx,
                      Lx.plateau = plotData$LxTx.plateau,
                      eval.Tmin = plotData$eval.Tmin,
                      eval.Tmax = plotData$eval.Tmax,
                      plotting.parameters = plotData$plotting.parameters)

    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })

  output$de_sarResultsTab <- renderUI({
    
    data <- de_DATA.analyse()

    if(is.null(data)){
      return(NULL)
    }

    aliquot <- input$de_selectAliquot
    plotData <- data[[aliquot]]@plotData

    DP.data <- list(eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    temperatures = plotData$temperatures,
                    DP.Q.line = plotData$DP.Q.line,
                    DP.Q.line.error = plotData$DP.Q.line.error,
                    Q.DP = plotData$Q.DP,
                    Q.DP.error = plotData$Q.DP.error,
                    TxTn = plotData$TxTn,
                    rejection.values = plotData$rejection.values,
                    plotting.parameters = plotData$plotting.parameters)
    
    GC.data <- list(fitting.parameters = plotData$fitting.parameters,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    names.duplicated = plotData$names.duplicated,
                    doses = plotData$doses,
                    GC.Q.line = plotData$GC.Q.line,
                    GC.Q.LxTx = plotData$GC.Q.LxTx,
                    GC.Q.LxTx.error = plotData$GC.Q.LxTx.error,
                    GC.Q.slope = plotData$GC.Q.slope,
                    Q.DP = plotData$Q.DP,
                    Q.DP.error = plotData$Q.DP.error,
                    Q.GC = plotData$Q.GC,
                    Q.GC.error = plotData$Q.GC.error,
                    rejection.values = plotData$rejection.values,
                    plotting.parameters = plotData$plotting.parameters)

    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.SAR_DP, DP.data)}),
                    renderPlot({do.call(shinyPlot_TL.SAR_GC, GC.data)})
                    ))


  })

  de_DATA.analyse <- reactive({

    temp <- input$de_analyseButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- de_generate.analyse()
    }

    return(data)
  })

  de_generate.analyse <- eventReactive(input$de_analyseButton,{

    data <- de_DATA.pretreatment()
    if(is.null(data)){
      return(NULL)
    }

    info <- de_DATA.info()

    protocol <- info$protocol

    # Parameters

    # Eval
    eval.Tmin <- as.numeric(input$de_evalSlider[1])
    eval.Tmax <- as.numeric(input$de_evalSlider[2])
    
    # Plotting
    plot.Tmin <- as.numeric(input$de_interestSlider[1])
    plot.Tmax <- as.numeric(input$de_interestSlider[2])

    plotting.parameters <- list(plot.Tmin=plot.Tmin,
                                plot.Tmax=plot.Tmax,
                                plateau.Tmin =eval.Tmin,
                                plateau.Tmax=eval.Tmax,
                                no.plot=TRUE)

    #Fitting parameters
    if(protocol == "MAAD"){

      # Fitting
      fit.method <- input$de_fittingMethod
      fit.weighted <- input$de_weightCheck
      fit.use.slope <- input$de_slopeCheck
      fit.rDoses.min <- as.numeric(input$de_rDoseSlider[1])
      fit.rDoses.max <- as.numeric(input$de_rDoseSlider[2])
      fit.aDoses.min <- as.numeric(input$de_aDoseSlider[1])
      fit.aDoses.max <- as.numeric(input$de_aDoseSlider[2])

      fitting.parameters <- list(fit.method=fit.method,
                                 fit.weighted=fit.weighted,
                                 fit.use.slope=fit.use.slope,
                                 fit.aDoses.min=fit.aDoses.min,
                                 fit.aDoses.max=fit.aDoses.max,
                                 fit.rDoses.min=fit.rDoses.min,
                                 fit.rDoses.max=fit.rDoses.max
      )

      # Rejection
      testdose.error <- input$de_testdoseError
      paleodose.error <- input$de_paleodoseError

      rejection.criteria <- list(testdose.error = testdose.error,
                                paleodose.error = paleodose.error)

    }else if(protocol == "SAR"){
      # Fitting
      fit.method <- input$de_fittingMethod
      fit.weighted <- input$de_weightCheck
      fit.rDoses.min <- as.numeric(input$de_rDoseSlider[1])
      fit.rDoses.max <- as.numeric(input$de_rDoseSlider[2])

      fitting.parameters <- list(fit.method=fit.method,
                                 fit.weighted=fit.weighted,
                                 fit.rDoses.min=fit.rDoses.min,
                                 fit.rDoses.max=fit.rDoses.max
      )

      # Rejection
      recycling.ratio <- input$de_recyclingRatio
      recuperation.rate <- input$de_recuparationRate
      testdose.error <- input$de_testdoseError
      paleodose.error <- input$de_paleodoseError

      rejection.criteria <- list(recycling.ratio = recycling.ratio,
                                 recuperation.rate = recuperation.rate,
                                 testdose.error = testdose.error,
                                 paleodose.error = paleodose.error)
    }


    results <- list()

    if(protocol == "HPT"){
     results[[1]] <- analyse_TL.plateau(object = data,
                                        plotting.parameters = plotting.parameters)
     
    }else if(protocol == "MAAD"){
      results[[1]] <- analyse_TL.MAAD(object = data,
                                     eval.Tmin = eval.Tmin,
                                     eval.Tmax = eval.Tmax,
                                     rejection.criteria = rejection.criteria,
                                     fitting.parameters = fitting.parameters,
                                     plotting.parameters = plotting.parameters)
      
    }else if(protocol == "SAR"){
       positions <- vector()

       for(i in 1: length(data@records)){
         temp.record <- data@records[[i]]
         temp.position <- temp.record@metadata$POSITION

         positions[i] <- temp.position
       }
       positions <- unique(positions)

       for(i in 1:length(positions)){

         temp.data <- mod_extract.aliquot(object = data,
                                          list = positions[i])

         temp.result <- analyse_TL.SAR(object= temp.data,
                                       eval.Tmin=eval.Tmin,
                                       eval.Tmax=eval.Tmax,
                                       fitting.parameters=fitting.parameters,
                                       plotting.parameters=plotting.parameters,
                                       rejection.criteria=rejection.criteria
         )

         results[[i]] <- temp.result
       }
      }else{
       results <- NULL
      }

    updateTabsetPanel(session,"de_analyseTabset","Results")
    
    return(results)
  })

  #############################################
  # Summary
  #############################################

  output$de_deTab <- renderUI({

    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "de_displayPanel"),
        uiOutput(outputId = "de_selectionPanel"),
        uiOutput(outputId = "de_s2GyPannel"),
        uiOutput(outputId = "de_deButton"),
        uiOutput(outputId = "de_deText")
      ),
      mainPanel(
        uiOutput(outputId = "de_dePanel"))
      )
  })

  output$de_displayPanel <- renderUI({
    protocol <- input$de_protocol
    
    if(is.null(protocol)){
      return(NULL)
    }
    
    if(protocol %in% c("MAAD")){
      fluidRow(column(width = 12,
                      h4("Display parameters"),
                      selectInput(inputId = "de_uncertaintyDisplay",
                                  label = "Uncertainty",
                                  choices = list("absolute", "relative"),
                                  selected = "absolute")
                      ))
    }else if(protocol %in% c("SAR")){
      fluidRow(column(width = 12,
                      h4("Display parameters"),
                      selectInput(inputId = "de_uncertaintyDisplay",
                                  label = "Uncertainty",
                                  choices = list("absolute", "relative"),
                                  selected = "absolute"),
                      selectInput(inputId = "de_plotSelection",
                                  label = "Plot",
                                  choices = list("abanico", "radial", "KDE", "histogram"),
                                  selected = "abanico")
      ))
    }
  })
  
  output$de_selectionPanel <- renderUI({
    protocol <- input$de_protocol

    if(protocol == "MAAD"){
      fluidRow(column(width = 12,
                      h4("D\u2091 selection"),
                      selectInput(inputId = "de_approachSelection",
                                  label = "Approach",
                                  choices = list("dose plateau", "growth curve"),
                                  selected = "growth curve"),
                      selectInput(inputId = "de_resultSelection",
                                  label = "Equivalent dose",
                                  choices = list("De", "Q", "I"),
                                  selected = "De")
                      ))
    }else if(protocol == "SAR"){
      fluidRow(column(width = 12,
                      h4("D\u2091 selection"),
                      selectInput(inputId = "de_approachSelection",
                                  label = "Approach",
                                  choices = list("dose plateau", "growth curve"),
                                  selected = "growth curve"),
                      selectInput(inputId = "de_methodSelection",
                                  label = "Method selection",
                                  choices = list("weighted", "unweighted", "MCM"),
                                  selected = "weighted"),
                      selectInput(inputId = "de_averageSelection",
                                  label = "Average Selection",
                                  choices = list("mean", "median"),
                                  selected = "mean"),
                      selectInput(inputId = "de_errorSelection",
                                  label = "Uncertainty Selection",
                                  choices = list("sd", "se"),
                                  selected = "sd")
                      ))
    }
  })

  output$de_deButton <- renderUI({

    protocol <- input$de_protocol

    if(protocol %in% c("SAR", "MAAD")){
      actionButton(inputId = "de_deButton",
                   label = "Convert D\u2091")
    }
  })

  output$de_deText <- renderUI({

    protocol <- input$de_protocol

    analyse <- de_DATA.analyse()
    de.Values <- de_DATA.De()

    temp <- input$de_deButton

    if(protocol %in% c("SAR", "MAAD")){

      if(is.null(analyse)){
        helpText("No data")
        
      }else if(is.null(de.Values)){
        helpText("Waiting for D\u2091 definition.")
        
      }else{
        helpText("D\u2091 defined")
      }
    }
  })

  output$de_averageSelection <- renderUI({

    protocol <- input$de_protocol

    if(protocol %in% c("SAR")){
      selectInput(inputId = "de_averageSelection",
                  label = "Average Selection",
                  choices = list("mean", "median"),
                  selected = "mean")
    }else{
      return(NULL)
    }
  })

  output$de_s2GyPannel <- renderUI({

    protocol <- input$de_protocol

    info <- de_DATA.info()
    
    if(!is.null(info)){
      
      s2Gy <- info$s2Gy
      s2Gy_err <- info$s2Gy_err
      
    }else{
      s2Gy <- ""
      s2Gy_err <- ""
    }

    if(!is.finite(s2Gy) || !is.finite(s2Gy_err)){
      s2Gy <- NULL
      s2Gy_err <- NULL
    }else{
      s2Gy <- round(s2Gy, 4)
      s2Gy_err <- round(s2Gy_err, 4)
    }

    if(protocol %in% c("MAAD","SAR")){


      fluidRow(
        h5(tags$b("Conversion factor [Gy/s]")),

        column(width = 6,
                      textInput(inputId = "s2Gy",
                                label = "D\u0309\u03b2",
                                value = s2Gy,
                                placeholder = "required")
                      ),
               column(width = 6,
                      textInput(inputId = "s2Gy_err",
                                label = "\u03B4D\u0309\u03b2",
                                value = s2Gy_err,
                                placeholder = "required")
                      )
      )
    }
  })

  output$de_dePanel <- renderUI({

    protocol <- input$de_protocol

    if(protocol == "MAAD"){
      uiOutput(outputId = "de_summaryMAADpanel")
    }else if(protocol == "SAR"){
      uiOutput(outputId = "de_summarySARpanel")
    }else{
      return (NULL)
    }
  })

  output$de_summaryMAADpanel <- renderUI({

    fluidRow(column(width = 12,
                    DT::dataTableOutput(outputId = "de_MAADTable"),
                    DT::dataTableOutput(outputId = "de_deTable")
                    ))
  })

  output$de_summarySARpanel <- renderUI({

    fluidRow(column(width = 12,
                    uiOutput(outputId = "de_sarPlot"),
                    DT::dataTableOutput(outputId = "de_deTable")
                    ))
  })

  output$de_MAADTable <- DT::renderDataTable({
    de_TABLE.maad()
  })
  
  de_TABLE.maad <- reactive({
    
    uncertainty <- input$de_uncertaintyDisplay
    
    if(is.null(uncertainty)){
      return(NULL)
    }
    
    analyse <- de_DATA.analyse()
    
    if(is.null(analyse) || is.null(info)){
      DP <- data.frame(Method = "GC",
                       Q = NA,
                       I = NA,
                       De = NA)
      
      GC <- data.frame(Method = "DP",
                       Q = NA,
                       I = NA,
                       De = NA)
    }else{
      temp.data <- analyse[[1]]
      
      DP.Q <- as.numeric(temp.data@data$DP$Q)
      DP.Q.error <- as.numeric(temp.data@data$DP$Q.error)
      DP.I <- as.numeric(temp.data@data$DP$I)
      DP.I.error <- as.numeric(temp.data@data$DP$I.error)
      DP.De <- as.numeric(temp.data@data$DP$De)
      DP.De.error <- as.numeric(temp.data@data$DP$De.error)
      
      GC.Q <- as.numeric(temp.data@data$GC$Q)
      GC.Q.error <- as.numeric(temp.data@data$GC$Q.error)
      GC.I <- as.numeric(temp.data@data$GC$I)
      GC.I.error <- as.numeric(temp.data@data$GC$I.error)
      GC.De <- as.numeric(temp.data@data$GC$De)
      GC.De.error <- as.numeric(temp.data@data$GC$De.error)
      
      if(uncertainty == "absolute"){
        DP <- data.frame(Method = "DP",
                         Q = paste(round(DP.Q,2), "\u00B1", round(DP.Q.error,2)),
                         I = paste(round(DP.I,2), "\u00B1", round(DP.I.error,2)),
                         De = paste(round(DP.De,2), "\u00B1", round(DP.De.error,2)))
        
        GC <- data.frame(
          Method = "GC",
          Q = paste(round(GC.Q,2), "\u00B1", round(GC.Q.error,2)),
          I = paste(round(GC.I,2), "\u00B1", round(GC.I.error,2)),
          De = paste(round(GC.De,2), "\u00B1", round(GC.De.error,2)))
        
      }else if (uncertainty == "relative"){
        DP <- data.frame(Method = "DP",
                         Q = paste(round(DP.Q,2), "\u00B1", round(DP.Q.error/DP.Q,2), "[%]"),
                         I = paste(round(DP.I,2), "\u00B1", round(DP.I.error/DP.I,2), "[%]"),
                         De = paste(round(DP.De,2), "\u00B1", round(DP.De.error/DP.De,2), "[%]"))
        
        GC <- data.frame(Method = "GC",
                         Q = paste(round(GC.Q,2), "\u00B1", round(GC.Q.error/GC.Q,2), "[%]"),
                         I = paste(round(GC.I,2), "\u00B1", round(GC.I.error/GC.I,2), "[%]"),
                         De = paste(round(GC.De,2), "\u00B1", round(GC.De.error/GC.De,2), "[%]"))
      }     
    }

    table <- rbind(DP, GC)
      
    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th('Approach'),
          tags$th('Q'),
          tags$th('I'),
          tags$th('D\u2091')
        )
      )
    )
    
    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))    
    return(datatable)
  })
  
  output$de_sarPlot <- renderUI({

    data <- de_DATA.analyse()

    method <- input$de_methodSelection
    average <- input$de_averageSelection
    error <- input$de_errorSelection
    
    uncertainty <- input$de_uncertaintyDisplay
    plot <- input$de_plotSelection
    
    if(uncertainty == "absolute"){
      error <- paste(error,".abs",sep = "")
    }else{
      error <- paste(error,".rel",sep = "")
    }
    
    #For radial & Histogram plot
    error.old <- character()

    if(error == "sd"){
      error.old <- "sd" 
    }else{
      error.old <- "se"
    }
    
    if(uncertainty == "relative"){
      error.old <- paste(error.old,"rel",sep = "")
    }else{
      error.old <- paste(error.old,"abs",sep = "")
    }
    
    if(method %in% c("weighted","MCM")){
      error.old <- paste(error.old,".weighted",sep = "")
    }
    

    if(is.null(data) || is.null(average)){
      return(NULL)
    }

    DP.De <- vector()
    DP.De_err <- vector()

    GC.De <- vector()
    GC.De_err <- vector()

    for(i in 1 : length(data)){
      temp.data <- data[[i]]

      DP.De[i] <- temp.data@data$DP$De
      DP.De_err[i] <- temp.data@data$DP$De.error

      GC.De[i] <- temp.data@data$GC$De
      GC.De_err[i] <- temp.data@data$GC$De.error
    }


    DP.values <- data.frame(De = DP.De,
                            De_err= DP.De_err)
    GC.values <- data.frame(De = GC.De,
                            De_err= GC.De_err)

    #Plot
    
    if(plot == "abanico"){
      fluidRow(
        h4("Abanico Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_AbanicoPlot(DP.values,
                                  stats=c("min","max"),
                                  summary=c("n",average, error),
                                  summary.method = method,
                                  log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_AbanicoPlot(GC.values,
                                  stats=c("min","max"),
                                  summary=c("n",average, error),
                                  summary.method = method,
                                  log.z = FALSE)
               })
        ))
    }else if(plot == "radial"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_RadialPlot(DP.values,
                                 stats=c("min","max"),
                                 summary=c("n",average, error.old),
                                 log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_RadialPlot(GC.values,
                                 stats=c("min","max"),
                                 summary=c("n",average, error.old),
                                 log.z = FALSE)
               })
        ))
    }else if(plot == "KDE"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_KDE(DP.values,
                          stats=c("min","max"),
                          summary=c("n",average, error),
                          summary.method = method,
                          log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_KDE(GC.values,
                          stats=c("min","max"),
                          summary=c("n",average, error),
                          summary.method = method,
                          log.z = FALSE)
               })
        ))
    }else if(plot == "histogram"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_Histogram(DP.values,
                                stats=c("min","max"),
                                summary=c("n",average, error.old),
                                log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_Histogram(GC.values,
                                stats=c("min","max"),
                                summary=c("n",average, error.old),
                                log.z = FALSE)
               })
        ))
    }
  })

  output$de_deTable <- renderDataTable({
    de_TABLE.De()
  })
  
  de_TABLE.De <- reactive({
  
    sample <- input$sa_sample
    
    info <- de_DATA.info()
    de.Values <- de_DATA.De()
    
    if(is.null(info)){
      nDiscs <- 0
    }else{
      nDiscs <- as.character(info$nDiscs)
    }
    
    if(is.null(de.Values)){
      De.text <- ""
    }else{
      De <- de.Values$De
      De_err <- de.Values$De_err
      
      De.text <- paste(round(De, 3), "\u00B1", round(De_err, 3))
    }
    
    table <- data.frame(Sample = sample,
                        nDiscs = nDiscs,
                        De = De.text)
    
    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th('Sample'),
          tags$th('Aliquots'),
          tags$th('De [Gy]')
        )
      )
    )
    
    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))
    
    return(datatable)
  })

  de_DATA.De <- reactive({
    temp <- input$de_deButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- de_generate.De()
    }

    return(data)
  })

  de_generate.De <- eventReactive(input$de_deButton,{

    info <- de_DATA.info()
    data <- de_DATA.analyse()

    if(is.null(data) || is.null(info)){
      return(NULL)
    }

    approach <- input$de_approachSelection
    result <- input$de_resultSelection
    method <- input$de_methodSelection
    average <- input$de_averageSelection
    error <- input$de_errorSelection

    s2Gy <- as.numeric(input$s2Gy)
    s2Gy_err <- as.numeric(input$s2Gy_err)

    if(!is.finite(s2Gy) || s2Gy <= 0 ){
      s2Gy <- NA
      s2Gy_err <- NA
    }

    protocol <- info$protocol

    temp.De <- NULL
    temp.De_err <- NULL

    if(protocol == "MAAD"){


      temp.data <- data[[1]]

      DP.Q <- as.numeric(temp.data@data$DP$Q)
      DP.Q_err <- as.numeric(temp.data@data$DP$Q.error)
      DP.I <- as.numeric(temp.data@data$DP$I)
      DP.I_err <- as.numeric(temp.data@data$DP$I.error)
      DP.De <- as.numeric(temp.data@data$DP$De)
      DP.De_err <- as.numeric(temp.data@data$DP$De.error)

      GC.Q <- as.numeric(temp.data@data$GC$Q)
      GC.Q_err <- as.numeric(temp.data@data$GC$Q.error)
      GC.I <- as.numeric(temp.data@data$GC$I)
      GC.I_err <- as.numeric(temp.data@data$GC$I.error)
      GC.De <- as.numeric(temp.data@data$GC$De)
      GC.De_err <- as.numeric(temp.data@data$GC$De.error)

      if(approach == "dose plateau"){
        Q <- DP.Q
        Q_err <- DP.Q_err
        I <- DP.I
        I_err <-DP.I_err
        De <- DP.De
        De_err <- DP.De_err
      }else if(approach == "growth curve"){
        Q <- GC.Q
        Q_err <- GC.Q_err
        I <- GC.I
        I_err <-GC.I_err
        De <- GC.De
        De_err <- GC.De_err
      }

      if(result == "Q"){
        temp.De <- Q
        temp.De_err <- Q_err
      }else if(result == "I"){
        temp.De <- I
        temp.De_err <- I_err
      }else if(result == "De"){
        temp.De <- De
        temp.De_err <- De_err
      }

    }else if(protocol == "SAR"){

      DP.De <- vector()
      DP.De_err <- vector()

      GC.De <- vector()
      GC.De_err <- vector()

      for(i in 1 : length(data)){
        temp.data <- data[[i]]

        DP.De[i] <- temp.data@data$DP$De
        DP.De_err[i] <- temp.data@data$DP$De.error

        GC.De[i] <- temp.data@data$GC$De
        GC.De_err[i] <- temp.data@data$GC$De.error
      }

      if(approach == "dose plateau"){
        De <- DP.De
        De_err <- DP.De_err
      }else if(approach == "growth curve"){
        De <- GC.De
        De_err <- GC.De_err
      }

      De.val <- data.frame(De=De,
                           De.error = De_err)
      
      stats <- calc_Statistics(data = De.val)

      if(method== "weighted"){
        De.data <- stats$weighted
      }else if(method == "unweighted"){
        De.data <- stats$unweighted
      }else if(method == "MCM"){
        De.data <- stats$MCM
      }

      if(average == "mean"){
        temp.De <- De.data$mean
      }else if(average == "median"){
        temp.De <- De.data$median
      }

      if(error == "sd"){
        temp.De_err <- De.data$sd.abs
      }else if(error == "se"){
        temp.De_err <- De.data$se.abs
      }

    }

    new.De <- temp.De*s2Gy

    De_rel <- sqrt(sum((temp.De_err/temp.De)^2,(s2Gy_err/s2Gy)^2,na.rm = TRUE))

    new.De_err <- new.De*De_rel

    result <- list(De=new.De,
                   De_err = new.De_err)

    return(result)
  })

  ##############################################################################################
  # a-value Estimation
  ##############################################################################################
  
  output$aPage <- renderUI({
    tabsetPanel(id = "aPage",
                tabPanel("File",
                         uiOutput(outputId = "av_fileTab")),
                tabPanel("Pretreatment",
                         uiOutput(outputId = "av_pretreatmentTab")),
                tabPanel("Analyse",
                         uiOutput(outputId = "av_analyseTab")),
                tabPanel("a-value",
                         uiOutput(outputId = "av_aTab"))
    )
  })
  
  #############################################
  # File
  #############################################
  
  output$av_fileTab <- renderUI({
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "av_fileSidebar")
      ),
      mainPanel(
        uiOutput(outputId = "av_fileMain")
      )
    )
  })
  
  output$av_fileSidebar <- renderUI({
    fluidRow(column(width = 12,
                    selectInput(inputId = "av_protocol",
                                label = "Protocol",
                                choices = list("MAAD", "SAR"),
                                selected = "MAAD"),
                    
                    selectInput(inputId = "av_extension",
                                label = "File extension",
                                choices = list(".binx", ".bin"),
                                selected = ".binx"),
                    
                    fileInput(inputId = "av_file",
                              label = "Import file",
                              accept = c(".bin", ".binx")),
                    
                    numericInput(inputId = "av_k",
                                 label = "Parameter for the uncertainties (k)",
                                 value = 1,
                                 step = 0.1,
                                 min = 0),
                    
                    actionButton(inputId = "av_loadButton",
                                 label = "Generate data"),
                    
                    uiOutput(outputId = "av_fileText")
    ))
  })
  
  
  output$av_fileMain <- renderUI({
    
    bin <- av_DATA.bin()
    
    if(!is(bin,"TLum.Analysis")){
      return(NULL)
    }
    
    dTypes <- character()
    temperatures <- list()
    TL <- list()
    
    records <- bin@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      
      temp.temperatures <- list(temp.record@temperatures)
      temp.TL <- list(temp.record@data)
      
      dTypes <- c(dTypes,temp.dTypes)
      temperatures <- c(temperatures, temp.temperatures)
      TL <- c(TL, temp.TL)
    }
    
    plotData <- list(dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BIN,plotData)})
    ))
    
  })
  
  output$av_fileText <- renderUI({
    
    risoe <- av_DATA.Risoe()
    bin <- av_DATA.bin()
    
    if(is.null(risoe)){
      helpText("No data")
      
    }else if(is.null(bin)){
      helpText("Waiting for data generation")
      
    }else{
      helpText("Data generated")
    }
  })
  
  av_DATA.bin <- reactive({
    
    temp <- input$av_loadButton
    
    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- av_generate.data()
    }
    return(data)
  })
  
  av_generate.data <- eventReactive(input$av_loadButton,{
    
    old.bin <- av_DATA.Risoe()
    
    if(is.null(old.bin)){
      return(NULL)
    }
    
    k <- as.numeric(input$av_k)
    protocol <- as.character(input$av_protocol)
    
    plotting.parameters <- list(no.plot=TRUE)
    
    bin <- Risoe.BINfileData2TLum.BIN.File(object = old.bin,
                                           k = k)
    
    data <- TLum.BIN.File2TLum.Analysis(object = bin,
                                        protocol = protocol)
    
    data <- mod_extract.TL(data,
                           plotting.parameters = plotting.parameters)
    
    data <- mod_update.dType(data)
    
    return(data)
  })
  
  
  av_DATA.Risoe <- reactive({
    
    path <- input$av_file$datapath
    ext <- as.character(input$av_extension)
    
    if(is.null(path)){
      return(NULL)
    }
    
    new.path <- paste(path, ext, sep="")
    
    file.rename(from = path,
                to = new.path)
    
    new.bin <- read_BIN2R(new.path)
    
    return(new.bin)
    
  })
  
  av_DATA.info <- reactive({
    
    protocol <- as.character(input$av_protocol)
    
    if(is.null(protocol)){
      return(NULL)
    }
    
    data <- av_DATA.bin()
    
    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    nRecords <- length(data@records)
    
    Tmin <- 0
    Tmax <- 0
    
    Da.min <- numeric()
    Da.max <- numeric()
    Dr.min <- numeric()
    Dr.max <- numeric()
    
    positions <- numeric()
    s2Gy <- numeric()
    s2Gy_err <- numeric()
    
    for(i in 1:nRecords){
      temp.record <- data@records[[i]]
      temp.metadata <- temp.record@metadata
      
      temp.Tmax <- max(temp.record@temperatures)
      temp.position <- as.numeric(temp.metadata$POSITION)
      
      temp.dtype <- as.character(temp.metadata$DTYPE)
      temp.dose <- as.numeric(temp.metadata$IRR_TIME)
      
      temp.s2Gy <- as.numeric(temp.metadata$IRR_DOSERATE)
      temp.s2Gy_err <- as.numeric(temp.metadata$IRR_DOSERATEERR)
      
      Tmax <- max(Tmax, temp.Tmax)
      positions <- c(positions,temp.position)
      
      if(protocol == "MAAD"){
        if(temp.dtype == "Natural"){
          Da.min <- 0
          Da.max <- max(Da.max, 0)
        }else if(temp.dtype == "N+dose"){
          Da.min <- min(Da.min, temp.dose)
          Da.max <- max(Da.max, temp.dose)
          
        }else if(temp.dtype == "Bleach"){
          Dr.min <- 0
          Dr.max <- max(Dr.max, 0)
        }else if(temp.dtype == "Bleach+dose"){
          Dr.min <- min(Dr.min, temp.dose)
          Dr.max <- max(Dr.max, temp.dose)
        }
        
      }else if(protocol == "SAR"){
        if(temp.dtype == "Dose"){
          Dr.min <- min(Dr.min, temp.dose)
          Dr.max <- max(Dr.max, temp.dose)
        }
      }
      
      s2Gy <- c(s2Gy, temp.s2Gy)
      s2Gy_err <- c(s2Gy_err, temp.s2Gy_err)
    }
    
    positions <- unique(positions)
    nDiscs <- length(positions)
    
    s2Gy <- mean(s2Gy)
    s2Gy_err <- mean(s2Gy_err)
    
    info <- list(protocol = protocol,
                 nRecords = nRecords,
                 nDiscs = nDiscs,
                 positions = positions,
                 Tmin = Tmin,
                 Tmax = Tmax,
                 Da.min = Da.min,
                 Da.max = Da.max,
                 Dr.min = Dr.min,
                 Dr.max = Dr.max,
                 s2Gy = s2Gy,
                 s2Gy_err = s2Gy_err)
    
    return(info)
  })
  
  
  #############################################
  # Pretreatment
  #############################################
  
  output$av_pretreatmentTab <- renderUI({
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "av_interestSlider"),
        
        checkboxGroupInput(inputId = "av_pretreatmentSelection",
                           label = "Preatreatments",
                           choices = c("remove disc", "remove preheat", "substract background", "align peak"),
                           selected = c("remove preheat", "substract background", "align peak")
        ),
        
        uiOutput(outputId = "av_removeOption"),
        
        uiOutput(outputId = "av_alignOption"),
        
        actionButton(inputId = "av_pretreatmentButton",
                     label = "Apply pretreatment"),
        
        uiOutput(outputId = "av_pretreatmentText")
      ),
      
      mainPanel(
        tabsetPanel(id = "av_pretreatmentTabset",
                    tabPanel("Data",
                             uiOutput(outputId = "av_pretreatmentPlot")),
                    
                    tabPanel("Remove preheats",
                             uiOutput(outputId = "av_preheatPlot")),
                    tabPanel("Substract background",
                             uiOutput(outputId = "av_backgroundPlot")),
                    tabPanel("Align peaks",
                             uiOutput(outputId = "av_alignPlot"))
        )
      )
    )
  })
  
  output$av_interestSlider <- renderUI({
    
    info <- av_DATA.info()
    
    if(is.null(info)){
      sliderInput("av_interestSlider",
                  label = "Region of interest [°C]",
                  min = 0,
                  max = 500,
                  value = c(0,500))
    }else{
      temp.T <- c(info$Tmin, info$Tmax)
      
      if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
        Tmin <- 0
        Tmax <- 500
      }else{
        Tmin <- temp.T[1]
        Tmax <- temp.T[2]
      }
      
      sliderInput("av_interestSlider",
                  label = "Region of interest [°C]",
                  min = Tmin,
                  max = Tmax,
                  value = c(Tmin,Tmax))
    }
    
    
  })
  
  output$av_removeOption <- renderUI({
    
    pretreatments <- input$av_pretreatmentSelection
    
    if("remove disc" %in% pretreatments){
      fluidRow(column(width = 11,
                      offset = 1,
                      textInput("av_removeList",
                                label = "discs to remove",
                                placeholder = "1 2 3 4 5 6 7 8 9 0 , ; -")
      ))
    }
  })
  
  output$av_alignOption <- renderUI({
    
    pretreatments <- input$av_pretreatmentSelection
    
    temp.T <- as.numeric(input$av_interestSlider)
    
    if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
      Tmin <- 0
      Tmax <- 500
    }else{
      Tmin <- temp.T[1]
      Tmax <- temp.T[2]
    }
    
    if("align peak" %in% pretreatments){
      fluidRow(column(width = 11,
                      offset = 1,
                      sliderInput("av_alignSlider",
                                  label = "Peak position [°C]",
                                  min = Tmin,
                                  max = Tmax,
                                  value = c(Tmin,Tmax)),
                      checkboxInput("av_alignTest",
                                    label = "Do NOT rely on the test doses for the alignment.",
                                    value = FALSE))
      )
    }
    
  })
  
  av_remove.list <- reactive({
    
    list.IN <- input$av_removeList
    
    temp.list <- strsplit(list.IN,split = c(",", ";", " "))[[1]]
    
    new.list <- numeric()
    
    for(i in 1:length(temp.list)){
      temp.val <- temp.list[i]
      
      if(grepl("-", temp.val)){
        temp.val <- strsplit(temp.val,split = "-")[[1]]
        temp.min <- min(as.numeric(temp.val))
        temp.max <- max(as.numeric(temp.val))
        
        temp.out <- seq(temp.min, temp.max)
      }else{
        temp.out <- as.numeric(temp.val)
      }
      
      new.list <- c(new.list, temp.out)
    }
    
    return(new.list)
  })
  
  av_DATA.pretreatment <- reactive({
    
    temp <- input$av_pretreatmentButton
    
    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- av_generate.pretreatment()
    }
    return(data)
  })
  
  av_generate.pretreatment <- eventReactive(input$av_pretreatmentButton,{
    
    data <- av_DATA.bin()
    
    if(is.null(data)){
      return(NULL)
    }
    
    pretreatments <- input$av_pretreatmentSelection
    
    # Parameters
    plot.Tmin <- input$av_interestSlider[1]
    plot.Tmax <- input$av_interestSlider[2]
    
    plotting.parameters <- list(plot.Tmin=plot.Tmin,
                                plot.Tmax=plot.Tmax,
                                no.plot=TRUE)
    
    if("remove disc" %in% pretreatments){
      remove.list <- av_remove.list()
      
      data <- mod_remove.aliquot(data,
                                 list = remove.list)
    }
    
    if("remove preheat" %in% pretreatments){
      data <- mod_remove.preheat(data,
                                 plotting.parameters = plotting.parameters)
      
      updateTabsetPanel(session,"av_pretreatmentTabset","Remove preheats")
    }
    
    if("substract background" %in% pretreatments){
      data <- mod_substract.background(data,
                                       plotting.parameters = plotting.parameters)
      
      updateTabsetPanel(session,"av_pretreatmentTabset","Substract background")
    }
    
    if("align peak" %in% pretreatments){
      peak.Tmin <- input$av_alignSlider[1]
      peak.Tmax <- input$av_alignSlider[2]
      
      no.testdose <- input$av_alignTest
      
      aligning.parameters <- list(peak.Tmin=peak.Tmin,
                                  peak.Tmax=peak.Tmax,
                                  no.testdose=no.testdose)
      
      data <- mod_align.peaks(data,
                              aligning.parameters = aligning.parameters,
                              plotting.parameters = plotting.parameters)
      
      updateTabsetPanel(session,"av_pretreatmentTabset","Align peaks")
    }
    
    return(data)
  })
  
  output$av_pretreatmentPlot <- renderUI({
    
    bin <- av_DATA.bin()
    
    if(!is(bin,"TLum.Analysis")){
      return(NULL)
    }
    
    dTypes <- character()
    temperatures <- list()
    TL <- list()
    
    records <- bin@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      
      temp.temperatures <- list(temp.record@temperatures)
      temp.TL <- list(temp.record@data)
      
      dTypes <- c(dTypes,temp.dTypes)
      temperatures <- c(temperatures, temp.temperatures)
      TL <- c(TL, temp.TL)
    }
    
    plotData <- list(dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BIN,plotData)})
    ))
  })
  
  output$av_preheatPlot <- renderUI({
    data <- av_DATA.pretreatment()
    
    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    history <- data@history
    
    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]
      
      if(temp.mod == "mod_remove.preheat"){
        plotData <- data@plotHistory[[i]]
      }
    }
    
    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.PH <- list(PH.signal = plotData$PH.signal,
                        PH.temperatures = plotData$PH.temperatures,
                        PH.times = plotData$PH.times)
    
    plotData.TL <- list(TL.signal = plotData$TL.signal,
                        TL.temperatures = plotData$TL.temperatures)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.PH_PH,plotData.PH)}),
                    renderPlot({do.call(shinyPlot_TL.PH_TL,plotData.TL)})
    ))
  })
  
  output$av_backgroundPlot <- renderUI({
    data <- av_DATA.pretreatment()
    
    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    history <- data@history
    
    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]
      
      if(temp.mod == "mod_substract.background"){
        plotData <- data@plotHistory[[i]]
      }
    }
    
    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.BG <- list(TL = plotData$old.TL,
                        BG = plotData$BG,
                        temperatures = plotData$temperatures)
    
    plotData.TL <- list(TL = plotData$new.TL,
                        temperatures = plotData$temperatures)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.BG_BG,plotData.BG)}),
                    renderPlot({do.call(shinyPlot_TL.BG_TL,plotData.TL)})
    ))
  })
  
  output$av_alignPlot <- renderUI({
    
    data <- av_DATA.pretreatment()
    
    if(!is(data,"TLum.Analysis")){
      return(NULL)
    }
    
    history <- data@history
    
    plotData <- NULL
    
    for(i in 1:length(history)){
      temp.mod <- history[i]
      
      if(temp.mod == "mod_align.peaks"){
        plotData <- data@plotHistory[[i]]
      }
    }
    
    if(is.null(plotData)){
      return(NULL)
    }
    
    plotData.peak <- list(temperatures = plotData$temperatures,
                          TL = plotData$old.TL,
                          Tx = plotData$ref.TL,
                          pos.peak = plotData$pos.peak,
                          plotting.parameters = plotData$plotting.parameters)
    
    plotData.TL <- list(temperatures = plotData$temperatures,
                        TL = plotData$new.TL,
                        pos.peak = plotData$pos.peak,
                        plotting.parameters = plotData$plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.AP_peak,plotData.peak)}),
                    renderPlot({do.call(shinyPlot_TL.AP_TL,plotData.TL)})
    ))
  })
  
  output$av_pretreatmentText <- renderUI({
    
    bin <- av_DATA.bin()
    pretreatment <- av_DATA.pretreatment()
    
    if(is.null(bin)){
      helpText("No data")
      
    }else if(is.null(pretreatment)){
      helpText("Waiting for pretreatment")
      
    }else{
      helpText("Pretreatment done")
    }
  })
  
  #############################################
  # De
  #############################################
  
  output$av_analyseTab <- renderUI({
    
    protocol <- as.character(input$av_protocol)
    
    if(protocol == "MAAD"){
      sidebarLayout(
        sidebarPanel(
          h4(input$av_protocol),
          
          uiOutput(outputId = "av_evalSlider"),
          
          uiOutput("av_fittingParametersText"),
          
          uiOutput(outputId = "av_fittingMethod"),
          uiOutput(outputId = "av_weightCheck"),
          uiOutput(outputId = "av_slopeCheck"),
          uiOutput(outputId = "av_aDoseSlider"),
          uiOutput(outputId = "av_rDoseSlider"),
          
          uiOutput("av_rejectionCriteriaText"),
          
          uiOutput(outputId = "av_paleodoseError"),
          uiOutput(outputId = "av_testdoseError"),
          
          actionButton(inputId = "av_analyseButton",
                       label = "Start De estimation"),
          
          uiOutput(outputId = "av_analyseText")
        ),
        mainPanel(
          uiOutput(outputId = "av_MAADpanel"))
      )
      
    }else if(protocol == "SAR"){
      sidebarLayout(
        sidebarPanel(
          h4(input$av_protocol),
          
          uiOutput(outputId = "av_evalSlider"),
          
          uiOutput("av_fittingParametersText"),
          
          uiOutput(outputId = "av_fittingMethod"),
          uiOutput(outputId = "av_weightCheck"),
          uiOutput(outputId = "av_rDoseSlider"),
          
          uiOutput("av_rejectionCriteriaText"),
          
          uiOutput(outputId = "av_recyclingRatio"),
          uiOutput(outputId = "av_recuparationRate"),
          uiOutput(outputId = "av_paleodoseError"),
          uiOutput(outputId = "av_testdoseError"),
          
          actionButton(inputId = "av_analyseButton",
                       label = "Start De estimation"),
          
          uiOutput(outputId = "av_analyseText")
          
        ),
        mainPanel(
          uiOutput(outputId = "av_SARpanel"))
      )
      
    }else{
      sidebarLayout(
        sidebarPanel(
          h5(paste(input$av_protocol,"is not yet supported"))
        ),
        mainPanel(
          h5("")
        )
      )
    }
  })
  
  output$av_evalSlider <- renderUI({
    
    temp.T <- as.numeric(input$av_interestSlider)
    
    if(is.null(temp.T) || length(temp.T) < 2 || !is.finite(temp.T)){
      Tmin <- 0
      Tmax <- 500
    }else{
      Tmin <- temp.T[1]
      Tmax <- temp.T[2]
    }
    
    sliderInput("av_evalSlider",
                label = "Integration interval [°C]",
                min = Tmin,
                max = Tmax,
                value = c(Tmin,Tmax))
    
  })
  
  output$av_fittingParametersText <- renderUI({
    h5(tags$b("Fitting parameters"))
    
  })
  output$av_fittingMethod <- renderUI({
    selectInput("av_fittingMethod",
                label = "Fitting method",
                choices = list("LIN", "EXP"),
                selected = "LIN")
    
  })
  
  output$av_weightCheck <- renderUI({
    checkboxInput("av_weightCheck",
                  label = "Weight fitting",
                  value = TRUE)
  })
  
  output$av_slopeCheck <- renderUI({
    checkboxInput("av_slopeCheck",
                  label = "Reuse Q slope for I",
                  value = TRUE)
  })
  
  output$av_aDoseSlider <- renderUI({
    
    info <- av_DATA.info()
    
    if(is.null(info)){
      Dmin <- 0
      Dmax <- 1000
      
    }else{
      Dmin <- as.numeric(info$Da.min)
      Dmax <- as.numeric(info$Da.max)
    }
    
    sliderInput("av_aDoseSlider",
                label = "Additive dose interval [s]",
                min = Dmin,
                max = Dmax,
                value = c(Dmin,Dmax))
  })
  
  output$av_rDoseSlider <- renderUI({
    
    info <- av_DATA.info()
    
    if(is.null(info)){
      Dmin <- 0
      Dmax <- 1000
    }else{
      Dmin <- as.numeric(info$Dr.min)
      Dmax <- as.numeric(info$Dr.max)
    }
    
    sliderInput("av_rDoseSlider",
                label = "Regenerative dose interval [s]",
                min = Dmin,
                max = Dmax,
                value = c(Dmin,Dmax))
  })
  
  output$av_rejectionCriteriaText <- renderUI({
    h5(tags$b("Rejection criteria"))
  })
  
  output$av_recyclingRatio <- renderUI({
    numericInput("av_recyclingRatio",
                 label = "Recycling ratio [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })
  
  output$av_recuparationRate <- renderUI({
    numericInput("av_recuparationRate",
                 label = "Recuparation rate [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })
  
  output$av_paleodoseError <- renderUI({
    numericInput(inputId = "av_paleodoseError",
                 label = "Maximum palaeodose error [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })
  
  output$av_testdoseError <- renderUI({
    numericInput(inputId = "av_testdoseError",
                 label = "Maximum testdose error [%]",
                 value = 10,
                 step = 1,
                 min = 0)
  })
  
  output$av_analyseText <- renderUI({
    
    pretreatment <- av_DATA.pretreatment()
    analyse <- av_DATA.analyse()
    
    if(is.null(pretreatment)){
      helpText("No data")
      
    }else if(is.null(analyse)){
      helpText("Waiting for analysis")
      
    }else{
      helpText("Analyse done")
    }
  })
 
  output$av_MAADpanel <- renderUI({
    
    tabsetPanel(id = "av_analyseTabset",
                tabPanel("data",
                         uiOutput("av_maadDataTab")),
                tabPanel("Additive doses",
                         uiOutput(outputId = "av_maadAdditiveTab")),
                tabPanel("Regenerative doses",
                         uiOutput(outputId = "av_maadRegenerativeTab")),
                tabPanel("Results",
                         uiOutput(outputId = "av_maadResultTab")))
    
  })
  
  output$av_maadDataTab <- renderUI({
    
    pretreatment <- av_DATA.pretreatment()
    
    if(is.null(pretreatment)){
      return(NULL)
    }
    
    # Eval
    eval.Tmin <- as.numeric(input$av_evalSlider[1])
    eval.Tmax <- as.numeric(input$av_evalSlider[2])
    
    #Plot
    plot.Tmin <- input$av_interestSlider[1]
    plot.Tmax <- input$av_interestSlider[2]
    
    plotting.parameters=list(plot.Tmin=plot.Tmin,
                             plot.Tmax=plot.Tmax)
    
    temperatures <- numeric()
    TL <- numeric()
    dTypes <- character()
    
    records <- pretreatment@records
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.dTypes <- temp.record@metadata$DTYPE
      temp.temperatures <- temp.record@temperatures
      temp.TL <- temp.record@data
      
      dTypes <- c(dTypes, temp.dTypes)
      temperatures <- cbind(temperatures, temp.temperatures)
      TL <- cbind(TL, temp.TL)
    }
    
    plotData <- list(eval.Tmin = eval.Tmin,
                     eval.Tmax = eval.Tmax,
                     dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL,
                     plotting.parameters = plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.analyse, plotData)})
    ))
  })
  
  output$av_maadAdditiveTab <- renderUI({
    data <- av_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    plotData <- data[[1]]@plotData
    
    Lx.data <- list(plot.name="Lx",
                    temperatures = plotData$temperatures,
                    names = plotData$aNames,
                    doses = plotData$aDoses,
                    Lx = plotData$aLx,
                    Lx.plateau = plotData$aLx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    Tx.data <- list(plot.name="Tx",
                    temperatures = plotData$temperatures,
                    names = plotData$aNames,
                    doses = plotData$aDoses,
                    Lx = plotData$aTx,
                    Lx.plateau = plotData$aTx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    LxTx.data <- list(plot.name="LxTx",
                      temperatures = plotData$temperatures,
                      names = plotData$aNames,
                      doses = plotData$aDoses,
                      Lx = plotData$aLxTx,
                      Lx.plateau = plotData$aLxTx.plateau,
                      eval.Tmin = plotData$eval.Tmin,
                      eval.Tmax = plotData$eval.Tmax,
                      plotting.parameters = plotData$plotting.parameters)
    
    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })
  
  output$av_maadRegenerativeTab <- renderUI({
    data <- av_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    plotData <- data[[1]]@plotData
    
    Lx.data <- list(plot.name="Lx",
                    temperatures = plotData$temperatures,
                    names = plotData$rNames,
                    doses = plotData$rDoses,
                    Lx = plotData$rLx,
                    Lx.plateau = plotData$rLx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    Tx.data <- list(plot.name="Tx",
                    temperatures = plotData$temperatures,
                    names = plotData$rNames,
                    doses = plotData$rDoses,
                    Lx = plotData$rTx,
                    Lx.plateau = plotData$rTx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    LxTx.data <- list(plot.name="LxTx",
                      temperatures = plotData$temperatures,
                      names = plotData$rNames,
                      doses = plotData$rDoses,
                      Lx = plotData$rLxTx,
                      Lx.plateau = plotData$rLxTx.plateau,
                      eval.Tmin = plotData$eval.Tmin,
                      eval.Tmax = plotData$eval.Tmax,
                      plotting.parameters = plotData$plotting.parameters)
    
    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })
  
  
  output$av_maadResultTab <- renderUI({
    data <- av_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    plotData <- data[[1]]@plotData
    
    ## Plot ##
    
    plotData.DP <- list(temperatures = plotData$temperatures,
                        eval.Tmin = plotData$eval.Tmin,
                        eval.Tmax = plotData$eval.Tmax,
                        DP.Q.line = plotData$DP.Q.line,
                        DP.Q.line.error = plotData$DP.Q.line.error,
                        DP.I.line = plotData$DP.I.line,
                        DP.I.line.error = plotData$DP.I.line.error,
                        Q.DP = plotData$Q.DP,
                        Q.DP.error = plotData$Q.DP.error,
                        I.DP = plotData$I.DP,
                        I.DP.error = plotData$I.DP.error,
                        plotting.parameters= plotData$plotting.parameters)
    
    plotData.GC <- list(temperatures = plotData$temperatures,
                        eval.Tmin = plotData$eval.Tmin,
                        eval.Tmax = plotData$eval.Tmax,
                        GC.Q.line = plotData$GC.Q.line,
                        GC.Q.slope = plotData$GC.Q.slope,
                        GC.Q.LxTx = plotData$GC.Q.LxTx,
                        GC.Q.LxTx.error = plotData$GC.Q.LxTx.error,
                        GC.Q.doses = plotData$GC.Q.doses,
                        GC.Q.names = plotData$GC.Q.names,
                        GC.I.line = plotData$GC.I.line,
                        GC.I.slope = plotData$GC.I.slope,
                        GC.I.LxTx = plotData$GC.I.LxTx,
                        GC.I.LxTx.error = plotData$GC.I.LxTx.error,
                        GC.I.doses = plotData$GC.I.doses,
                        GC.I.names = plotData$GC.I.names,
                        Q.DP = plotData$Q.DP,
                        Q.DP.error = plotData$Q.DP.error,
                        Q.GC = plotData$Q.GC,
                        Q.GC.error = plotData$Q.GC.error,
                        I.DP = plotData$I.DP,
                        I.DP.error = plotData$I.DP.error,
                        I.GC = plotData$I.GC,
                        I.GC.error = plotData$I.GC.error,
                        rejection.values = plotData$rejection.values,
                        fitting.parameters = plotData$fitting.parameters,
                        plotting.parameters= plotData$plotting.parameters)    
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.MAAD_DP, plotData.DP)}),
                    renderPlot({do.call(shinyPlot_TL.MAAD_GC, plotData.GC)})
    ))
  })
  
  output$av_SARpanel <- renderUI({
    
    info <- av_DATA.info()
    
    if(is.null(info)){
      return(NULL)
    }
    
    nAliquot <- info$nDiscs
    
    fluidRow(column(width = 12,
                    numericInput(inputId = "av_selectAliquot",
                                 label = "Aliquot selection",
                                 value = 1,
                                 min = 1,
                                 max = nAliquot,
                                 step = 1),
                    uiOutput(outputId = "av_sarDiscPanel")))
  })
  
  output$av_sarDiscPanel <- renderUI({
    
    tabsetPanel(id = "av_analyseTabset",
                tabPanel("Data",
                         uiOutput(outputId = "av_sarDataTab")),
                tabPanel("Regenerative doses",
                         uiOutput(outputId = "av_sarRegenerativeTab")),
                tabPanel("Results",
                         uiOutput(outputId = "av_sarResultsTab")))
  })
  
  output$av_sarDataTab <- renderUI({
    
    pretreatment <- av_DATA.pretreatment()
    info <- av_DATA.info()
    
    if(is.null(pretreatment)){
      return(NULL)
    }
    
    # Eval
    eval.Tmin <- as.numeric(input$av_evalSlider[1])
    eval.Tmax <- as.numeric(input$av_evalSlider[2])
    
    #Plot
    plot.Tmin <- input$av_interestSlider[1]
    plot.Tmax <- input$av_interestSlider[2]
    
    plotting.parameters=list(plot.Tmin=plot.Tmin,
                             plot.Tmax=plot.Tmax)
    
    
    index <- input$av_selectAliquot
    
    aliquots <- info$positions
    
    dTypes <- character()
    temperatures <- numeric()
    TL <- numeric()
    
    records <- pretreatment@records
    
    
    
    for(i in 1:length(records)){
      temp.record <- records[[i]]
      
      temp.aliquot <- temp.record@metadata$POSITION
      
      if(temp.aliquot == aliquots[index]){
        
        temp.dTypes <- temp.record@metadata$DTYPE
        
        temp.temperatures <- temp.record@temperatures
        temp.TL <- temp.record@data
        
        dTypes <- c(dTypes,temp.dTypes)
        temperatures <- cbind(temperatures, temp.temperatures)
        TL <- cbind(TL, temp.TL)
      }
    }
    
    plotData <- list(eval.Tmin = eval.Tmin,
                     eval.Tmax = eval.Tmax,
                     dTypes = dTypes,
                     temperatures = temperatures,
                     TL = TL,
                     plotting.parameters = plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.analyse, plotData)})
    ))
  })
  
  output$av_sarRegenerativeTab <- renderUI({
    
    data <- av_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    aliquot <- input$av_selectAliquot
    plotData <- data[[aliquot]]@plotData
    
    Lx.data <- list(plot.name="Lx",
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    doses = plotData$doses,
                    Lx = plotData$Lx,
                    Lx.plateau = plotData$Lx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    Tx.data <- list(plot.name="Tx",
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    doses = plotData$doses,
                    Lx = plotData$Tx,
                    Lx.plateau = plotData$Tx.plateau,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    plotting.parameters = plotData$plotting.parameters)
    
    LxTx.data <- list(plot.name="LxTx",
                      temperatures = plotData$temperatures,
                      names = plotData$names,
                      doses = plotData$doses,
                      Lx = plotData$LxTx,
                      Lx.plateau = plotData$LxTx.plateau,
                      eval.Tmin = plotData$eval.Tmin,
                      eval.Tmax = plotData$eval.Tmax,
                      plotting.parameters = plotData$plotting.parameters)
    
    ## Plot ##
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.HP, Lx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, Tx.data)}),
                    renderPlot({do.call(shinyPlot_TL.HP, LxTx.data)})))
  })
  
  output$av_sarResultsTab <- renderUI({
    
    data <- av_DATA.analyse()
    
    if(is.null(data)){
      return(NULL)
    }
    
    aliquot <- input$av_selectAliquot
    plotData <- data[[aliquot]]@plotData
    
    DP.data <- list(eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    temperatures = plotData$temperatures,
                    DP.Q.line = plotData$DP.Q.line,
                    DP.Q.line.error = plotData$DP.Q.line.error,
                    Q.DP = plotData$Q.DP,
                    Q.DP.error = plotData$Q.DP.error,
                    TxTn = plotData$TxTn,
                    rejection.values = plotData$rejection.values,
                    plotting.parameters = plotData$plotting.parameters)
    
    GC.data <- list(fitting.parameters = plotData$fitting.parameters,
                    eval.Tmin = plotData$eval.Tmin,
                    eval.Tmax = plotData$eval.Tmax,
                    temperatures = plotData$temperatures,
                    names = plotData$names,
                    names.duplicated = plotData$names.duplicated,
                    doses = plotData$doses,
                    GC.Q.line = plotData$GC.Q.line,
                    GC.Q.LxTx = plotData$GC.Q.LxTx,
                    GC.Q.LxTx.error = plotData$GC.Q.LxTx.error,
                    GC.Q.slope = plotData$GC.Q.slope,
                    Q.DP = plotData$Q.DP,
                    Q.DP.error = plotData$Q.DP.error,
                    Q.GC = plotData$Q.GC,
                    Q.GC.error = plotData$Q.GC.error,
                    rejection.values = plotData$rejection.values,
                    plotting.parameters = plotData$plotting.parameters)
    
    fluidRow(column(width = 12,
                    renderPlot({do.call(shinyPlot_TL.SAR_DP, DP.data)}),
                    renderPlot({do.call(shinyPlot_TL.SAR_GC, GC.data)})
    ))
    
    
  })
  
  av_DATA.analyse <- reactive({
    
    temp <- input$av_analyseButton
    
    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- av_generate.analyse()
    }
    
    return(data)
  })
  
  av_generate.analyse <- eventReactive(input$av_analyseButton,{
    
    data <- av_DATA.pretreatment()
    if(is.null(data)){
      return(NULL)
    }
    
    info <- av_DATA.info()
    
    protocol <- info$protocol
    
    # Parameters
    
    # Eval
    eval.Tmin <- as.numeric(input$av_evalSlider[1])
    eval.Tmax <- as.numeric(input$av_evalSlider[2])
    
    # Plotting
    plot.Tmin <- as.numeric(input$av_interestSlider[1])
    plot.Tmax <- as.numeric(input$av_interestSlider[2])
    
    plotting.parameters <- list(plot.Tmin=plot.Tmin,
                                plot.Tmax=plot.Tmax,
                                plateau.Tmin =eval.Tmin,
                                plateau.Tmax=eval.Tmax,
                                no.plot=TRUE)
    
    #Fitting parameters
    if(protocol == "MAAD"){
      
      # Fitting
      fit.method <- input$av_fittingMethod
      fit.weighted <- input$av_weightCheck
      fit.use.slope <- input$av_slopeCheck
      fit.rDoses.min <- as.numeric(input$av_rDoseSlider[1])
      fit.rDoses.max <- as.numeric(input$av_rDoseSlider[2])
      fit.aDoses.min <- as.numeric(input$av_aDoseSlider[1])
      fit.aDoses.max <- as.numeric(input$av_aDoseSlider[2])
      
      fitting.parameters <- list(fit.method=fit.method,
                                 fit.weighted=fit.weighted,
                                 fit.use.slope=fit.use.slope,
                                 fit.aDoses.min=fit.aDoses.min,
                                 fit.aDoses.max=fit.aDoses.max,
                                 fit.rDoses.min=fit.rDoses.min,
                                 fit.rDoses.max=fit.rDoses.max
      )
      
      # Rejection
      testdose.error <- input$av_testdoseError
      paleodose.error <- input$av_paleodoseError
      
      rejection.criteria <- list(testdose.error = testdose.error,
                                 paleodose.error = paleodose.error)
      
    }else if(protocol == "SAR"){
      # Fitting
      fit.method <- input$av_fittingMethod
      fit.weighted <- input$av_weightCheck
      fit.rDoses.min <- as.numeric(input$av_rDoseSlider[1])
      fit.rDoses.max <- as.numeric(input$av_rDoseSlider[2])
      
      fitting.parameters <- list(fit.method=fit.method,
                                 fit.weighted=fit.weighted,
                                 fit.rDoses.min=fit.rDoses.min,
                                 fit.rDoses.max=fit.rDoses.max
      )
      
      # Rejection
      recycling.ratio <- input$av_recyclingRatio
      recuperation.rate <- input$av_recuparationRate
      testdose.error <- input$av_testdoseError
      paleodose.error <- input$av_paleodoseError
      
      rejection.criteria <- list(recycling.ratio = recycling.ratio,
                                 recuperation.rate = recuperation.rate,
                                 testdose.error = testdose.error,
                                 paleodose.error = paleodose.error)
    }
    
    
    results <- list()
    
   if(protocol == "MAAD"){
      results[[1]] <- analyse_TL.MAAD(object = data,
                                      eval.Tmin = eval.Tmin,
                                      eval.Tmax = eval.Tmax,
                                      rejection.criteria = rejection.criteria,
                                      fitting.parameters = fitting.parameters,
                                      plotting.parameters = plotting.parameters)
      
    }else if(protocol == "SAR"){
      positions <- vector()
      
      for(i in 1: length(data@records)){
        temp.record <- data@records[[i]]
        temp.position <- temp.record@metadata$POSITION
        
        positions[i] <- temp.position
      }
      positions <- unique(positions)
      
      for(i in 1:length(positions)){
        
        temp.data <- mod_extract.aliquot(object = data,
                                         list = positions[i])
        
        temp.result <- analyse_TL.SAR(object= temp.data,
                                      eval.Tmin=eval.Tmin,
                                      eval.Tmax=eval.Tmax,
                                      fitting.parameters=fitting.parameters,
                                      plotting.parameters=plotting.parameters,
                                      rejection.criteria=rejection.criteria
        )
        
        results[[i]] <- temp.result
      }
    }else{
      results <- NULL
    }
    
    updateTabsetPanel(session,"av_analyseTabset","Results")
    
    return(results)
  })
  
  #############################################
  # Summary
  #############################################
  
  output$av_aTab <- renderUI({
    
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = "av_displayPanel"),
        uiOutput(outputId = "av_selectionPanel"),
        uiOutput(outputId = "av_s2GyPannel"),
        uiOutput(outputId = "av_b2aPannel"),
        uiOutput(outputId = "av_aButton"),
        uiOutput(outputId = "av_aText")
      ),
      mainPanel(
        uiOutput(outputId = "av_dePanel"))
    )
  })
  
  output$av_displayPanel <- renderUI({
    protocol <- input$av_protocol
    
    if(is.null(protocol)){
      return(NULL)
    }
    
    if(protocol %in% c("MAAD")){
      fluidRow(column(width = 12,
                      h4("Display parameters"),
                      selectInput(inputId = "av_uncertaintyDisplay",
                                  label = "Uncertainty",
                                  choices = list("absolute", "relative"),
                                  selected = "absolute")
      ))
    }else if(protocol %in% c("SAR")){
      fluidRow(column(width = 12,
                      h4("Display parameters"),
                      selectInput(inputId = "av_uncertaintyDisplay",
                                  label = "Uncertainty",
                                  choices = list("absolute", "relative"),
                                  selected = "absolute"),
                      selectInput(inputId = "av_plotSelection",
                                  label = "Plot",
                                  choices = list("abanico", "radial", "KDE", "histogram"),
                                  selected = "abanico")
      ))
    }
  })
  
  output$av_selectionPanel <- renderUI({
    protocol <- input$av_protocol
    
    if(protocol == "MAAD"){
      fluidRow(column(width = 12,
                      h4("D\u2091 selection"),
                      selectInput(inputId = "av_approachSelection",
                                  label = "Approach",
                                  choices = list("dose plateau", "growth curve"),
                                  selected = "growth curve"),
                      selectInput(inputId = "av_resultSelection",
                                  label = "Equivalent dose",
                                  choices = list("De", "Q", "I"),
                                  selected = "De")
      ))
    }else if(protocol == "SAR"){
      fluidRow(column(width = 12,
                      h4("D\u2091 selection"),
                      selectInput(inputId = "av_approachSelection",
                                  label = "Approach",
                                  choices = list("dose plateau", "growth curve"),
                                  selected = "growth curve"),
                      selectInput(inputId = "av_methodSelection",
                                  label = "Method selection",
                                  choices = list("weighted", "unweighted", "MCM"),
                                  selected = "weighted"),
                      selectInput(inputId = "av_averageSelection",
                                  label = "Average Selection",
                                  choices = list("mean", "median"),
                                  selected = "mean"),
                      selectInput(inputId = "av_errorSelection",
                                  label = "Uncertainty Selection",
                                  choices = list("sd", "se"),
                                  selected = "sd")
      ))
    }
  })
  
  output$av_aButton <- renderUI({
    
    protocol <- input$av_protocol
    
    if(protocol %in% c("SAR", "MAAD")){
      actionButton(inputId = "av_aButton",
                   label = "Calculate a")
    }
  })
  
  output$av_aText <- renderUI({
    
    protocol <- input$av_protocol
    
    analyse <- av_DATA.analyse()
    a.Values <- av_DATA.a()
    
    temp <- input$av_aButton
    
    if(protocol %in% c("SAR", "MAAD")){
      
      if(is.null(analyse)){
        helpText("No data")
        
      }else if(is.null(a.Values)){
        helpText("Waiting for a estimation.")
        
      }else{
        helpText("a calculated")
      }
    }
  })
  
  output$av_averageSelection <- renderUI({
    
    protocol <- input$av_protocol
    
    if(protocol %in% c("SAR")){
      selectInput(inputId = "av_averageSelection",
                  label = "Average Selection",
                  choices = list("mean", "median"),
                  selected = "mean")
    }else{
      return(NULL)
    }
  })
  
  output$av_s2GyPannel <- renderUI({
    
    protocol <- input$av_protocol
    
    info <- av_DATA.info()
    
    if(!is.null(info)){
      
      s2Gy <- info$s2Gy
      s2Gy_err <- info$s2Gy_err
      
    }else{
      s2Gy <- ""
      s2Gy_err <- ""
    }
    
    if(!is.finite(s2Gy) || !is.finite(s2Gy_err)){
      s2Gy <- NULL
      s2Gy_err <- NULL
    }else{
      s2Gy <- round(s2Gy, 4)
      s2Gy_err <- round(s2Gy_err, 4)
    }
    
    if(protocol %in% c("MAAD","SAR")){
      
      
      fluidRow(
        h5(tags$b("Equivalent dose conversion")),
        
        column(width = 6,
               textInput(inputId = "av_beta2Gy",
                         label = "D\u0309\u03b2 [Gy/s]",
                         value = s2Gy,
                         placeholder = "required")
        ),
        column(width = 6,
               textInput(inputId = "av_beta2Gy_err",
                         label = "\u03B4D\u0309\u03b2",
                         value = s2Gy_err,
                         placeholder = "required")
        )
      )
    }
  })
  
  output$av_b2aPannel <- renderUI({
    
    protocol <- input$av_protocol
    
    if(protocol %in% c("MAAD","SAR")){
      
      fluidRow(column(width = 12,
                      h5(tags$b("\u03b1 dose")),
                      
                      fluidRow(column(width = 6,
                                      textInput(inputId = "av_alpha",
                                                label = "\u03b1 [s]",
                                                placeholder = "required")),
                               column(width = 6,
                                      textInput(inputId = "av_alpha_err",
                                                label = "\u03B4\u03b1",
                                                placeholder = "required"))),
                      fluidRow(column(width = 6,
                                      textInput(inputId = "av_alpha2Gy",
                                                label = "D\u0309\u03b1 [Gy/s]",
                                                placeholder = "required")),
                               column(width = 6,
                                      textInput(inputId = "av_alpha2Gy_err",
                                                label = "\u03B4D\u0309\u03b1",
                                                placeholder = "required")))
                      ))
    }
  })
  
  output$av_dePanel <- renderUI({
    
    protocol <- input$av_protocol
    
    if(protocol == "MAAD"){
      uiOutput(outputId = "av_summaryMAADpanel")
    }else if(protocol == "SAR"){
      uiOutput(outputId = "av_summarySARpanel")
    }else{
      return (NULL)
    }
  })
  
  output$av_summaryMAADpanel <- renderUI({
    
    fluidRow(column(width = 12,
                    DT::dataTableOutput(outputId = "av_MAADTable"),
                    DT::dataTableOutput(outputId = "av_aTable")
    ))
  })
  
  output$av_summarySARpanel <- renderUI({
    
    fluidRow(column(width = 12,
                    uiOutput(outputId = "av_sarPlot"),
                    DT::dataTableOutput(outputId = "av_aTable")
    ))
  })
  
  output$av_MAADTable <- DT::renderDataTable({
    av_TABLE.maad()
  })
  
  av_TABLE.maad <- reactive({
    
    uncertainty <- input$av_uncertaintyDisplay
    
    if(is.null(uncertainty)){
      return(NULL)
    }
    
    analyse <- av_DATA.analyse()
    
    if(is.null(analyse) || is.null(info)){
      DP <- data.frame(Method = "GC",
                       Q = NA,
                       I = NA,
                       De = NA)
      
      GC <- data.frame(Method = "DP",
                       Q = NA,
                       I = NA,
                       De = NA)
    }else{
      temp.data <- analyse[[1]]
      
      DP.Q <- as.numeric(temp.data@data$DP$Q)
      DP.Q.error <- as.numeric(temp.data@data$DP$Q.error)
      DP.I <- as.numeric(temp.data@data$DP$I)
      DP.I.error <- as.numeric(temp.data@data$DP$I.error)
      DP.De <- as.numeric(temp.data@data$DP$De)
      DP.De.error <- as.numeric(temp.data@data$DP$De.error)
      
      GC.Q <- as.numeric(temp.data@data$GC$Q)
      GC.Q.error <- as.numeric(temp.data@data$GC$Q.error)
      GC.I <- as.numeric(temp.data@data$GC$I)
      GC.I.error <- as.numeric(temp.data@data$GC$I.error)
      GC.De <- as.numeric(temp.data@data$GC$De)
      GC.De.error <- as.numeric(temp.data@data$GC$De.error)
      
      if(uncertainty == "absolute"){
        DP <- data.frame(Method = "DP",
                         Q = paste(round(DP.Q,2), "\u00B1", round(DP.Q.error,2)),
                         I = paste(round(DP.I,2), "\u00B1", round(DP.I.error,2)),
                         De = paste(round(DP.De,2), "\u00B1", round(DP.De.error,2)))
        
        GC <- data.frame(
          Method = "GC",
          Q = paste(round(GC.Q,2), "\u00B1", round(GC.Q.error,2)),
          I = paste(round(GC.I,2), "\u00B1", round(GC.I.error,2)),
          De = paste(round(GC.De,2), "\u00B1", round(GC.De.error,2)))
        
      }else if (uncertainty == "relative"){
        DP <- data.frame(Method = "DP",
                         Q = paste(round(DP.Q,2), "\u00B1", round(DP.Q.error/DP.Q,2), "[%]"),
                         I = paste(round(DP.I,2), "\u00B1", round(DP.I.error/DP.I,2), "[%]"),
                         De = paste(round(DP.De,2), "\u00B1", round(DP.De.error/DP.De,2), "[%]"))
        
        GC <- data.frame(Method = "GC",
                         Q = paste(round(GC.Q,2), "\u00B1", round(GC.Q.error/GC.Q,2), "[%]"),
                         I = paste(round(GC.I,2), "\u00B1", round(GC.I.error/GC.I,2), "[%]"),
                         De = paste(round(GC.De,2), "\u00B1", round(GC.De.error/GC.De,2), "[%]"))
      }     
    }
    
    table <- rbind(DP, GC)
    
    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th('Approach'),
          tags$th('Q'),
          tags$th('I'),
          tags$th('D\u2091')
        )
      )
    )
    
    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))    
    return(datatable)
  })
  
  output$av_sarPlot <- renderUI({
    
    data <- av_DATA.analyse()
    
    method <- input$av_methodSelection
    average <- input$av_averageSelection
    error <- input$av_errorSelection
    
    uncertainty <- input$av_uncertaintyDisplay
    plot <- input$av_plotSelection
    
    if(uncertainty == "absolute"){
      error <- paste(error,".abs",sep = "")
    }else{
      error <- paste(error,".rel",sep = "")
    }
    
    #For radial & Histogram plot
    error.old <- character()
    
    if(error == "sd"){
      error.old <- "sd" 
    }else{
      error.old <- "se"
    }
    
    if(uncertainty == "relative"){
      error.old <- paste(error.old,"rel",sep = "")
    }else{
      error.old <- paste(error.old,"abs",sep = "")
    }
    
    if(method %in% c("weighted","MCM")){
      error.old <- paste(error.old,".weighted",sep = "")
    }
    
    
    if(is.null(data) || is.null(average)){
      return(NULL)
    }
    
    DP.De <- vector()
    DP.De_err <- vector()
    
    GC.De <- vector()
    GC.De_err <- vector()
    
    for(i in 1 : length(data)){
      temp.data <- data[[i]]
      
      DP.De[i] <- temp.data@data$DP$De
      DP.De_err[i] <- temp.data@data$DP$De.error
      
      GC.De[i] <- temp.data@data$GC$De
      GC.De_err[i] <- temp.data@data$GC$De.error
    }
    
    
    DP.values <- data.frame(De = DP.De,
                            De_err= DP.De_err)
    GC.values <- data.frame(De = GC.De,
                            De_err= GC.De_err)
    
    #Plot
    
    if(plot == "abanico"){
      fluidRow(
        h4("Abanico Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_AbanicoPlot(DP.values,
                                  stats=c("min","max"),
                                  summary=c("n",average, error),
                                  summary.method = method,
                                  log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_AbanicoPlot(GC.values,
                                  stats=c("min","max"),
                                  summary=c("n",average, error),
                                  summary.method = method,
                                  log.z = FALSE)
               })
        ))
    }else if(plot == "radial"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_RadialPlot(DP.values,
                                 stats=c("min","max"),
                                 summary=c("n",average, error.old),
                                 log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_RadialPlot(GC.values,
                                 stats=c("min","max"),
                                 summary=c("n",average, error.old),
                                 log.z = FALSE)
               })
        ))
    }else if(plot == "KDE"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_KDE(DP.values,
                          stats=c("min","max"),
                          summary=c("n",average, error),
                          summary.method = method,
                          log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_KDE(GC.values,
                          stats=c("min","max"),
                          summary=c("n",average, error),
                          summary.method = method,
                          log.z = FALSE)
               })
        ))
    }else if(plot == "histogram"){
      fluidRow(
        h4("Radial Plot"),
        column(width = 6,
               h4("Dose plateau approach"),
               renderPlot({
                 plot_Histogram(DP.values,
                                stats=c("min","max"),
                                summary=c("n",average, error.old),
                                log.z = FALSE)
               })
        ),
        
        column(width = 6,
               h4("Growth curve approach"),
               renderPlot({
                 plot_Histogram(GC.values,
                                stats=c("min","max"),
                                summary=c("n",average, error.old),
                                log.z = FALSE)
               })
        ))
    }
  })
  
  output$av_aTable <- renderDataTable({
    av_TABLE.a()
  })
  
  av_TABLE.a <- reactive({
    
    sample <- input$sa_sample
    
    info <- av_DATA.info()
    a.Values <- av_DATA.a()
    
    if(is.null(info)){
      nDiscs <- 0
    }else{
      nDiscs <- as.character(info$nDiscs)
    }
    
    if(is.null(a.Values)){
      a.text <- ""
      alpha.text <- ""
      beta.text <- ""
      
    }else{
      a <- a.Values$a
      a_err <- a.Values$a_err
      a.text <- paste(round(a, 3), "\u00B1", round(a_err, 3))
      
      alpha <- a.Values$alpha
      alpha_err <- a.Values$alpha_err
      alpha.text <- paste(round(alpha, 3), "\u00B1", round(alpha_err, 3))
      
      beta <- a.Values$beta
      beta_err <- a.Values$beta_err
      beta.text <- paste(round(beta, 3), "\u00B1", round(beta_err, 3))
    }
    
    table <- data.frame(Sample = sample,
                        nDiscs = nDiscs,
                        alpha = alpha.text,
                        beta = beta.text,
                        a = a.text)
    
    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th('Sample'),
          tags$th('Aliquots'),
          tags$th('\u03B1 [Gy]'),
          tags$th('\u03B2 [Gy]'),
          tags$th('a-Value')
          
        )
      )
    )
    
    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))
    
    return(datatable)
  })
  
  av_DATA.a <- reactive({
    temp <- input$av_aButton
    
    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- av_generate.a()
    }
    
    return(data)
  })
  
  av_generate.a <- eventReactive(input$av_aButton,{
    
    info <- av_DATA.info()
    data <- av_DATA.analyse()
    
    if(is.null(data) || is.null(info)){
      return(NULL)
    }
    
    approach <- input$av_approachSelection
    result <- input$av_resultSelection
    method <- input$av_methodSelection
    average <- input$av_averageSelection
    error <- input$av_errorSelection
    
    beta2Gy <- as.numeric(input$av_beta2Gy)
    beta2Gy_err <- as.numeric(input$av_beta2Gy_err)
    
    if(!is.finite(beta2Gy) || beta2Gy <= 0 ){
      beta2Gy <- NA
      beta2Gy_err <- NA
    }
    
    alpha <- as.numeric(input$av_alpha)
    alpha_err <- as.numeric(input$av_alpha_err)
    
    if(!is.finite(alpha) || alpha <= 0 ){
      alpha <- NA
      alpha_err <- NA
    }
    
    alpha2Gy <- as.numeric(input$av_alpha2Gy)
    alpha2Gy_err <- as.numeric(input$av_alpha2Gy_err)
    
    if(!is.finite(alpha2Gy) || alpha2Gy <= 0 ){
      alpha2Gy <- NA
      alpha2Gy_err <- NA
    }
    
    protocol <- info$protocol
    
    temp.beta <- NULL
    temp.beta_err <- NULL
    
    if(protocol == "MAAD"){
      
      
      temp.data <- data[[1]]
      
      DP.Q <- as.numeric(temp.data@data$DP$Q)
      DP.Q_err <- as.numeric(temp.data@data$DP$Q.error)
      DP.I <- as.numeric(temp.data@data$DP$I)
      DP.I_err <- as.numeric(temp.data@data$DP$I.error)
      DP.De <- as.numeric(temp.data@data$DP$De)
      DP.De_err <- as.numeric(temp.data@data$DP$De.error)
      
      GC.Q <- as.numeric(temp.data@data$GC$Q)
      GC.Q_err <- as.numeric(temp.data@data$GC$Q.error)
      GC.I <- as.numeric(temp.data@data$GC$I)
      GC.I_err <- as.numeric(temp.data@data$GC$I.error)
      GC.De <- as.numeric(temp.data@data$GC$De)
      GC.De_err <- as.numeric(temp.data@data$GC$De.error)
      
      if(approach == "dose plateau"){
        Q <- DP.Q
        Q_err <- DP.Q_err
        I <- DP.I
        I_err <-DP.I_err
        De <- DP.De
        De_err <- DP.De_err
      }else if(approach == "growth curve"){
        Q <- GC.Q
        Q_err <- GC.Q_err
        I <- GC.I
        I_err <-GC.I_err
        De <- GC.De
        De_err <- GC.De_err
      }
      
      if(result == "Q"){
        temp.beta <- Q
        temp.beta_err <- Q_err
      }else if(result == "I"){
        temp.beta <- I
        temp.beta_err <- I_err
      }else if(result == "De"){
        temp.beta <- De
        temp.beta_err <- De_err
      }
      
    }else if(protocol == "SAR"){
      
      DP.De <- vector()
      DP.De_err <- vector()
      
      GC.De <- vector()
      GC.De_err <- vector()
      
      for(i in 1 : length(data)){
        temp.data <- data[[i]]
        
        DP.De[i] <- temp.data@data$DP$De
        DP.De_err[i] <- temp.data@data$DP$De.error
        
        GC.De[i] <- temp.data@data$GC$De
        GC.De_err[i] <- temp.data@data$GC$De.error
      }
      
      if(approach == "dose plateau"){
        De <- DP.De
        De_err <- DP.De_err
      }else if(approach == "growth curve"){
        De <- GC.De
        De_err <- GC.De_err
      }
      
      De.val <- data.frame(De=De,
                           De.error = De_err)
      
      stats <- calc_Statistics(data = De.val)
      
      if(method== "weighted"){
        de.data <- stats$weighted
      }else if(method == "unweighted"){
        de.data <- stats$unweighted
      }else if(method == "MCM"){
        de.data <- stats$MCM
      }
      
      if(average == "mean"){
        temp.beta <- de.data$mean
      }else if(average == "median"){
        temp.beta <- de.data$median
      }
      
      if(error == "sd"){
        temp.beta_err <- de.data$sd.abs
      }else if(error == "se"){
        temp.beta_err <- de.data$se.abs
      }
      
    }
    
    new.beta <- temp.beta*beta2Gy
    beta_rel <- sqrt(sum((temp.beta_err/temp.beta)^2,(beta2Gy_err/beta2Gy)^2,na.rm = TRUE))
    new.beta_err <- new.beta*beta_rel
    
    new.alpha <- alpha*alpha2Gy
    alpha_rel <- sqrt(sum((alpha_err/alpha)^2,(alpha2Gy_err/alpha2Gy)^2,na.rm = TRUE))
    new.alpha_err <- new.alpha*alpha_rel
    
    new.a <- new.beta/new.alpha
    a_rel <- sqrt(sum((new.alpha_err/new.alpha)^2,(new.beta_err/new.beta)^2,na.rm = TRUE))
    new.a_err <- new.a*a_rel
    
    result <- list(a = new.a,
                   a_err = new.a_err,
                   alpha = new.alpha,
                   alpha_err = new.alpha_err,
                   beta = new.beta,
                   beta_err = new.beta_err)
    
    return(result)
  })
  
  ##############################################################################################
  # Dr Estimation
  ##############################################################################################

  output$drPage <- renderUI({
    tabsetPanel(id = "drPage",
                tabPanel("Data",
                         uiOutput(outputId = "dataDrTab")),
                tabPanel("D\u0309",
                         uiOutput(outputId = "resultDrTab"))
    )
  })

  #############################################
  # Data
  #############################################

  output$dataDrTab <- renderUI({

    sidebarLayout(
      uiOutput("inputSidePanel"),
      uiOutput("inputMainPanel")
      )
  })

  output$inputSidePanel <- renderUI({
    
    sidebarPanel(width = 3,
                 h4("General parameters"),
                 selectInput(inputId = "material",
                             label = "Context",
                             choices = c("sediment",
                                         "flint",
                                         "ceramic",
                                         "cave sediment",
                                         "cave flint"),
                             selected = "sediment"),
                 selectInput(inputId = "mineral",
                             label = "Mineral",
                             choices = c("Q","F","PM"),
                             selected = "Q"),
                 selectInput(inputId = "conversionFactor",
                             label = "Conversion factor",
                             choices = c("AdamiecAitken1998",
                                         "Guerinetal2011",
                                         "Liritzisetal2013",
                                         "X"),
                             selected = "Liritzisetal2013"),
                 selectInput(inputId = "alphaSizeFactor",
                             label = "Alpha size attenuation factor",
                             choices = c("Bell1980",
                                         "Brennanetal1991"),
                             selected = "Brennanetal1991"),
                 uiOutput(outputId="betaSizeFactor"),
                 
                 selectInput(inputId = "betaEtchFactor",
                             label = "Beta etch attenuation factor",
                             choices = c("Bell1979",
                                         "Brennan2003"),
                             selected = "Brennan2003"),
                 
                 br(),
                 actionButton(inputId = "dracButton",
                              label = "D\u0309 estimation"),
                 
                 uiOutput(outputId= "dracText")
                 )
  })

  output$inputMainPanel <- renderUI({
    mainPanel(width = 9,
              fluidRow(
                uiOutput(outputId= "m1_column"),
                uiOutput(outputId= "m2_column"),
                uiOutput(outputId= "m3_column"),
                uiOutput(outputId = "dc_column")
              ))
  })
  
  output$m1_column <- renderUI({
    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){
      column(width = 3,
             uiOutput(outputId="m1_text"),

             uiOutput(outputId = "m1_doseRateBox"),

             uiOutput(outputId="m1_U"),
             uiOutput(outputId="m1_Th"),
             uiOutput(outputId="m1_K"),
             uiOutput(outputId="m1_K2RbBox"),
             uiOutput(outputId="m1_Rb"),


             uiOutput(outputId="m1_alpha"),
             uiOutput(outputId="m1_beta"),
             uiOutput(outputId="m1_gamma"),

             uiOutput(outputId="m1_sizeText"),
             uiOutput(outputId="m1_size"),

             uiOutput(outputId="m1_etchText"),
             uiOutput(outputId="m1_etch"),

             uiOutput(outputId="m1_aValueText"),
             uiOutput(outputId="m1_aValue"),

             uiOutput(outputId = "m1_densityText"),
             uiOutput(outputId = "m1_density"),

             uiOutput(outputId = "m1_waterText"),
             uiOutput(outputId = "m1_water")
      )
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,

             uiOutput(outputId="m1_text"),

             uiOutput(outputId = "m1_doseRateBox"),

             uiOutput(outputId="m1_U"),
             uiOutput(outputId="m1_Th"),
             uiOutput(outputId="m1_K"),
             uiOutput(outputId="m1_K2RbBox"),
             uiOutput(outputId="m1_Rb"),

             uiOutput(outputId="m1_alpha"),
             uiOutput(outputId="m1_beta"),
             uiOutput(outputId="m1_gamma"),

             uiOutput(outputId="m1_sizeText"),
             uiOutput(outputId="m1_size"),

             uiOutput(outputId="m1_etchText"),
             uiOutput(outputId="m1_etch"),

             uiOutput(outputId="m1_aValueText"),
             uiOutput(outputId="m1_aValue"),

             uiOutput(outputId = "m1_densityText"),
             uiOutput(outputId = "m1_density"),

             uiOutput(outputId = "m1_waterText"),
             uiOutput(outputId = "m1_water")
      )
    }
  })

  output$m2_column <- renderUI({
    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,

             uiOutput(outputId="m2_text"),

             uiOutput(outputId = "m2_doseRateBox"),


             uiOutput(outputId="m2_U"),
             uiOutput(outputId="m2_Th"),
             uiOutput(outputId="m2_K"),
             uiOutput(outputId="m2_K2RbBox"),
             uiOutput(outputId="m2_Rb"),

             uiOutput(outputId="m2_alpha"),
             uiOutput(outputId="m2_beta"),
             uiOutput(outputId="m2_gamma"),

             uiOutput(outputId="m2_densityText"),
             uiOutput(outputId="m2_density"),

             uiOutput(outputId="m2_waterText"),
             uiOutput(outputId="m2_water"),
             uiOutput(outputId = "m2_proportion")
      )
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,

             uiOutput(outputId="m2_text"),

             uiOutput(outputId = "m2_doseRateBox"),

             uiOutput(outputId="m2_U"),
             uiOutput(outputId="m2_Th"),
             uiOutput(outputId="m2_K"),
             uiOutput(outputId="m2_K2RbBox"),
             uiOutput(outputId="m2_Rb"),

             uiOutput(outputId="m2_alpha"),
             uiOutput(outputId="m2_beta"),
             uiOutput(outputId="m2_gamma"),

             uiOutput(outputId="m2_densityText"),
             uiOutput(outputId="m2_density"),

             uiOutput(outputId="m2_waterText"),
             uiOutput(outputId="m2_water"),


             uiOutput(outputId = "m2_proportion")
      )
    }
  })

  output$m3_column <- renderUI({

    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,

             uiOutput(outputId="m3_text"),

             uiOutput(outputId = "m3_doseRateBox"),

             uiOutput(outputId="m3_U"),
             uiOutput(outputId="m3_K"),
             uiOutput(outputId="m3_Th"),
             uiOutput(outputId="m3_K2RbBox"),
             uiOutput(outputId="m3_Rb"),


             uiOutput(outputId="m3_alpha"),
             uiOutput(outputId="m3_beta"),
             uiOutput(outputId="m3_gamma"),

             uiOutput(outputId="m3_densityText"),
             uiOutput(outputId="m3_density"),

             uiOutput(outputId="m3_waterText"),
             uiOutput(outputId="m3_water"),

             uiOutput(outputId = "m3_proportion")
      )
    }
  })

  output$dc_column <- renderUI({
    
    material <- input$material
    
    if(is.null(material)){
      return(NULL)
    }
    
    if(material %in% c("ceramic", "cave sediment", "cave flint")){
      
      column(width = 3,
             h4("Dc information"),
             
             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = "geographical position"),
             
             uiOutput(outputId = "DcLoc"),
             
             
             uiOutput(outputId="directDc"),
             
             h5(tags$b("Depth [m]")),
             
             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                placeholder = "required"))
             )
      ) 
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,
             h4("Dc information"),
             
             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = "geographical position"),
             
             uiOutput(outputId = "DcLoc"),
             
             
             uiOutput(outputId="directDc"),
             
             h5(tags$b("Depth [m]")),
             
             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                placeholder = "required"))
             )
      )
    }
  })

  output$betaSizeFactor <- renderUI({
    if(input$mineral == "Q"){
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-Q"),
                  selected = "Guerinetal2012-Q")

    }else if(input$mineral == "F"){
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-F"),
                  selected = "Guerinetal2012-F")

    }else{
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-Q",
                              "Guerinetal2012-F"),
                  selected = "Brennan2003")
    }
  })

  # m1
  # Text
  output$m1_text <- renderUI({

    if(input$material %in% c("flint","cave flint")){
      h4("Flint information")

    }else if(input$material %in% c("ceramic", "cave sediment", "sediment") ){

      h4("Grain information")
    }
  })

  output$m1_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m1_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  # Concentration
  output$m1_U <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m1_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m1_Th <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m1_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m1_K <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m1_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m1_K2RbBox <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      checkboxInput(inputId = "m1_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m1_Rb <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){

      if(!is.null(input$m1_K2RbBox) && !input$m1_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m1_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m1_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m1_alpha <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_alpha_err",
                         label = "\u03B4\u03b1"))
      )
    }
  })

  output$m1_beta <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m1_gamma <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other

  output$m1_sizeText <- renderUI({
    h5(tags$b("Grain size [\u03bcm]"))
  })

  output$m1_size <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_size_min",
                       label = "min",
                       placeholder = "required",
                       value = 100)),
      column(width = 6,
             textInput(inputId = "m1_size_max",
                       label = "max",
                       placeholder = "required",
                       value = 200))
    )
  })

  output$m1_etchText <- renderUI({
    h5(tags$b("Etch depth [\u03bcm]"))
  })

  output$m1_etch <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_etch_min",
                       label = "min",
                       placeholder = "required",
                       value = 0)),
      column(width = 6,
             textInput(inputId = "m1_etch_max",
                       label = "max",
                       placeholder = "required",
                       value = 0))
    )
  })

  output$m1_aValueText <- renderUI({
    h5(tags$b("a-value"))
  })

  output$m1_aValue <- renderUI({
    
    a.Values <- av_DATA.a()
    
    if(is.null(a.Values)){
      aVal <- ""
      aVal_err <- ""
    }else{
      aVal <- as.numeric(a.Values$a)
      aVal <- round(aVal,3)
      aVal_err <- as.numeric(a.Values$a_err)
      aVal_err <- round(aVal_err)
    }
    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_aValue",
                       label = "a",
                       value = aVal)),
      column(width = 6,
             textInput(inputId = "m1_aValue_err",
                       label = "\u03B4a",
                       value = aVal_err))
    )
  })

  output$m1_densityText <- renderUI({
    if(input$material %in% c("flint", "cave flint")){
      h5("density")
    }
  })

  output$m1_density <- renderUI({

    if(input$material %in% c("flint", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_density", 
                         label = "\u03C1",
                         value = 2.65)),
        column(width = 6,
               textInput(inputId = "m1_density_err",
                         label = "\u03B4\u03C1",
                         value = 0.2))
      )
    }
  })

  output$m1_waterText <- renderUI({
    if(input$material %in% c("")){
      h5("Water content m= (W-D)/D [%]")
    }
  })

  output$m1_water <- renderUI({
    if(input$material %in% c("")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_water",
                         label = "m",
                         value = 0)),
        column(width = 6,
               textInput(inputId = "m1_water_err",
                         label = "\u03B4m",
                         value = 0))
      )

    }
  })



  # m2
  output$m2_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m2_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  # concentration
  output$m2_text <- renderUI({

    if(input$material %in% c("ceramic")){
      h4("Ceramic information")

    }else if(input$material %in% c("flint",  "sediment", "cave sediment", "cave flint") ){

      h4("Sediment information")
    }
  })

  output$m2_U <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m2_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m2_Th <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m2_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m2_K <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m2_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m2_K2RbBox <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      checkboxInput(inputId = "m2_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m2_Rb <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      if(!is.null(input$m2_K2RbBox) && !input$m2_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m2_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m2_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m2_alpha <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_alpha_err",
                         label = "\u03B4\u03B1"))
      )
    }
  })

  output$m2_beta <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m2_gamma <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other
  output$m2_densityText <- renderUI({
    h5(tags$b("Density [mg/mm3]"))
  })

  output$m2_density <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m2_density",
                       label = "\u03C1",
                       value = 1.8)),
      column(width = 6,
             textInput(inputId = "m2_density_err",
                       label = "\u03B4\u03C1",
                       value = 0.1))
    )
  })

  output$m2_waterText <- renderUI({
    h5(tags$b("Water content m = (W-D)/D [%]"))
  })

  output$m2_water <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m2_water",
                       label = "m",
                       placeholder = "required",
                       value = 5)),
      column(width = 6,
             textInput(inputId = "m2_water_err",
                       label = "\u03B4m",
                       placeholder = "required",
                       value = 2))
    )
  })

  output$m2_proportion <- renderUI({

    P <- input$m3_proportion
    P_err <- input$m3_proportion_err

    if(is.null(P)){
      P <- 0
    }

    if(is.null(P_err)){
      P_err <- 0
    }

    if(input$material %in% c("cave sediment", "cave flint")){

      fluidRow(
        column(width = 6,
               p(strong("p [%]"),
                 br(),
                 p(100-P))
        ),
        column(width = 6,
               p(strong("\u03B4p"),
                 br(),
                 div(P_err))
        )
      )
    }
  })

  # m3
  # concentration
  output$m3_text <- renderUI({

    if(input$material %in% c("ceramic")){
      h4("Sediment information")

    }else if(input$material %in% c("cave sediment", "cave flint")){
      h4("Rock information")

    }
  })

  output$m3_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m3_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  output$m3_U <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m3_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m3_Th <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m3_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m3_K <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m3_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m3_K2RbBox <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      checkboxInput(inputId = "m3_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m3_Rb <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      if(!is.null(input$m3_K2RbBox) && !input$m3_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m3_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m3_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m3_alpha <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_alpha_err",
                         label = "\u03B4\u03B1"))
      )
    }
  })

  output$m3_beta <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m3_gamma <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other

  output$m3_densityText <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Density [mg/mm3]"))
    }
  })

  output$m3_density <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_density",
                         label = "\u03C1",
                         value = 1.8)),
        column(width = 6,
               textInput(inputId = "m3_density_err",
                         label = "\u03B4\u03C1",
                         value = 0.1))
      )
    }
  })

  output$m3_waterText <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Water content m = (W-D)/D [%]"))
    }
  })

  output$m3_water <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_water",
                         label = "m",
                         placeholder = "required",
                         value = 5)),
        column(width = 6,
               textInput(inputId = "m3_water_err",
                         label = "\u03B4m",
                         placeholder = "required",
                         value = 2))
      )
    }
  })

  output$m3_proportion <- renderUI({

    P <- input$m2_proportion
    P_err <- input$m2_proportion_err

    if(is.null(P)){
      P <- 100
    }
    if(is.null(P_err)){
      P_err <- 0
    }

    if(input$material %in% c("cave sediment", "cave flint")){
      shinyjs::disable("m2_proportion")
      shinyjs::disable("m2_proportion_err")

      fluidRow(
        shinyjs::useShinyjs(),
        column(width = 6,
               numericInput(inputId = "m3_proportion",
                            label = "p [%]",
                            value = 100-P,
                            min = 0,
                            max = 100,
                            step = 5)),
        column(width = 6,
               textInput(inputId = "m3_proportion_err",
                         label = "\u03B4p",
                         value = P_err))
      )
    }
  })

  #Dc
  output$DcLoc <- renderUI({

    #if(input$geoDcBox){
    if(input$DcRadioButton == "geographical position"){
      fluidRow(
        column(width = 12,
               fluidRow(column(width = 6,
                               textInput(inputId = "latitude",
                                         label = "Latitude")),
                        column(width = 6,
                               textInput(inputId = "longitude",
                                         label = "Longitude"))),
               
               textInput(inputId = "altitude",
                         label = "Altitude [m]"),

               checkboxInput(inputId = "fieldChangeBox",
                             label = " field change correction",
                             value = TRUE),

               checkboxInput(inputId = "shallowDepthBox",
                             label = "Scale for shallow depth",
                             value = TRUE)
        )
      )
    }
  })

  output$directDc <- renderUI({

    #if(input$directDcBox){
    if(input$DcRadioButton == "in-situ measurement"){

      fluidRow(
        column(width = 6,
               textInput(inputId = "dc",
                         label = "Dc [Gy]")),
        column(width = 6,
               textInput(inputId = "dc_err",
                         label = "\u03B4Dc"))
      )
    }
  })


  # Age estimation
  dr_DATA.Dr <- reactive({

    temp <- input$dracButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- dr_generate.Dr()
    }

    return(data)
  })

  dr_generate.Dr <- eventReactive(input$dracButton,{

    De.values <- de_DATA.De()

    if(is.null(De.values)){
      de <- 'X'
      de_err <- 'X'
    }else{
      de <- as.numeric(De.values$De)
      de_err <- as.numeric(De.values$De_err)
    }


    material <- input$material

    if(input$DcRadioButton == "in-situ measurement"){
      directDc <- TRUE
    }else{
      directDc <- FALSE
    }

    project <- input$sa_project
    if(project == ""){
      project <- "unknown"
    }else{
      project <- gsub(" ", "", project, fixed = TRUE)
    }
    sample <- input$sa_sample
    if(sample==""){
      sample <- "unknown"
    }else{
      sample <- gsub(" ", "", sample, fixed = TRUE)
      
    }

    date <- input$sa_date
    date <- as.numeric(format(date,"%Y"))

    mineral <- input$mineral

    conversionFactor <- input$conversionFactor
    alphaSizeFactor <- input$alphaSizeFactor
    betaSizeFactor <- input$betaSizeFactor
    betaEtchFactor <- input$betaEtchFactor

    m1_U <- input$m1_U
    m1_U <- gsub(",",".", m1_U, fixed = TRUE)
    m1_U <- as.numeric(m1_U)
    if(length(m1_U)==0 || !is.finite(m1_U) ){
      m1_U <- "X"
    }
    m1_U_err <- input$m1_U_err
    m1_U_err <- gsub(",",".", m1_U_err, fixed = TRUE)
    m1_U_err <- as.numeric(m1_U_err)
    if(length(m1_U_err)==0 ||!is.finite(m1_U_err)){
      m1_U_err <- "X"
    }
    m1_Th <- input$m1_Th
    m1_Th <- gsub(",",".", m1_Th, fixed = TRUE)
    m1_Th <- as.numeric(m1_Th)
    if(length(m1_Th)==0 ||!is.finite(m1_Th)){
      m1_Th <- "X"
    }
    m1_Th_err <- input$m1_Th_err
    m1_Th_err <- gsub(",",".", m1_Th_err, fixed = TRUE)
    m1_Th_err <- as.numeric(m1_Th_err)
    if(length(m1_Th_err)==0 ||!is.finite(m1_Th_err)){
      m1_Th_err <- "X"
    }
    m1_K <- input$m1_K
    m1_K <- gsub(",",".", m1_K, fixed = TRUE)
    m1_K <- as.numeric(m1_K)
    if(length(m1_K)==0 ||!is.finite(m1_K)){
      m1_K <- "X"
    }
    m1_K_err <- input$m1_K_err
    m1_K_err <- gsub(",",".", m1_K_err, fixed = TRUE)
    m1_K_err <- as.numeric(m1_K_err)
    if(length(m1_K_err)==0 ||!is.finite(m1_K_err)){
     m1_K_err <- "X"
    }
    m1_Rb <- input$m1_Rb
    m1_Rb <- gsub(",",".", m1_Rb, fixed = TRUE)
    m1_Rb <- as.numeric(m1_Rb)
    if(length(m1_Rb)==0 ||!is.finite(m1_Rb)){
      m1_Rb <- "X"
    }
    m1_Rb_err <- input$m1_Rb_err
    m1_Rb_err <- gsub(",",".", m1_Rb_err, fixed = TRUE)
    m1_Rb_err <- as.numeric(m1_Rb_err)
    if(length(m1_Rb_err)==0 ||!is.finite(m1_Rb_err)){
      m1_Rb_err <- "X"
    }

    if(is.logical(input$m1_K2RbBox) && input$m1_K2RbBox){
      m1_K2Rb <- TRUE
    }else{
      m1_K2Rb <-  FALSE
    }

    m1_alpha <- input$m1_alpha
    m1_alpha <- gsub(",",".", m1_alpha, fixed = TRUE)
    m1_alpha <- as.numeric(m1_alpha)
    if(length(m1_alpha)==0 ||!is.finite(m1_alpha)){
      m1_alpha <- "X"
    }
    m1_alpha_err <- input$m1_alpha_err
    m1_alpha_err <- gsub(",",".", m1_alpha_err, fixed = TRUE)
    m1_alpha_err <- as.numeric(m1_alpha_err)
    if(length(m1_alpha_err)==0 ||!is.finite(m1_alpha_err)){
      m1_alpha_err <- "X"
    }
    m1_beta <- input$m1_beta
    m1_beta <- gsub(",",".", m1_beta, fixed = TRUE)
    m1_beta <- as.numeric(m1_beta)
    if(length(m1_beta)==0 ||!is.finite(m1_beta)){
      m1_beta <- "X"
    }
    m1_beta_err <- input$m1_beta_err
    m1_beta_err <- gsub(",",".", m1_beta_err, fixed = TRUE)
    m1_beta_err <- as.numeric(m1_beta_err)
    if(length(m1_beta_err)==0 ||!is.finite(m1_beta_err)){
      m1_beta_err <- "X"
    }
    m1_gamma <- input$m1_gamma
    m1_gamma <- gsub(",",".", m1_gamma, fixed = TRUE)
    m1_gamma <- as.numeric(m1_gamma)
    if(length(m1_gamma)==0 ||!is.finite(m1_gamma)){
      m1_gamma <- "X"
    }
    m1_gamma_err <- input$m1_gamma_err
    m1_gamma_err <- gsub(",",".", m1_gamma_err, fixed = TRUE)
    m1_gamma_err <- as.numeric(m1_gamma_err)
    if(length(m1_gamma_err)==0 ||!is.finite(m1_gamma_err)){
     m1_gamma_err <- "X"
    }
    
    m2_U <- input$m2_U
    m2_U <- gsub(",",".", m2_U, fixed = TRUE)
    m2_U <- as.numeric(m2_U)
    if(length(m2_U)==0 ||!is.finite(m2_U)){
     m2_U <- "X"
    }
    m2_U_err <- input$m2_U_err
    m2_U_err <- gsub(",",".", m2_U_err, fixed = TRUE)
    m2_U_err <- as.numeric(m2_U_err)
    if(length(m2_U_err)==0 ||!is.finite(m2_U_err)){
     m2_U_err <- "X"
    }
    m2_Th <- input$m2_Th
    m2_Th <- gsub(",",".", m2_Th, fixed = TRUE)
    m2_Th <- as.numeric(m2_Th)
    if(length(m2_Th)==0 ||!is.finite(m2_Th)){
     m2_Th <- "X"
    }
    m2_Th_err <- input$m2_Th_err
    m2_Th_err <- gsub(",",".", m2_Th_err, fixed = TRUE)
    m2_Th_err <- as.numeric(m2_Th_err)
    if(length(m2_Th_err)==0 ||!is.finite(m2_Th_err)){
     m2_Th_err <- "X"
    }
    m2_K <- input$m2_K
    m2_K <- gsub(",",".", m2_K, fixed = TRUE)
    m2_K <- as.numeric(m2_K)
    if(length(m2_K)==0 ||!is.finite(m2_K)){
     m2_K <- "X"
    }
    m2_K_err <- input$m2_K_err
    m2_K_err <- gsub(",",".", m2_K_err, fixed = TRUE)
    m2_K_err <- as.numeric(m2_K_err)
    if(length(m2_K_err)==0 ||!is.finite(m2_K_err)){
     m2_K_err <- "X"
    }
    m2_Rb <- input$m2_Rb
    m2_Rb <- gsub(",",".", m2_Rb, fixed = TRUE)
    m2_Rb <- as.numeric(m2_Rb)
    if(length(m2_Rb)==0 ||!is.finite(m2_Rb)){
     m2_Rb <- "X"
    }
    m2_Rb_err <- input$m2_Rb_err
    m2_Rb_err <- gsub(",",".", m2_Rb_err, fixed = TRUE)
    m2_Rb_err <- as.numeric(m2_Rb_err)
    if(length(m2_Rb_err)==0 ||!is.finite(m2_Rb_err)){
     m2_Rb_err <- "X"
    }

    if(is.logical(input$m2_K2RbBox) && input$m2_K2RbBox){
     m2_K2Rb <- TRUE
    }else{
     m2_K2Rb <- FALSE
    }

    m2_alpha <- input$m2_alpha
    m2_alpha <- gsub(",",".", m2_alpha, fixed = TRUE)
    m2_alpha <- as.numeric(m2_alpha)
    if(length(m2_alpha)==0 ||!is.finite(m2_alpha)){
     m2_alpha <- "X"
    }
    m2_alpha_err <- input$m2_alpha_err
    m2_alpha_err <- gsub(",",".", m2_alpha_err, fixed = TRUE)
    m2_alpha_err <- as.numeric(m2_alpha_err)
    if(length(m2_alpha_err)==0 ||!is.finite(m2_alpha_err)){
     m2_alpha_err <- "X"
    }
    m2_beta <- input$m2_beta
    m2_beta <- gsub(",",".", m2_beta, fixed = TRUE)
    m2_beta <- as.numeric(m2_beta)
    if(length(m2_beta)==0 ||!is.finite(m2_beta)){
     m2_beta <- "X"
    }
    m2_beta_err <- input$m2_beta_err
    m2_beta_err <- gsub(",",".", m2_beta_err, fixed = TRUE)
    m2_beta_err <- as.numeric(m2_beta_err)
    if(length(m2_beta_err)==0 ||!is.finite(m2_beta_err)){
     m2_beta_err <- "X"
    }
    m2_gamma <- input$m2_gamma
    m2_gamma <- gsub(",",".", m2_gamma, fixed = TRUE)
    m2_gamma <- as.numeric(m2_gamma)
    if(length(m2_gamma)==0 ||!is.finite(m2_gamma)){
     m2_gamma <- "X"
    }
    m2_gamma_err <- input$m2_gamma_err
    m2_gamma_err <- gsub(",",".", m2_gamma_err, fixed = TRUE)
    m2_gamma_err <- as.numeric(m2_gamma_err)
    if(length(m2_gamma_err)==0 ||!is.finite(m2_gamma_err)){
     m2_gamma_err <- "X"
    }
    
    m3_U <- input$m3_U
    m3_U <- gsub(",",".", m3_U, fixed = TRUE)
    m3_U <- as.numeric(m3_U)
    if(length(m3_U)==0 ||!is.finite(m3_U)){
     m3_U <- "X"
    }
    m3_U_err <- input$m3_U_err
    m3_U_err <- gsub(",",".", m3_U_err, fixed = TRUE)
    m3_U_err <- as.numeric(m3_U_err)
    if(length(m3_U_err)==0 ||!is.finite(m3_U_err)){
     m3_U_err <- "X"
    }
    m3_Th <- input$m3_Th
    m3_Th <- gsub(",",".", m3_Th, fixed = TRUE)
    m3_Th <- as.numeric(m3_Th)
    if(length(m3_Th)==0 ||!is.finite(m3_Th)){
     m3_Th <- "X"
    }
    m3_Th_err <- input$m3_Th_err
    m3_Th_err <- gsub(",",".", m3_Th_err, fixed = TRUE)
    m3_Th_err <- as.numeric(m3_Th_err)
    if(length(m3_Th_err)==0 ||!is.finite(m3_Th_err)){
     m3_Th_err <- "X"
    }
    m3_K <- input$m3_K
    m3_K <- gsub(",",".", m3_K, fixed = TRUE)
    m3_K <- as.numeric(m3_K)
    if(length(m3_K)==0 ||!is.finite(m3_K)){
     m3_K <- "X"
    }
    m3_K_err <- input$m3_K_err
    m3_K_err <- gsub(",",".", m3_K_err, fixed = TRUE)
    m3_K_err <- as.numeric(m3_K_err)
    if(length(m3_K_err)==0 ||!is.finite(m3_K_err)){
     m3_K_err <- "X"
    }
    m3_Rb <- input$m3_Rb
    m3_Rb <- gsub(",",".", m3_Rb, fixed = TRUE)
    m3_Rb <- as.numeric(m3_Rb)
    if(length(m3_Rb)==0 ||!is.finite(m3_Rb)){
     m3_Rb <- "X"
    }
    m3_Rb_err <- input$m3_Rb_err
    m3_Rb_err <- gsub(",",".", m3_Rb_err, fixed = TRUE)
    m3_Rb_err <- as.numeric(m3_Rb_err)
    if(length(m3_Rb_err)==0 ||!is.finite(m3_Rb_err)){
     m3_Rb_err <- "X"
    }

    if(is.logical(input$m3_K2RbBox) && input$m3_K2RbBox){
     m3_K2Rb <- TRUE
    }else{
     m3_K2Rb <- FALSE
    }

    m3_alpha <- input$m3_alpha
    m3_alpha <- gsub(",",".", m3_alpha, fixed = TRUE)
    m3_alpha <- as.numeric(m3_alpha)
    if(length(m3_alpha)==0 ||!is.finite(m3_alpha)){
     m3_alpha <- "X"
    }
    m3_alpha_err <- input$m3_alpha_err
    m3_alpha_err <- gsub(",",".", m3_alpha_err, fixed = TRUE)
    m3_alpha_err <- as.numeric(m3_alpha_err)
    if(length(m3_alpha_err)==0 ||!is.finite(m3_alpha_err)){
     m3_alpha_err <- "X"
    }
    m3_beta <- input$m3_beta
    m3_beta <- gsub(",",".", m3_beta, fixed = TRUE)
    m3_beta <- as.numeric(m3_beta)
    if(length(m3_beta)==0 ||!is.finite(m3_beta)){
     m3_beta <- "X"
    }
    m3_beta_err <- input$m3_beta_err
    m3_beta_err <- gsub(",",".", m3_beta_err, fixed = TRUE)
    m3_beta_err <- as.numeric(m3_beta_err)
    if(length(m3_beta_err)==0 ||!is.finite(m3_beta_err)){
     m3_beta_err <- "X"
    }
    m3_gamma <- input$m3_gamma
    m3_gamma <- gsub(",",".", m3_gamma, fixed = TRUE)
    m3_gamma <- as.numeric(m3_gamma)
    if(length(m3_gamma)==0 ||!is.finite(m3_gamma)){
     m3_gamma <- "X"
    }
    m3_gamma_err <- input$m3_gamma_err
    m3_gamma_err <- gsub(",",".", m3_gamma_err, fixed = TRUE)
    m3_gamma_err <- as.numeric(m3_gamma_err)
    if(length(m3_gamma_err)==0 ||!is.finite(m3_gamma_err)){
     m3_gamma_err <- "X"
    }

    if(is.logical(input$shallowDepthBox) && input$shallowDepthBox){
     shallowDepth <- TRUE

    }else{
     shallowDepth <- FALSE
    }

    m1_size_min <- input$m1_size_min
    m1_size_min <- gsub(",",".", m1_size_min, fixed = TRUE)
    m1_size_min <- as.numeric(m1_size_min)
    if(length(m1_size_min)==0 || !is.finite(m1_size_min)){
      m1_size_min <- 1
    }
    
    m1_size_max <- input$m1_size_max
    m1_size_max <- gsub(",",".", m1_size_max, fixed = TRUE)
    m1_size_max <- as.numeric(m1_size_max)
    if(length(m1_size_max)==0 || !is.finite(m1_size_min) ){
     m1_size_max <- 1000
    }

    m1_etch_min <- input$m1_etch_min
    m1_etch_min <- gsub(",",".", m1_etch_min, fixed = TRUE)
    m1_etch_min <- as.numeric(m1_etch_min)
    if(length(m1_etch_min)==0 || !is.finite(m1_etch_min)){
      m1_etch_min <- 0
    }
    m1_etch_max <- input$m1_etch_max
    m1_etch_max <- gsub(",",".", m1_etch_max, fixed = TRUE)
    m1_etch_max <- as.numeric(m1_etch_max)
    if(length(m1_etch_max)==0 || !is.finite(m1_etch_max) ){
     m1_etch_max <- 30
    }

    m1_aValue <- input$m1_aValue
    m1_aValue <- gsub(",",".", m1_aValue, fixed = TRUE)
    m1_aValue <- as.numeric(m1_aValue)
    if(length(m1_aValue)==0 || !is.finite(m1_aValue)){
     m1_aValue <- "X"
    }
    m1_aValue_err <- input$m1_aValue_err
    m1_aValue_err <- gsub(",",".", m1_aValue_err, fixed = TRUE)
    m1_aValue_err <- as.numeric(m1_aValue_err)
    if(length(m1_aValue_err)==0 || !is.finite(m1_aValue_err) ){
     m1_aValue_err <- "X"
    }

    m1_water <- input$m1_water
    m1_water <- gsub(",",".", m1_water, fixed = TRUE)
    m1_water <- as.numeric(m1_water)
    if(length(m1_water)==0 || !is.finite(m1_water)){
     m1_water <- 0
    }
    m1_water_err <- input$m1_water_err
    m1_water_err <- gsub(",",".", m1_water_err, fixed = TRUE)
    m1_water_err <- as.numeric(m1_water_err)
    if(length(m1_water_err)==0 || !is.finite(m1_water_err)){
     m1_water_err <- 0
    }

    m1_density <- input$m1_density
    m1_density <- gsub(",",".", m1_density, fixed = TRUE)
    m1_density <- as.numeric(m1_density)
    m1_density <- as.numeric(input$m1_density)
    if(length(m1_density)==0  || !is.finite(m1_density)){
      m1_density <- "X"
    }
    m1_density_err <- input$m1_density_err
    m1_density_err <- gsub(",",".", m1_density_err, fixed = TRUE)
    m1_density_err <- as.numeric(m1_density_err)
    if(length(m1_density_err)==0 || !is.finite(m1_density_err)){
     m1_density_err <- "X"
    }

    m2_water <- input$m2_water
    m2_water <- gsub(",",".", m2_water, fixed = TRUE)
    m2_water <- as.numeric(m2_water)
    if(length(m2_water)==0 || !is.finite(m2_water)){
     m2_water <- 0
    }
    m2_water_err <- input$m2_water_err
    m2_water_err <- gsub(",",".", m2_water_err, fixed = TRUE)
    m2_water_err <- as.numeric(m2_water_err)
    if(length(m2_water_err)==0 || !is.finite(m2_water_err)){
     m2_water_err <- 0
    }

    m2_density <- input$m2_density
    m2_density <- gsub(",",".", m2_density, fixed = TRUE)
    m2_density <- as.numeric(m2_density)
    if(length(m2_density)==0 || !is.finite(m2_density)){
      m2_density <- "X"
    }
    m2_density_err <- input$m2_density_err
    m2_density_err <- gsub(",",".", m2_density_err, fixed = TRUE)
    m2_density_err <- as.numeric(m2_density_err)
    if(length(m2_density_err)==0 || !is.finite(m2_density_err)){
     m2_density_err <- "X"
    }

    m3_water <- input$m3_water
    m3_water <- gsub(",",".", m3_water, fixed = TRUE)
    m3_water <- as.numeric(m3_water)
    if(length(m3_water)==0 || !is.finite(m3_water)){
     m3_water <- 0
    }
    m3_water_err <- input$m3_water_err
    m3_water_err <- gsub(",",".", m3_water_err, fixed = TRUE)
    m3_water_err <- as.numeric(m3_water_err)
    if(length(m3_water_err)==0 || !is.finite(m3_water_err)){
     m3_water_err <- 0
    }

    m3_density <- input$m3_density
    m3_density <- gsub(",",".", m3_density, fixed = TRUE)
    m3_density <- as.numeric(m3_density)
    if(length(m3_density)==0 || !is.finite(m3_density) ){
      m3_density <- "X"
    }
    m3_density_err <- input$m3_density_err
    m3_density_err <- gsub(",",".", m3_density_err, fixed = TRUE)
    m3_density_err <- as.numeric(m3_density_err)
    if(length(m3_density_err)==0 || !is.finite(m3_density_err) ){
     m3_density_err <- "X"
    }

    m2_proportion <- input$m2_proportion
    m2_proportion <- gsub(",",".", m2_proportion, fixed = TRUE)
    m2_proportion <- as.numeric(m2_proportion)
    m2_proportion <- m2_proportion/100
    m2_proportion_err <- input$m2_proportion_err
    m2_proportion_err <- gsub(",",".", m2_proportion_err, fixed = TRUE)
    m2_proportion_err <- as.numeric(m2_proportion_err)
    m2_proportion_err <- m2_proportion_err/100

    m3_proportion <- input$m3_proportion
    m3_proportion <- gsub(",",".", m3_proportion, fixed = TRUE)
    m3_proportion <- as.numeric(m3_proportion)
    m3_proportion <- m3_proportion/100
    m3_proportion_err <- input$m3_proportion_err
    m3_proportion_err <- gsub(",",".", m3_proportion_err, fixed = TRUE)
    m3_proportion_err <- as.numeric(m3_proportion_err)
    m3_proportion_err <- m3_proportion_err/100

    depth <- input$depth
    depth <- gsub(",",".", depth, fixed = TRUE)
    depth <- as.numeric(depth)
    if(length(depth)==0|| !is.finite(depth)){
      depth <- "X"
    }
    depth_err <- input$depth_err
    depth_err <- gsub(",",".", depth_err, fixed = TRUE)
    depth_err <- as.numeric(depth_err)
    if(length(depth_err)==0 || !is.finite(depth_err)){
     depth_err <- "X"
    }

    if(is.logical(input$fieldChangeBox) && input$fieldChangeBox){
     fieldChange <- TRUE
    }else{
     fieldChange <- FALSE
    }


    latitude <- input$latitude
    latitude <- gsub(",",".", latitude, fixed = TRUE)
    latitude <- as.numeric(latitude)
    longitude <- input$longitude
    longitude <- gsub(",",".", longitude, fixed = TRUE)
    longitude <- as.numeric(longitude)
    if(length(latitude)==0 || length(longitude)==0 || !is.finite(latitude) || !is.finite(longitude) || directDc){
     latitude <- "X"
     longitude <- "X"
    }
    altitude <- input$altitude
    altitude <- gsub(",",".", altitude, fixed = TRUE)
    altitude <- as.numeric(altitude)
    if(length(altitude)==0 || !is.finite(altitude) || directDc){
     altitude <- "X"
    }

    dc <- input$dc
    dc <- gsub(",",".", dc, fixed = TRUE)
    dc <- as.numeric(dc)
    if(length(dc)==0 || !is.finite(dc) || !directDc){
      dc <- "X"
    }
    dc_err <- input$dc_err
    dc_err <- gsub(",",".", dc_err, fixed = TRUE)
    dc_err <- as.numeric(dc_err)
    if(length(dc_err)==0 || !is.finite(dc_err) || !directDc){
     dc_err <- "X"
    }

    if(material == "sediment"){
     data <- template_DRAC(notification = FALSE)

     data$`Project ID` <- project
     data$`Sample ID` <- sample
     data$Mineral <- mineral
     data$`Conversion factors` <- conversionFactor

     data$`ExternalU (ppm)` <- m2_U
     data$`errExternal U (ppm)` <- m2_U_err
     data$`External Th (ppm)`  <- m2_Th
     data$`errExternal Th (ppm)` <- m2_Th_err
     data$`External K (%)` <- m2_K
     data$`errExternal K (%)` <- m2_Th_err
     data$`External Rb (ppm)` <- m2_Rb
     data$`errExternal Rb (ppm)` <- m2_Rb_err

     if(m2_K2Rb){
       m2_K2Rb <- "Y"
     }else{
       m2_K2Rb <- "N"
     }
     data$`Calculate external Rb from K conc?` <- m2_K2Rb

     data$`Internal U (ppm)` <- m1_U
     data$`errInternal U (ppm)` <- m1_U_err
     data$`Internal Th (ppm)` <- m1_Th
     data$`errInternal Th (ppm)` <- m1_Th_err
     data$`Internal K (%)` <- m1_K
     data$`errInternal K (%)` <- m1_K_err
     data$`Rb (ppm)` <- m1_Rb
     data$`errRb (ppm)` <- m1_Rb_err

     if(m1_K2Rb){
       m1_K2Rb <- "Y"
     }else{
       m1_K2Rb <- "N"
     }
     data$`Calculate internal Rb from K conc?` <- m1_K2Rb

     data$`User external alphadoserate (Gy.ka-1)` <- m2_alpha
     data$`errUser external alphadoserate (Gy.ka-1)` <- m2_alpha_err
     data$`User external betadoserate (Gy.ka-1)` <- m2_beta
     data$`errUser external betadoserate (Gy.ka-1)` <- m2_beta_err
     data$`User external gamma doserate (Gy.ka-1)` <- m2_gamma
     data$`errUser external gammadoserate (Gy.ka-1)` <- m2_gamma_err

     if(!("X" %in% c(m1_alpha, m1_alpha_err, m1_beta, m1_beta_err))){
       data$`User internal doserate (Gy.ka-1)` <- sum(m1_alpha,m1_beta,na.rm = TRUE)
       data$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(m1_alpha_err^2,m1_beta_err^2,na.rm = TRUE))

     }else if("X" %in% c(m1_alpha, m1_alpha_err)){
       data$`User internal doserate (Gy.ka-1)` <- m1_beta
       data$`errUser internal doserate (Gy.ka-1)` <- m1_beta_err

     }else if("X" %in% c(m1_beta, m1_beta_err)){
       data$`User internal doserate (Gy.ka-1)` <- m1_alpha
       data$`errUser internal doserate (Gy.ka-1)` <- m1_alpha_err

     }else{
       data$`User internal doserate (Gy.ka-1)` <- "X"
       data$`errUser internal doserate (Gy.ka-1)` <- "X"
     }


     if(shallowDepth){
       shallowDepth <- "Y"
     }else{
       shallowDepth <- "N"
     }
     data$`Scale gammadoserate at shallow depths?` <- shallowDepth

     data$`Grain size min (microns)` <- m1_size_min
     data$`Grain size max (microns)`  <- m1_size_max

     data$`alpha-Grain size attenuation` <- alphaSizeFactor
     data$`beta-Grain size attenuation ` <- betaSizeFactor

     data$`Etch depth min (microns)` <- m1_etch_min
     data$`Etch depth max (microns)` <- m1_etch_max

     data$`beta-Etch depth attenuation factor` <- betaEtchFactor

     data$`a-value` <- m1_aValue
     data$`erra-value` <- m1_aValue_err

     data$`Water content ((wet weight - dry weight)/dry weight) %` <- m2_water
     data$`errWater content %` <- m2_water_err

     data$`Depth (m)` <- depth
     data$`errDepth (m)` <- depth_err

     data$`Overburden density (g cm-3)` <- m2_density
     data$`errOverburden density (g cm-3)`<- m2_density_err

     data$`Latitude (decimal degrees)` <- latitude
     data$`Longitude (decimal degrees)` <- longitude
     data$`Altitude (m)` <- altitude

     data$`User cosmicdoserate (Gy.ka-1)` <- dc
     data$`errUser cosmicdoserate (Gy.ka-1)` <- dc_err

     data$`De (Gy)` <- de
     data$`errDe (Gy)`<- de_err

     # Use_DRAC
     res <- try(use_DRAC(file = data,
                     name= "shinyDRAC",
                     verbose=FALSE),
                silent = TRUE)

     if(class(res) == "try-error"){
       result <- NULL

     }else{
       DRAC.age <- as.numeric(res$DRAC$highlights$`Age (ka)`[1])
       DRAC.age.err <- as.numeric(res$DRAC$highlights$`errAge (ka)`[1])

       int.alpha <- as.numeric(res$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
       int.alpha.err <- as.numeric(res$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

       int.beta <- as.numeric(res$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
       int.beta.err <- as.numeric(res$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

       ext.alpha <- as.numeric(res$DRAC$highlights$`Water corrected alphadoserate`[1])
       ext.alpha.err <- as.numeric(res$DRAC$highlights$`Water corrected erralphadoserate`[1])

       ext.beta <- as.numeric(res$DRAC$highlights$`Water corrected betadoserate`[1])
       ext.beta.err <- as.numeric(res$DRAC$highlights$`Water corrected errbetadoserate`[1])

       ext.gamma <- as.numeric(res$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
       ext.gamma.err <- as.numeric(res$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

       cosmic <- as.numeric(res$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
       cosmic.err <- as.numeric(res$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])


       DRAC.int.Dr <- int.alpha+int.beta
       DRAC.int.Dr.err <- sqrt(sum(int.alpha.err^2, int.beta^2))

       DRAC.ext.Dr <- ext.alpha+ext.beta+ext.gamma
       DRAC.ext.Dr.err <- sqrt(sum(ext.alpha.err^2, ext.beta.err^2, ext.gamma.err^2) )

       DRAC.env.Dr <- 0
       DRAC.env.Dr.err <- 0

       DRAC.alpha.Dr <- int.alpha+ext.alpha
       DRAC.alpha.Dr.err <- sqrt(sum(int.alpha.err^2, ext.alpha.err^2))

       DRAC.beta.Dr <- int.beta+ext.beta
       DRAC.beta.Dr.err <- sqrt(sum(int.beta.err^2, ext.beta.err^2))

       DRAC.gamma.Dr <- ext.gamma
       DRAC.gamma.Dr.err <- ext.gamma.err

       DRAC.cosmic.Dr <- cosmic
       DRAC.cosmic.Dr.err <- cosmic.err

       DRAC.Dr <- sum(DRAC.alpha.Dr,
                      DRAC.beta.Dr,
                      DRAC.gamma.Dr,
                      DRAC.cosmic.Dr)


       DRAC.Dr.err <- sqrt(sum(DRAC.alpha.Dr.err^2,
                               DRAC.beta.Dr.err^2,
                               DRAC.gamma.Dr.err^2,
                               DRAC.cosmic.Dr.err^2))

       DRAC.result <- list(Age = DRAC.age,
                           Age.err =DRAC.age.err,
                           De=de,
                           De.err = de_err,
                           Dr = DRAC.Dr,
                           Dr.err = DRAC.Dr.err,
                           int.Dr = DRAC.int.Dr,
                           int.Dr.err = DRAC.int.Dr.err,
                           ext.Dr = DRAC.ext.Dr,
                           ext.Dr.err = DRAC.ext.Dr.err,
                           env.Dr = DRAC.env.Dr,
                           env.Dr.err = DRAC.env.Dr.err,
                           alpha.Dr = DRAC.alpha.Dr,
                           alpha.Dr.err = DRAC.alpha.Dr.err,
                           beta.Dr = DRAC.beta.Dr,
                           beta.Dr.err = DRAC.beta.Dr.err,
                           gamma.Dr = DRAC.gamma.Dr,
                           gamma.Dr.err = DRAC.gamma.Dr.err,
                           cosmic.Dr = DRAC.cosmic.Dr,
                           cosmic.Dr.err = DRAC.cosmic.Dr.err)

       comment <- ""

       temp.result <- list(age=DRAC.age,
                           age.err=DRAC.age.err,
                           Dr=DRAC.Dr,
                           Dr.err=DRAC.Dr.err,
                           DRAC = DRAC.result,
                           R = DRAC.result,
                           comment=comment)

       result <- set_TLum.Results(data = temp.result)
     }

    }else if(material == "flint"){

     data <- template_DRAC4flint()

     data$info$project <- project
     data$info$sample <- sample
     data$info$date <- date
     data$info$mineral <- mineral
     data$info$conversion.factors <- conversionFactor
     data$info$alpha.size.attenuation <- alphaSizeFactor
     data$info$beta.size.attenuation <- betaSizeFactor
     data$info$beta.etch.attenuation <- betaEtchFactor

     data$De$De <- de
     data$De$De.err <- de_err

     data$flint$Dr$U <- m1_U
     data$flint$Dr$U.err <- m1_U_err
     data$flint$Dr$Th <- m1_Th
     data$flint$Dr$Th.err <- m1_Th_err
     data$flint$Dr$K <- m1_K
     data$flint$Dr$K.err <- m1_K_err
     data$flint$Dr$Rb <- m1_Rb
     data$flint$Dr$Rb.err <- m1_Rb_err
     data$flint$Dr$K2Rb <- m1_K2Rb

     data$flint$Dr$alpha <- m1_alpha
     data$flint$Dr$alpha.err <- m1_alpha_err
     data$flint$Dr$beta <- m1_beta
     data$flint$Dr$beta.err <- m1_beta_err
     data$flint$Dr$gamma <- m1_gamma
     data$flint$Dr$gamma.err <- m1_gamma_err

     data$flint$info$grain.size.min <- m1_size_min
     data$flint$info$grain.size.max <- m1_size_max

     data$flint$info$grain.etch.min <- m1_etch_min
     data$flint$info$grain.etch.max <- m1_etch_max

     data$flint$info$a.value <- m1_aValue
     data$flint$info$a.value.err <- m1_aValue_err
     data$flint$info$water.content <- m1_water
     data$flint$info$water.content.err <- m1_water_err
     data$flint$info$density <- m1_density
     data$flint$info$density.err <- m1_density_err

     data$sediment$Dr$U <- m2_U
     data$sediment$Dr$U.err <- m2_U_err
     data$sediment$Dr$Th <- m2_Th
     data$sediment$Dr$Th.err <- m2_Th_err
     data$sediment$Dr$K <- m2_K
     data$sediment$Dr$K.err <- m2_K_err
     data$sediment$Dr$Rb <- m2_Rb
     data$sediment$Dr$Rb.err <- m2_Rb_err
     data$sediment$Dr$K2Rb <- m2_K2Rb

     data$sediment$Dr$alpha <- m2_alpha
     data$sediment$Dr$alpha.err <- m2_alpha_err
     data$sediment$Dr$beta <- m2_beta
     data$sediment$Dr$beta.err <- m2_beta_err
     data$sediment$Dr$gamma <- m2_gamma
     data$sediment$Dr$gamma.err <- m2_gamma_err

     data$sediment$info$water.content <- m1_water
     data$sediment$info$water.content.err <- m1_water_err

     data$sediment$info$density <- m1_density
     data$sediment$info$density.err <- m1_density_err

     data$sediment$info$scale4shallow.depth <- shallowDepth

     data$cosmic$depth <- depth
     data$cosmic$depth.err <- depth_err

     data$cosmic$latitude <- latitude
     data$cosmic$longitude <- longitude
     data$cosmic$altitude <- altitude

     data$cosmic$Dr <- dc
     data$cosmic$Dr.err <- dc_err

     data$cosmic$corr.fieldChanges <- fieldChange

     res <- try(use_DRAC4flint(data))

     if(class(res) == "try-error"){
       result <- NULL

     }else{
       result <- res
     }

    }else if(material == "ceramic"){
      data <- template_DRAC4ceramic()

      data$info$project <- project
      data$info$sample <- sample
      data$info$date <- date
      data$info$mineral <- mineral
      data$info$conversion.factors <- conversionFactor
      data$info$alpha.size.attenuation <- alphaSizeFactor
      data$info$beta.size.attenuation <- betaSizeFactor
      data$info$beta.etch.attenuation <- betaEtchFactor

      data$De$De <- de
      data$De$De.err <- de_err

      data$grain$Dr$U <- m1_U
      data$grain$Dr$U.err <- m1_U_err
      data$grain$Dr$Th <- m1_Th
      data$grain$Dr$Th.err <- m1_Th_err
      data$grain$Dr$K <- m1_K
      data$grain$Dr$K.err <- m1_K_err
      data$grain$Dr$Rb <- m1_Rb
      data$grain$Dr$Rb.err <- m1_Rb_err
      data$grain$Dr$K2Rb <- m1_K2Rb

      data$grain$Dr$alpha <- m1_alpha
      data$grain$Dr$alpha.err <- m1_alpha_err
      data$grain$Dr$beta <- m1_beta
      data$grain$Dr$beta.err <- m1_beta_err
      data$grain$Dr$gamma <- m1_gamma
      data$grain$Dr$gamma.err <- m1_gamma_err

      data$grain$info$grain.size.min <- m1_size_min
      data$grain$info$grain.size.max <- m1_size_max

      data$grain$info$grain.etch.min <- m1_etch_min
      data$grain$info$grain.etch.max <- m1_etch_max

      data$grain$info$a.value <- m1_aValue
      data$grain$info$a.value.err <- m1_aValue_err

      data$ceramic$Dr$U <- m2_U
      data$ceramic$Dr$U.err <- m2_U_err
      data$ceramic$Dr$Th <- m2_Th
      data$ceramic$Dr$Th.err <- m2_Th_err
      data$ceramic$Dr$K <- m2_K
      data$ceramic$Dr$K.err <- m2_K_err
      data$ceramic$Dr$Rb <- m2_Rb
      data$ceramic$Dr$Rb.err <- m2_Rb_err
      data$ceramic$Dr$K2Rb <- m2_K2Rb

      data$ceramic$Dr$alpha <- m2_alpha
      data$ceramic$Dr$alpha.err <- m2_alpha_err
      data$ceramic$Dr$beta <- m2_beta
      data$sediment$Dr$beta.err <- m2_beta_err
      data$ceramic$Dr$gamma <- m2_gamma
      data$ceramic$Dr$gamma.err <- m2_gamma_err

      data$ceramic$info$water.content <- m2_water
      data$ceramic$info$water.content.err <- m2_water_err
      data$ceramic$info$density <- m2_density
      data$ceramic$info$density.err <- m2_density_err

      data$sediment$Dr$U <- m3_U
      data$sediment$Dr$U.err <- m3_U_err
      data$sediment$Dr$Th <- m3_Th
      data$sediment$Dr$Th.err <- m3_Th_err
      data$sediment$Dr$K <- m3_K
      data$sediment$Dr$K.err <- m3_K_err
      data$sediment$Dr$Rb <- m3_Rb
      data$sediment$Dr$Rb.err <- m3_Rb_err
      data$sediment$Dr$K2Rb <- m3_K2Rb

      data$sediment$Dr$alpha <- m3_alpha
      data$sediment$Dr$alpha.err <- m3_alpha_err
      data$sediment$Dr$beta <- m3_beta
      data$sediment$Dr$beta.err <- m3_beta_err
      data$sediment$Dr$gamma <- m3_gamma
      data$sediment$Dr$gamma.err <- m3_gamma_err

      data$sediment$info$water.content <- m3_water
      data$sediment$info$water.content.err <- m3_water_err
      data$sediment$info$density <- m3_density
      data$sediment$info$density.err <- m3_density_err
      data$sediment$info$scale4shallow.depth <- shallowDepth

      data$cosmic$depth <- depth
      data$cosmic$depth.err <- depth_err

      data$cosmic$latitude <- latitude
      data$cosmic$longitude <- longitude
      data$cosmic$altitude <- altitude

      data$cosmic$Dr <- dc
      data$cosmic$Dr.err <- dc_err

      data$cosmic$corr.fieldChanges <- fieldChange

      res <- try(use_DRAC4ceramic(data))

      if(class(res) == "try-error"){
        result <- NULL
      }else{
        result <- res
      }

    }else if(material == "cave sediment"){
      data <- template_DRAC4cave()

      data$info$project <- project
      data$info$sample <- sample
      data$info$date <- date
      data$info$mineral <- mineral
      data$info$conversion.factors <- conversionFactor
      data$info$alpha.size.attenuation <- alphaSizeFactor
      data$info$beta.size.attenuation <- betaSizeFactor
      data$info$beta.etch.attenuation <- betaEtchFactor

      data$De$De <- de
      data$De$De.err <- de_err

      data$grain$Dr$U <- m1_U
      data$grain$Dr$U.err <- m1_U_err
      data$grain$Dr$Th <- m1_Th
      data$grain$Dr$Th.err <- m1_Th_err
      data$grain$Dr$K <- m1_K
      data$grain$Dr$K.err <- m1_K_err
      data$grain$Dr$Rb <- m1_Rb
      data$grain$Dr$Rb.err <- m1_Rb_err
      data$grain$Dr$K2Rb <- m1_K2Rb

      data$grain$Dr$alpha <- m1_alpha
      data$grain$Dr$alpha.err <- m1_alpha_err
      data$grain$Dr$beta <- m1_beta
      data$grain$Dr$beta.err <- m1_beta_err
      data$grain$Dr$gamma <- m1_gamma
      data$grain$Dr$gamma.err <- m1_gamma_err

      data$grain$info$grain.size.min <- m1_size_min
      data$grain$info$grain.size.max <- m1_size_max

      data$grain$info$grain.etch.min <- m1_etch_min
      data$grain$info$grain.etch.max <- m1_etch_max

      data$grain$info$a.value <- m1_aValue
      data$grain$info$a.value.err <- m1_aValue_err

      data$sediment$Dr$U <- m2_U
      data$sediment$Dr$U.err <- m2_U_err
      data$sediment$Dr$Th <- m2_Th
      data$sediment$Dr$Th.err <- m2_Th_err
      data$sediment$Dr$K <- m2_K
      data$sediment$Dr$K.err <- m2_K_err
      data$sediment$Dr$Rb <- m2_Rb
      data$sediment$Dr$Rb.err <- m2_Rb_err
      data$sediment$Dr$K2Rb <- m2_K2Rb

      data$sediment$Dr$alpha <- m2_alpha
      data$sediment$Dr$alpha.err <- m2_alpha_err
      data$sediment$Dr$beta <- m2_beta
      data$sediment$Dr$beta.err <- m2_beta_err
      data$sediment$Dr$gamma <- m2_gamma
      data$sediment$Dr$gamma.err <- m2_gamma_err

      data$sediment$info$water.content <- m2_water
      data$sediment$info$water.content.err <- m2_water_err
      data$sediment$info$density <- m2_density
      data$sediment$info$density.err <- m2_density_err
      data$sediment$info$scale4shallow.depth <- shallowDepth


      data$rock$Dr$U <- m3_U
      data$rock$Dr$U.err <- m3_U_err
      data$rock$Dr$Th <- m3_Th
      data$rock$Dr$Th.err <- m3_Th_err
      data$rock$Dr$K <- m3_K
      data$rock$Dr$K.err <- m3_K_err
      data$rock$Dr$Rb <- m3_Rb
      data$rock$Dr$Rb.err <- m3_Rb_err
      data$rock$Dr$K2Rb <- m3_K2Rb

      data$rock$Dr$alpha <- m3_alpha
      data$rock$Dr$alpha.err <- m3_alpha_err
      data$rock$Dr$beta <- m3_beta
      data$rock$Dr$beta.err <- m3_beta_err
      data$rock$Dr$gamma <- m3_gamma
      data$rock$Dr$gamma.err <- m3_gamma_err

      data$rock$info$water.content <- m3_water
      data$rock$info$water.content.err <- m3_water_err
      data$rock$info$density <- m3_density
      data$rock$info$density.err <- m3_density_err
      data$rock$info$ratio <- m3_proportion
      data$rock$info$ratio <- m3_proportion_err



      data$cosmic$depth <- depth
      data$cosmic$depth.err <- depth_err

      data$cosmic$latitude <- latitude
      data$cosmic$longitude <- longitude
      data$cosmic$altitude <- altitude

      data$cosmic$Dr <- dc
      data$cosmic$Dr.err <- dc_err

      data$cosmic$corr.fieldChanges <- fieldChange

      res <- try(use_DRAC4cave(data))

      if(class(res) == "try-error"){
        result <- NULL

      }else{
        result <- res
   }

    }else if(material == "cave flint"){
     result <- NULL
    }else{
     return(NULL)
    }

    #BACK
    if(!is.null(result)){
      updateTabsetPanel(session, "drPage", "D\u0309")
    }
    
    return(result)

  })
  #############################################
  # Result
  #############################################

  output$resultDrTab <- renderUI({
    fluidRow(column(width = 12,
                    uiOutput("inputTab"),
                    uiOutput("resultTab"))
    )
  })

  output$dracText <- renderUI({

    material <- input$material

    if(!(material %in% c("sediment", "flint",  "ceramic", "cave sediment"))){
      helpText("This context is still under development.")

    }else{
      
      dr <- dr_DATA.Dr()

      if(is.null(dr)){
        if(input$dracButton > 0){
          helpText("Missing data")
        }else{
          helpText("Waiting for D\u0309 calculation")
        }
      }else{
        helpText("D\u0309 calculated")
      }
    }
  })

  # Results

  output$inputTab <- renderUI({
    fluidRow(
      h4("Input table"),
      DT::dataTableOutput(outputId = "concentrationTable"),
      checkboxInput(inputId = "concentrationTexBox",label = "LaTeX source",
                    value = FALSE),
      verbatimTextOutput(outputId = "concentrationTex")
    )
  })

  output$resultTab <- renderUI({
    fluidRow(
      h4("Result table"),
      DT::dataTableOutput(outputId = "doseRateTable"),
      checkboxInput(inputId = "doseRateTexBox",label = "LaTeX source",
                    value = FALSE),
      verbatimTextOutput(outputId = "doseRateTex")
    )
  })

  output$concentrationTable <- DT::renderDataTable({
    TABLE.concentration()
    
  })

  output$concentrationTex <- renderText({
    if(input$concentrationTexBox){
      concentrationTex()
    }else{
      return(NULL)
    }
  })

  TABLE.concentration <- reactive({
    project <- input$sa_project
    sample <- input$sa_sample

    material <- input$material

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    m1_U <- as.numeric(input$m1_U)
    m1_U_err <- as.numeric(input$m1_U_err)
    m1_Th <- as.numeric(input$m1_Th)
    m1_Th_err <- as.numeric(input$m1_Th_err)
    m1_K <- as.numeric(input$m1_K)
    m1_K_err <- as.numeric(input$m1_K_err)

    m1_aValue <- as.numeric(input$m1_aValue)
    m1_aValue_err <- as.numeric(input$m1_aValue_err)

    m2_U <- as.numeric(input$m2_U)
    m2_U_err <- as.numeric(input$m2_U_err)
    m2_Th <- as.numeric(input$m2_Th)
    m2_Th_err <- as.numeric(input$m2_Th_err)
    m2_K <- as.numeric(input$m2_K)
    m2_K_err <- as.numeric(input$m2_K_err)

    m2_water <- as.numeric(input$m2_water)
    m2_water_err <- as.numeric(input$m2_water_err)

    m3_U <- as.numeric(input$m3_U)
    m3_U_err <- as.numeric(input$m3_U_err)
    m3_Th <- as.numeric(input$m3_Th)
    m3_Th_err <- as.numeric(input$m3_Th_err)
    m3_K <- as.numeric(input$m3_K)
    m3_K_err <- as.numeric(input$m3_K_err)

    m3_water <- as.numeric(input$m3_water)
    m3_water_err <- as.numeric(input$m3_water_err)

    if(material == "sediment"){
      table <- data.frame(Project = project,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Project'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "flint"){
      table <- data.frame(Project = project,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Project'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Flint'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "ceramic"){
      table <- data.frame(Project = project,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                          Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                          K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                          a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                          Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                          K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                          water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                          Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                          K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                          water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Project'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Ceramic'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "cave sediment"){
      table <- data.frame(Project = project,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m_3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                           Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                           K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                           water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Project'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Sediment'),
            tags$th(colspan = 4, 'Rock')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )
    }else if(material == "cave flint"){
      table <- data.frame(Project = project,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m_3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                           Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                           K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                           water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Project'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Flint'),
            tags$th(colspan = 4, 'Sediment'),
            tags$th(colspan = 4, 'Rock')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )
    }else{

    }

    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))
    return(datatable)
  })

  concentrationTex <- reactive({
    project <- input$sa_project
    sample <- input$sa_sample

    material <- input$material

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    m1_U <- as.numeric(input$m1_U)
    m1_U_err <- as.numeric(input$m1_U_err)
    m1_Th <- as.numeric(input$m1_Th)
    m1_Th_err <- as.numeric(input$m1_Th_err)
    m1_K <- as.numeric(input$m1_K)
    m1_K_err <- as.numeric(input$m1_K_err)

    m1_aValue <- as.numeric(input$m1_aValue)
    m1_aValue_err <- as.numeric(input$m1_aValue_err)

    m2_U <- as.numeric(input$m2_U)
    m2_U_err <- as.numeric(input$m2_U_err)
    m2_Th <- as.numeric(input$m2_Th)
    m2_Th_err <- as.numeric(input$m2_Th_err)
    m2_K <- as.numeric(input$m2_K)
    m2_K_err <- as.numeric(input$m2_K_err)

    m2_water <- as.numeric(input$m2_water)
    m2_water_err <- as.numeric(input$m2_water_err)

    m3_U <- as.numeric(input$m3_U)
    m3_U_err <- as.numeric(input$m3_U_err)
    m3_Th <- as.numeric(input$m3_Th)
    m3_Th_err <- as.numeric(input$m3_Th_err)
    m3_K <- as.numeric(input$m3_K)
    m3_K_err <- as.numeric(input$m3_K_err)

    m3_water <- as.numeric(input$m3_water)
    m3_water_err <- as.numeric(input$m3_water_err)


    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    if(material == "sediment"){

      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(project, "&", sample, "&" ,
                       "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(material =="flint"){
      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Flint} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(project, "&", sample, "&" ,
                       "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "ceramic"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Ceramic} & \\multicolumn{4}{c|}{Sediment} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(project, "&", sample, "&" ,
                       "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "cave sediment"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(project, "&", sample, "&" ,
                       "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "cave flint"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{flint} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(project, "&", sample, "&" ,
                       "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")
    }


    table <- c(table,
               "\\hline", "\n",
               "\\end{tabular}", "\n",
               "\\end{table}", "\n",
               "\\end{landscape}", "\n"
    )



    return(table)
  })


  output$doseRateTable <- DT::renderDataTable({
    TABLE.doseRate()
  })

  output$doseRateTex <- renderText({
    if(input$doseRateTexBox){
      doseRateTex()
    }else{
      return(NULL)
    }
  })

  TABLE.doseRate <- reactive({

    material <- input$material

    project <- input$sa_project
    sample <- input$sa_sample

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- 0
      alpha.Dr_err <- 0
      beta.Dr <- 0
      beta.Dr_err <- 0
      gamma.Dr <- 0
      gamma.Dr_err <- 0
      cosmic.Dr <- 0
      cosmic.Dr_err <- 0

      tot.Dr <- 0
      tot.Dr_err <- 0

    }else{
      data <- Dr.values@data

      alpha.Dr <- data$R$alpha.Dr
      alpha.Dr_err <- data$R$alpha.Dr.err
      beta.Dr <- data$R$beta.Dr
      beta.Dr_err <- data$R$beta.Dr.err
      gamma.Dr <- data$R$gamma.Dr
      gamma.Dr_err <- data$R$gamma.Dr.err
      cosmic.Dr <- data$R$cosmic.Dr
      cosmic.Dr_err <- data$R$cosmic.Dr.err

      tot.Dr <- data$Dr
      tot.Dr_err <- data$Dr.err
    }

    table <- data.frame(Project = project,
                        Sample = sample,
                        Depth = paste(depth, "\u00B1", depth_err),
                        Dr = data.frame(alpha = paste(round(alpha.Dr), "\u00B1", round(alpha.Dr_err,3)),
                                        beta = paste(round(beta.Dr,3), "\u00B1", round(beta.Dr_err,3)),
                                        gamma = paste(round(gamma.Dr,3), "\u00B1", round(gamma.Dr_err,3)),
                                        cosmic = paste(round(cosmic.Dr,3), "\u00B1", round(cosmic.Dr_err,3)),
                                        total = paste(round(tot.Dr,3), "\u00B1", round(tot.Dr_err,3))))

    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th(rowspan = 2, 'Project'),
          tags$th(rowspan = 2, 'Sample'),
          tags$th(rowspan = 2, 'Depth [m]'),
          tags$th(colspan = 5, 'D\u0309 [Gy/ka]')
        ),
        tags$tr(
          lapply(c('\u03b1 [Gy/ka]', '\u03b2 [Gy/ka]', "\u03b3 [Gy/ka]", "cosmic [Gy/ka]", "Tot. [Gy/ka]"), tags$th)
        )
      )
    )

    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))
    return(datatable)
  })

  doseRateTex <- reactive({

    material <- input$material

    site <- input$sa_site
    sample <- input$sa_sample

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- 0
      alpha.Dr_err <- 0

      beta.Dr <- 0
      beta.Dr_err <- 0

      gamma.Dr <- 0
      gamma.Dr_err <- 0

      cosmic.Dr <- 0
      cosmic.Dr_err <- 0

      tot.Dr <- 0
      tot.Dr_err <- 0

    }else{
      data <- Dr.values@data

      alpha.Dr <- data$R$alpha.Dr
      alpha.Dr_err <- data$R$alpha.Dr.err

      beta.Dr <- data$R$beta.Dr
      beta.Dr_err <- data$R$beta.Dr.err

      gamma.Dr <- data$R$gamma.Dr
      gamma.Dr_err <- data$R$gamma.Dr.err

      cosmic.Dr <- data$R$cosmic.Dr
      cosmic.Dr_err <- data$R$cosmic.Dr.err

      tot.Dr <- data$Dr
      tot.Dr_err <- data$Dr.err
    }

    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    table <- c(table,
               "\\begin{tabular}{|c|c|c|c|c|c|c|c|}", "\n",
               "\\hline", "\n",
               "\\multirow{2}{*}{Project} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & \\multicolumn{5}{c|}{$\\dot{D}$ [Gy/ka]} \\\\", "\n",
               "\\cline{4-8}", "\n",
               "& & & $\\alpha$ [Gy/ka] & $\\beta$ [Gy/ka] & $\\gamma$ [Gy/ka] & $D_c$ [Gy/ka] & Tot. [Gy/ka] \\\\", "\n",
               "\\hline", "\n")

    table <- c(table,
               paste(site, "&", sample, "&" ,
                     "$",round(depth,3), "\\pm", round(depth_err,3), "$", "&",
                     "$",round(alpha.Dr,3) , "\\pm", round(alpha.Dr_err,3), "$", "&",
                     "$",round(beta.Dr,3) , "\\pm", round(beta.Dr_err,3), "$", "&",
                     "$",round(gamma.Dr,3) , "\\pm", round(gamma.Dr_err,3), "$", "&",
                     "$",round(cosmic.Dr,3) , "\\pm", round(cosmic.Dr_err,3), "$", "&",
                     "$",round(tot.Dr,3) , "\\pm", round(tot.Dr_err,3), "$", "\\\\"),
               "\n")

    table <- c(table,
               "\\hline", "\n",
               "\\end{tabular}", "\n",
               "\\end{table}", "\n",
               "\\end{landscape}", "\n"
    )

    return(table)
  })
  ##############################################################################################
  # Age Estimation
  ##############################################################################################

  output$agePage <- renderUI({
    sidebarLayout(sidebarPanel(width = 3,
                               uiOutput(outputId = "ag_dataPanel")),
                  mainPanel(width = 9,
                            uiOutput(outputId = "ag_AgePanel")))
  })
  
  output$ag_dataPanel <- renderUI({
    
    fluidRow(column(width = 12,
                    uiOutput(outputId = "ag_De"),
                    uiOutput(outputId = "ag_Dr")
                    ))
  }) 

  output$ag_De <- renderUI({
    De.values <- de_DATA.De()
    
    if(is.null(De.values)){
      De <- ""
      De_err <-""
    }else{
      De <- as.numeric(De.values$De)
      De_err <- as.numeric(De.values$De_err)
    }
    
    fluidRow(column(width = 6,
                    textInput(inputId = "ag_De",
                              label = "D\u2091",
                              value = De)),
             column(width = 6,
                    textInput(inputId = "ag_De_err",
                              label = "\u03b4D\u2091",
                              value = De_err))
             )
  })
  
  output$ag_Dr <- renderUI({
    Dr.values <- dr_DATA.Dr()
    
    if(is.null(Dr.values)){
      Dr <- ""
      Dr_err <-""
    }else{
      Dr <- as.numeric(Dr.values@data$Dr)
      Dr <- round(Dr,3)
      Dr_err <- as.numeric(Dr.values@data$Dr.err)
      Dr_err <- round(Dr_err,3)
    }
    
    fluidRow(column(width = 6,
                    textInput(inputId = "ag_Dr",
                              label = "D\u0309",
                              value = Dr)),
             column(width = 6,
                    textInput(inputId = "ag_Dr_err",
                              label = "\u03b4D\u0309",
                              value = Dr_err))
             )
  })
  
  output$ag_AgePanel <- renderUI({
    fluidRow(column(width = 12,
                    DT::dataTableOutput(outputId = "ageTable")
    ))
  })
  
  output$ageTable <- DT::renderDataTable({
    TABLE.age()
  })

  TABLE.age <- reactive({

    project <- input$sa_project
    site <- input$sa_site
    sample <- input$sa_sample

    date <- input$sa_date
    year <- as.numeric(format(date,"%Y"))

    De.values <- de_DATA.De()
    Dr.values <- dr_DATA.Dr()
    age.values <- ag_DATA.age()

    if(is.null(age.values)){
      if(is.null(De.values)){
        De <- 0
        De_err <- 0
      }else{
        De <- De.values$De
        De_err <- De.values$De_err
      }

      if(is.null(Dr.values)){
        Dr <- 0
        Dr_err <- 0
      }else{
        Dr <- as.numeric(Dr.values@data$Dr)
        Dr_err <- as.numeric(Dr.values@data$Dr.err)
      }

      Age <- 0
      Age_err <- 0

    }else{
      De <- age.values$De
      De_err <- age.values$De_err

      Dr <- age.values$Dr
      Dr_err <- age.values$Dr_err

      Age <- age.values$Age
      Age_err <- age.values$Age_err
    }

    table <- data.frame(Project = project,
                        Site = site, 
                        Year = year,
                        Sample = sample,
                        Age = data.frame(De = paste(round(De,3), "\u00B1", round(De_err,3)),
                                        Dr = paste(round(Dr,3), "\u00B1", round(Dr_err,3)),
                                        Age = paste(round(Age,3), "\u00B1", round(Age_err,3))))

    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th('Project'),
          tags$th('Site'),
          tags$th('Year'),
          tags$th('Sample'),
          tags$th('D\u2091 [Gy]'),
          tags$th('D\u0309 [Gy/ka]'),
          tags$th('Age [ka]')
        )
      )
    )

    datatable <- datatable(data = table, 
                           container = container, 
                           rownames = FALSE
                           , options = list(dom = "t"))
    return(datatable)
  })

  ag_DATA.age <- reactive({
    
    De <- input$ag_De
    if(is.null(De)|| De == ""){
      De <- NA
    }else{
      De <- gsub(",",".", De, fixed = TRUE)
      De <- as.numeric(De)
    }

    De_err <- input$ag_De_err
    if(is.null(De_err) || De_err == ""){
      De_err <- NA
    }else{
      De_err <- gsub(",",".", De_err, fixed = TRUE)
      De_err <- as.numeric(De_err)      
    }
    
    Dr <- input$ag_Dr
    if(is.null(Dr) || Dr == ""){
      Dr <- NA
    }else{
      Dr <- gsub(",",".", Dr, fixed = TRUE)
      Dr <- as.numeric(Dr)
    }
    Dr_err <- input$ag_Dr_err
    
    if(is.null(Dr_err) || Dr_err == ""){
      Dr_err <- NA
    }else{
      Dr_err <- gsub(",",".", Dr_err, fixed = TRUE)
      Dr_err <- as.numeric(Dr_err)
    }
    

    Age <- De/Dr
    De_rel <- De_err/De
    Dr_rel <- Dr_err/Dr
    Age_rel <- sqrt(sum(De_rel^2,Dr_rel^2,na.rm = TRUE))
    Age_err <- Age_rel*Age

    result <- list(De = De,
                   De_err = De_err,
                   Dr = Dr,
                   Dr_err = Dr_err,
                   Age = Age,
                   Age_err = Age_err)
    return(result)
  })
  ##############################################################################################
  # Help
  ##############################################################################################

  output$helpPage<- renderUI({

  })
})
