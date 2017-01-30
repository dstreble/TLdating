require(Luminescence)
require(TLdating)
require(shiny)
require(shinyjs)
require(DT)
require(xtable)

shinyServer(function(input, output, session){

  DATA.values <- reactiveValues()
  
  ##############################################################################################
  # Initialize
  ##############################################################################################

  ##############################################################################################
  # Sample information
  ##############################################################################################

  output$infoPage <- renderUI({
    sidebarLayout(
      sidebarPanel(h4("Sample information"),
                   textInput(inputId = "project",
                             label = "Project",
                             placeholder = "required",
                             value = isolate(DATA.values$project)),
                   textInput(inputId = "site",
                             label = "Site",
                             placeholder = "required",
                             value = isolate(DATA.values$site)),
                   # dateInput(inputId = "date",
                   #           label = "Sampling year",
                   #           format = "yyyy",
                   #           startview = "decade"),
                   textInput(inputId = "date",
                             label = "Sampling year",
                             value = isolate(DATA.values$date)),
                   textInput(inputId = "sample",
                             label = "Sample name",
                             placeholder = "required",
                             value = isolate(DATA.values$sample))

      ),
      mainPanel(div(class="welcomeText",
                    list(h4("Welcome in ShinyDRAC")
                         ))
      )
    )
  })

  # --------------------------------
  # DATA.values update
  # --------------------------------
  
  save.project <- observe({
    new <- input$project
    old <- isolate(DATA.values$project)
    
    new <- as.character(new)

    if(length(new)==0){
      new <- old
    }
    
    DATA.values$project <- new
  })

  save.site <- observe({
    new <- input$site
    old <- isolate(DATA.values$project)

    new <- as.character(new)

    if(length(new)==0){
      new <- old
    }
    
    DATA.values$site <- new
  })

  save.date <- observe({
    new <- input$date
    old <- isolate(DATA.values$date)
      
    new <- as.numeric(new)
    
    if(length(new) == 0){
      if(is.null(old)){
        new <- Sys.Date()
        new <- as.numeric(format(new,"%Y"))
      }else{
        new <- old
      }
    }
    
    DATA.values$date <- new
  })

  save.sample <- observe({
    new <- input$sample
    old <- isolate(DATA.values$sample)
    
    new <- as.character(new)

    if(length(new)==0){
      new <- old
    }
    
    DATA.values$sample <- new
  })

  
  ##############################################################################################
  # Input data
  ##############################################################################################

  output$inPage <- renderUI({

    sidebarLayout(
      uiOutput("inputSidePanel"),
      uiOutput("inputMainPanel")
    )
  })

  output$inputSidePanel <- renderUI({

    sidebarPanel(width = 3,
                 uiOutput(outputId = "deValue"),
                 h4("General parameters"),
                 selectInput(inputId = "context",
                             label = "Context",
                             choices = c("sediment",
                                         "flint",
                                         "ceramic",
                                         "cave sediment",
                                         "cave flint"),
                             selected = isolate(DATA.values$context)),
                 selectInput(inputId = "mineral",
                             label = "Mineral",
                             choices = c("Q","F","PM"),
                             selected = isolate(DATA.values$mineral)),
                 selectInput(inputId = "conversionFactor",
                             label = "Conversion factor",
                             choices = c("AdamiecAitken1998",
                                         "Guerinetal2011",
                                         "Liritzisetal2013",
                                         'X'),
                             selected = isolate(DATA.values$conversionFactor)),
                 selectInput(inputId = "alphaSizeFactor",
                             label = "Alpha size attenuation factor",
                             choices = c("Bell1980",
                                         "Brennanetal1991"),
                             selected = isolate(DATA.values$alphaSizeFactor)),
                 uiOutput(outputId="betaSizeFactor"),
                 selectInput(inputId = "betaEtchFactor",
                             label = "Beta etch attenuation factor",
                             choices = c("Bell1979",
                                         "Brennan2003"),
                             selected = isolate(DATA.values$betaEtchFactor)),

                 br(),
                 actionButton(inputId = "dracButton",
                              label = "Age estimation"),

                 uiOutput(outputId= "dracText")
    )
  })

  output$deValue <- renderUI({
    fluidRow(
      h4("Equivalent dose"),
      column(width = 6,
             textInput(inputId = "de",
                       label = "D\u2091 [Gy]",
                       value = isolate(DATA.values$de),
                       placeholder = "required")),
      column(width = 6,
             textInput(inputId = "de_err",
                       label = "\u03B4D\u2091",
                       value = isolate(DATA.values$de_err),
                       placeholder = "required"))
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
    context <- DATA.values$context

    if(is.null(context)){
      return(NULL)
    }

    if(context %in% c("ceramic", "cave sediment", "cave flint")){
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
    }else if(context %in% c("sediment", "flint")){
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
    context <- DATA.values$context

    if(is.null(context)){
      return(NULL)
    }

    if(context %in% c("ceramic", "cave sediment", "cave flint")){

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
    }else if(context %in% c("sediment", "flint")){
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

    context <- DATA.values$context

    if(is.null(context)){
      return(NULL)
    }

    if(context %in% c("ceramic", "cave sediment", "cave flint")){

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

    context <- DATA.values$context

    if(is.null(context)){
      return(NULL)
    }

    if(context %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,
             h4("Dc information"),

             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = isolate(DATA.values$DcRadioButton)),

             uiOutput(outputId = "DcLoc"),


             uiOutput(outputId="directDc"),

             h5(tags$b("Depth [m]")),

             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                value = isolate(DATA.values$depth),
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                value = isolate(DATA.values$depth_err),
                                placeholder = "required"))
             )
      )
    }else if(context %in% c("sediment", "flint")){
      column(width = 4,
             h4("Dc information"),

             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = isolate(DATA.values$DcRadioButton)),

             uiOutput(outputId = "DcLoc"),


             uiOutput(outputId="directDc"),

             h5(tags$b("Depth [m]")),

             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                value = isolate(DATA.values$depth),
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                value = isolate(DATA.values$depth_err),
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
                  selected = isolate(DATA.values$betaSizeFactor))

    }else if(input$mineral == "F"){
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-F"),
                  selected = isolate(DATA.values$betaSizeFactor))

    }else{
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-Q",
                              "Guerinetal2012-F"),
                  selected = isolate(DATA.values$betaSizeFactor))
    }
  })

  # m1
  # Text
  output$m1_text <- renderUI({

    if(DATA.values$context %in% c("flint","cave flint")){
      h4("Flint information")

    }else if(DATA.values$context %in% c("ceramic", "cave sediment", "sediment") ){

      h4("Grain information")
    }
  })

  output$m1_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m1_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = c(isolate(DATA.values$m1_doseRateBox), "radioelement concentration"))
  })

  # Concentration
  output$m1_U <- renderUI({
    if("radioelement concentration" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_U",
                         label = "U [ppm]",
                         value = isolate(DATA.values$m1_U))),
        column(width = 6,
               textInput(inputId = "m1_U_err",
                         label = "\u03B4U",
                         value = isolate(DATA.values$m1_U_err)))
      )
    }
  })

  output$m1_Th <- renderUI({
    if("radioelement concentration" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_Th",
                         label = "Th [ppm]",
                         value = isolate(DATA.values$m1_Th))
               ),
        column(width = 6,
               textInput(inputId = "m1_Th_err",
                         label = "\u03B4Th",
                         value = isolate(DATA.values$m1_Th_err))
               )
      )
    }
  })

  output$m1_K <- renderUI({
    if("radioelement concentration" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_K",
                         label = "K [%]",
                         value = isolate(DATA.values$m1_K))
               ),
        column(width = 6,
               textInput(inputId = "m1_K_err",
                         label = "\u03B4K",
                         value = isolate(DATA.values$m1_K_err))
               )
      )
    }
  })

  output$m1_K2RbBox <- renderUI({
    if("radioelement concentration" %in% DATA.values$m1_doseRateBox){
      checkboxInput(inputId = "m1_K2RbBox",
                    label = "Rb from K",
                    value = isolate(DATA.values$m1_K2RbBox))
    }
  })

  output$m1_Rb <- renderUI({
    if("radioelement concentration" %in% DATA.values$m1_doseRateBox){

      if(!is.null(DATA.values$m1_K2RbBox) && !DATA.values$m1_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m1_Rb",
                           label = "Rb [ppm]",
                           value = isolate(DATA.values$m1_Rb))
                 ),
          column(width = 6,
                 textInput(inputId = "m1_Rb_err",
                           label = "\u03B4Rb",
                           value = isolate(DATA.values$m1_Rb_err))
                 )
        )
      }
    }
  })

  #dose rate

  output$m1_alpha <- renderUI({
    if("direct measurement" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_alpha",
                         label = "\u03B1 [Gy]",
                         value = isolate(DATA.values$m1_alpha))
               ),
        column(width = 6,
               textInput(inputId = "m1_alpha_err",
                         label = "\u03B4\u03b1",
                         value = isolate(DATA.values$m1_alpha_err))
               )
      )
    }
  })

  output$m1_beta <- renderUI({
    if("direct measurement" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_beta",
                         label = "\u03B2 [Gy]",
                         value = isolate(DATA.values$m1_beta))
               ),
        column(width = 6,
               textInput(inputId = "m1_beta_err",
                         label = "\u03B4\u03B2",
                         value = isolate(DATA.values$m1_beta_err))
               )
      )
    }
  })

  output$m1_gamma <- renderUI({
    if("direct measurement" %in% DATA.values$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_gamma",
                         label = "\u03B3 [Gy]",
                         value = isolate(DATA.values$m1_gamma))
               ),
        column(width = 6,
               textInput(inputId = "m1_gamma_err",
                         label = "\u03B4\u03B3",
                         value = isolate(DATA.values$m1_gamma_err))
               )
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
                       value = isolate(DATA.values$m1_size_min))
             ),
      column(width = 6,
             textInput(inputId = "m1_size_max",
                       label = "max",
                       placeholder = "required",
                       value = isolate(DATA.values$m1_size_max))
             )
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
                       value = isolate(DATA.values$m1_etch_min))
             ),
      column(width = 6,
             textInput(inputId = "m1_etch_max",
                       label = "max",
                       placeholder = "required",
                       value = isolate(DATA.values$m1_etch_max))
             )
    )
  })

  output$m1_aValueText <- renderUI({
    h5(tags$b("a-value"))
  })

  output$m1_aValue <- renderUI({

    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_aValue",
                       label = "a",
                       value = isolate(DATA.values$m1_aValue))
             ),
      column(width = 6,
             textInput(inputId = "m1_aValue_err",
                       label = "\u03B4a",
                       value = isolate(DATA.values$m1_aValue_err))
             )
    )
  })

  output$m1_densityText <- renderUI({
    if(DATA.values$context %in% c("flint", "cave flint")){
      h5(tags$b("Density [mg/mm3]"))
    }
  })

  output$m1_density <- renderUI({

    if(DATA.values$context %in% c("flint", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_density",
                         label = "\u03C1",
                         value = isolate(DATA.values$m1_density))
               ),
        column(width = 6,
               textInput(inputId = "m1_density_err",
                         label = "\u03B4\u03C1",
                         value = isolate(DATA.values$m1_density_err))
               )
      )
    }
  })

  output$m1_waterText <- renderUI({
    if(DATA.values$context %in% c("")){
      h5("Water content m= (W-D)/D [%]")
    }
  })

  output$m1_water <- renderUI({
    if(DATA.values$context %in% c("")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_water",
                         label = "m",
                         value = isolate(DATA.values$m1_water))),
        column(width = 6,
               textInput(inputId = "m1_water_err",
                         label = "\u03B4m",
                         value = isolate(DATA.values$m1_water_err)))
      )

    }
  })



  # m2
  output$m2_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m2_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = c(isolate(DATA.values$m2_doseRateBox),"radioelement concentration") )
  })

  # concentration
  output$m2_text <- renderUI({

    if(DATA.values$context %in% c("ceramic")){
      h4("Ceramic information")

    }else if(DATA.values$context %in% c("flint",  "sediment", "cave sediment", "cave flint") ){

      h4("Sediment information")
    }
  })

  output$m2_U <- renderUI({
    if("radioelement concentration" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_U",
                         label = "U [ppm]",
                         value = isolate(DATA.values$m2_U))
               ),
        column(width = 6,
               textInput(inputId = "m2_U_err",
                         label = "\u03B4U",
                         value = isolate(DATA.values$m2_U_err))
               )
      )
    }
  })

  output$m2_Th <- renderUI({
    if("radioelement concentration" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_Th",
                         label = "Th [ppm]",
                         value = isolate(DATA.values$m2_Th))
               ),
        column(width = 6,
               textInput(inputId = "m2_Th_err",
                         label = "\u03B4Th",
                         value = isolate(DATA.values$m2_Th_err))
               )
      )
    }
  })

  output$m2_K <- renderUI({
    if("radioelement concentration" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_K",
                         label = "K [%]",
                         value = isolate(DATA.values$m2_K))
               ),
        column(width = 6,
               textInput(inputId = "m2_K_err",
                         label = "\u03B4K",
                         value = isolate(DATA.values$m2_K_err))
               )
      )
    }
  })

  output$m2_K2RbBox <- renderUI({
    if("radioelement concentration" %in% DATA.values$m2_doseRateBox){
      checkboxInput(inputId = "m2_K2RbBox",
                    label = "Rb from K",
                    value = isolate(DATA.values$m2_K2RbBox))
    }
  })

  output$m2_Rb <- renderUI({
    if("radioelement concentration" %in% DATA.values$m2_doseRateBox){
      if(!is.null(DATA.values$m2_K2RbBox) && !DATA.values$m2_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m2_Rb",
                           label = "Rb [ppm]",
                           value = isolate(DATA.values$m2_Rb))
                 ),
          column(width = 6,
                 textInput(inputId = "m2_Rb_err",
                           label = "\u03B4Rb",
                           value = isolate(DATA.values$m2_Rb_err))
          )
        )
      }
    }
  })

  #dose rate

  output$m2_alpha <- renderUI({
    if("direct measurement" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_alpha",
                         label = "\u03B1 [Gy]",
                         value = isolate(DATA.values$m2_alpha))
               ),
        column(width = 6,
               textInput(inputId = "m2_alpha_err",
                         label = "\u03B4\u03B1",
                         value = isolate(DATA.values$m2_alpha_err))
        )
      )
    }
  })

  output$m2_beta <- renderUI({
    if("direct measurement" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_beta",
                         label = "\u03B2 [Gy]",
                         value = isolate(DATA.values$m2_beta))
               ),
        column(width = 6,
               textInput(inputId = "m2_beta_err",
                         label = "\u03B4\u03B2",
                         value = isolate(DATA.values$m2_beta_err))
        )
      )
    }
  })

  output$m2_gamma <- renderUI({
    if("direct measurement" %in% DATA.values$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_gamma",
                         label = "\u03B3 [Gy]",
                         value = isolate(DATA.values$m2_gamma))
               ),
        column(width = 6,
               textInput(inputId = "m2_gamma_err",
                         label = "\u03B4\u03B3",
                         value = isolate(DATA.values$m2_gamma_err))
        )
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
                       value = isolate(DATA.values$m2_density))
             ),
      column(width = 6,
             textInput(inputId = "m2_density_err",
                       label = "\u03B4\u03C1",
                       value = isolate(DATA.values$m2_density_err))
      )
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
                       value = isolate(DATA.values$m2_water))
             ),
      column(width = 6,
             textInput(inputId = "m2_water_err",
                       label = "\u03B4m",
                       placeholder = "required",
                       value = isolate(DATA.values$m2_water_err)))
    )
  })

  output$m2_proportion <- renderUI({

    if(DATA.values$context %in% c("cave sediment", "cave flint")){

      fluidRow(
        column(width = 6,
               p(strong("p [%]"),
                 br(),
                 p(DATA.values$m2_proportion*100))
        ),
        column(width = 6,
               p(strong("\u03B4p"),
                 br(),
                 div(DATA.values$m2_proportion_err*100))
        )
      )
    }
  })

  # m3
  # concentration
  output$m3_text <- renderUI({

    if(DATA.values$context %in% c("ceramic")){
      h4("Sediment information")

    }else if(DATA.values$context %in% c("cave sediment", "cave flint")){
      h4("Rock information")

    }
  })

  output$m3_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m3_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = c(isolate(DATA.values$m3_doseRateBox),"radioelement concentration"))
  })

  output$m3_U <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "radioelement concentration" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_U",
                         label = "U [ppm]",
                         value = isolate(DATA.values$m3_U))
               ),
        column(width = 6,
               textInput(inputId = "m3_U_err",
                         label = "\u03B4U",
                         value = isolate(DATA.values$m3_U_err))
               )
      )
    }
  })

  output$m3_Th <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "radioelement concentration" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_Th",
                         label = "Th [ppm]",
                         value = isolate(DATA.values$m3_Th))),
        column(width = 6,
               textInput(inputId = "m3_Th_err",
                         label = "\u03B4Th",
                         value = isolate(DATA.values$m3_Th_err)))
      )
    }
  })

  output$m3_K <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "radioelement concentration" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_K",
                         label = "K [%]",
                         value = isolate(DATA.values$m3_K))),
        column(width = 6,
               textInput(inputId = "m3_K_err",
                         label = "\u03B4K",
                         value = isolate(DATA.values$m3_K_err)))
      )
    }
  })

  output$m3_K2RbBox <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "radioelement concentration" %in% DATA.values$m3_doseRateBox){
      checkboxInput(inputId = "m3_K2RbBox",
                    label = "Rb from K",
                    value = isolate(DATA.values$m3_K2RbBox))
    }
  })

  output$m3_Rb <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "radioelement concentration" %in% DATA.values$m3_doseRateBox){
      if(!is.null(DATA.values$m3_K2RbBox) && !DATA.values$m3_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m3_Rb",
                           label = "Rb [ppm]",
                           value = isolate(DATA.values$m3_Rb))),
          column(width = 6,
                 textInput(inputId = "m3_Rb_err",
                           label = "\u03B4Rb",
                           value = isolate(DATA.values$m3_Rb_err)))
        )
      }
    }
  })

  #dose rate

  output$m3_alpha <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "direct measurement" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_alpha",
                         label = "\u03B1 [Gy]",
                         value = isolate(DATA.values$m3_alpha))),
        column(width = 6,
               textInput(inputId = "m3_alpha_err",
                         label = "\u03B4\u03B1",
                         value = isolate(DATA.values$m3_alpha_err)))
      )
    }
  })

  output$m3_beta <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "direct measurement" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_beta",
                         label = "\u03B2 [Gy]",
                         value = isolate(DATA.values$m3_beta))),
        column(width = 6,
               textInput(inputId = "m3_beta_err",
                         label = "\u03B4\u03B2",
                         value = isolate(DATA.values$m3_beta_err)))
      )
    }
  })

  output$m3_gamma <- renderUI({
    if(!is.null(DATA.values$m3_doseRateBox) && "direct measurement" %in% DATA.values$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_gamma",
                         label = "\u03B3 [Gy]",
                         value = isolate(DATA.values$m3_gamma))),
        column(width = 6,
               textInput(inputId = "m3_gamma_err",
                         label = "\u03B4\u03B3",
                         value = isolate(DATA.values$m3_gamma_err)))
      )
    }
  })

  #other

  output$m3_densityText <- renderUI({
    if(DATA.values$context %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Density [mg/mm3]"))
    }
  })

  output$m3_density <- renderUI({
    if(DATA.values$context %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_density",
                         label = "\u03C1",
                         value = isolate(DATA.values$m3_density))),
        column(width = 6,
               textInput(inputId = "m3_density_err",
                         label = "\u03B4\u03C1",
                         value = isolate(DATA.values$m3_density_err)))
      )
    }
  })

  output$m3_waterText <- renderUI({
    if(DATA.values$context %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Water content m = (W-D)/D [%]"))
    }
  })

  output$m3_water <- renderUI({
    if(DATA.values$context %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_water",
                         label = "m",
                         placeholder = "required",
                         value = isolate(DATA.values$m3_water))),
        column(width = 6,
               textInput(inputId = "m3_water_err",
                         label = "\u03B4m",
                         placeholder = "required",
                         value = isolate(DATA.values$m3_water_err)))
      )
    }
  })

  output$m3_proportion <- renderUI({

    if(DATA.values$context %in% c("cave sediment", "cave flint")){
      shinyjs::disable("m2_proportion")
      shinyjs::disable("m2_proportion_err")

      fluidRow(
        shinyjs::useShinyjs(),
        column(width = 6,
               numericInput(inputId = "m3_proportion",
                            label = "p [%]",
                            value = isolate(DATA.values$m3_proportion)*100,
                            min = 0,
                            max = 100,
                            step = 5)),
        column(width = 6,
               textInput(inputId = "m3_proportion_err",
                         label = "\u03B4p",
                         value = isolate(DATA.values$m3_proportion_err)*100)
               )
      )
    }
  })

  #Dc
  output$DcLoc <- renderUI({

    #if(input$geoDcBox){
    if(DATA.values$DcRadioButton == "geographical position"){
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
                             value = isolate(DATA.values$fieldChangeBox)),

               checkboxInput(inputId = "shallowDepthBox",
                             label = "Scale for shallow depth",
                             value = isolate(DATA.values$shallowDepthBox))
        )
      )
    }
  })

  output$directDc <- renderUI({

    #if(input$directDcBox){
    if(DATA.values$DcRadioButton == "in-situ measurement"){

      fluidRow(
        column(width = 6,
               textInput(inputId = "dc",
                         label = "Dc [Gy]",
                         value = isolate(DATA.values$dc))),
        column(width = 6,
               textInput(inputId = "dc_err",
                         label = "\u03B4Dc",
                         value = isolate(DATA.values$dc_err)))
      )
    }
  })

  # --------------------------------
  # DATA.values update
  # --------------------------------
  
  save.DcRadioButton <- observe({
    new <- input$DcRadioButton
    old <- isolate(DATA.values$DcRadioButton)
    
    if(is.null(new)){
      if(is.null(old)){
        new <- "geographical position"
      }else{
        new <- old
      }
    }else{
      new <- as.character(new)
    }
    
    DATA.values$DcRadioButton <- new
    
    # directDC
    if(new == "geographical position"){
      DATA.values$directDc <- FALSE
    }else{
      DATA.values$directDc <- TRUE
    }
    
  })
  
  save.m1_doseRateBox <- observe({
    new <- input$m1_doseRateBox
    old <- isolate(DATA.values$m1_doseRateBox)
    
    new <- as.character(new)
    
    
    if(length(new)==0){
      #new <- old
      new <- NULL
    }

    DATA.values$m1_doseRateBox <- new
  })
  
  save.m1_K2RbBox <- observe({
    new <- input$m1_K2RbBox
    old <- isolate(DATA.values$m1_K2RbBox)
    
    
    if(is.null(new)){
      if(!is.logical(old)){
        new <- TRUE
      }else{
        new <- old
      }
    }
    
    DATA.values$m1_K2RbBox <- new
  })
  
  save.m2_doseRateBox <- observe({
    new <- input$m2_doseRateBox
    old <- isolate(DATA.values$m2_doseRateBox)
    
    new <- as.character(new)
    
    if(length(new)==0){
      #new <- old
      new <- NULL
    }
    
    DATA.values$m2_doseRateBox <- new
  })
  
  save.m2_K2RbBox <- observe({
    new <- input$m2_K2RbBox
    old <- isolate(DATA.values$m2_K2RbBox)
    
    if(is.null(new)){
      if(!is.logical(old)){
        new <- TRUE
      }else{
        new <- old
      }
    }
    
    DATA.values$m2_K2RbBox <- new
  })
  
  save.m3_doseRateBox <- observe({
    new <- input$m3_doseRateBox
    old <- isolate(DATA.values$m3_doseRateBox)
    
    new <- as.character(new)
    
    if(length(new)==0){
      #new <- old
      new <- NULL
    }
    
    DATA.values$m3_doseRateBox <- new
  })
  
  save.m3_K2RbBox <- observe({
    new <- input$m3_K2RbBox
    old <- isolate(DATA.values$m3_K2RbBox)
    
    if(is.null(new)){
      if(!is.logical(old)){
        new <- TRUE
      }else{
        new <- old
      }
    }
    
    DATA.values$m3_K2RbBox <- new
  })
  
  save.de <- observe({
    new <- input$de
    old <- isolate(DATA.values$de) 
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$de <- new
  })

  save.de_err <- observe({
    new <- input$de_err
    old <- isolate(DATA.values$de_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$de_err <- new
  })

  save.context <- observe({
    new <- input$context
    old <- isolate(DATA.values$context)
    
    new <- as.character(new)
    
    if(length(new) == 0){
      if(is.null(old)){
        new <- "sediment"
      }else{
        new <- old
      }
    }

    DATA.values$context <- new
  })

  save.mineral <- observe({
    new <- input$mineral
    old <- isolate(DATA.values$mineral)
    
    new <- as.character(new)

    if(length(new) == 0){
      if(is.null(old)){
        new <- 'Q'
      }else{
        new <- old
      }
    }
    
    DATA.values$mineral <- new
  })

  save.conversionFactor <- observe({
    new <- input$conversionFactor
    old <- isolate(DATA.values$conversionFactor)
    
    new <- as.character(new)
    
    if(length(new) == 0){
      if(is.null(old)){
        new <- 'Liritzisetal2013'
      }else{
        new <- old
      }
    }

    DATA.values$conversionFactor <- new
  })

  save.alphaSizeFactor <- observe({
    new <- input$alphaSizeFactor
    old <- isolate(DATA.values$alphaSizeFactor)
    
    new <- as.character(new)

    if(length(new) == 0){
      if(is.null(old)){
        new <- 'Brennanetal1991'
      }else{
        new <- old
      }
    }
    
    DATA.values$alphaSizeFactor <- new
  })

  save.betaSizeFactor <- observe({
    new <- input$betaSizeFactor
    old <- isolate(DATA.values$betaSizeFactor)
    
    new <- as.character(new)

    # Init
    if(length(new) == 0){
      if(is.null(old)){
        if(DATA.values$mineral == "Q"){
          new <- 'Guerinetal2012-Q'
          
        }else if(DATA.values$mineral == "F"){
          new <- "Guerinetal2012-F"
          
        }else{
          new <- "Brennan2003"
        }
      }else{
        new <- old
      }
    }
    
    # Check
    if(new == 'Guerinetal2012-Q' && DATA.values$mineral == "F"){
        new <- "Guerinetal2012-F"
    }
    
    if(new == 'Guerinetal2012-F' && DATA.values$mineral == "Q"){
        new <- "Guerinetal2012-Q"
    }
    
    DATA.values$betaSizeFactor <- new
  })

  save.betaEtchFactor <- observe({
    new <- input$betaEtchFactor
    old <- isolate(DATA.values$betaEtchFactor)
    
    new <- as.character(new)

    #Init
    if(length(new) == 0){
      if(is.null(old)){
        new <- 'Brennan2003'
      }else{
        new <- old
      }
    }
    
    DATA.values$betaEtchFactor <- new
  })

  save.m1_U <- observe({
    new <- input$m1_U
    old <- isolate(DATA.values$m1_U)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_U <- new
  })

  save.m1_U_err <- observe({
    new <- input$m1_U_err
    old <- isolate(DATA.values$m1_U_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_U_err <- new
  })

  save.m1_Th <- observe({
    new <- input$m1_Th
    old <- isolate(DATA.values$m1_Th)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_Th <- new
  })

  save.m1_Th_err <- observe({
    new <- input$m1_Th_err
    old <- isolate(DATA.values$m1_Th_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_Th_err <- new
  })

  save.m1_K <- observe({
    new <- input$m1_K
    old <- isolate(DATA.values$m1_K)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_K <- new
  })

  save.m1_K_err <- observe({
    new <- input$m1_K_err
    old <- isolate(DATA.values$m1_K_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_K_err <- new
  })

  save.m1_Rb <- observe({
    new <- input$m1_Rb
    old <- isolate(DATA.values$m1_Rb)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_Rb <- new
  })

  save.m1_Rb_err <- observe({
    new <- input$m1_Rb_err
    old <- isolate(DATA.values$m1_Rb_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_Rb_err <- new
  })

  save.m1_K2Rb <- observe({
    new <- input$m1_K2RbBox
    old <- isolate(DATA.values$m1_K2Rb)
    
    if(!is.logical(new)){
      if(!is.logical(old)){
        new <- FALSE
      }else{
        new <- old
      }
    }

    DATA.values$m1_K2Rb <- new
  })

  save.m1_alpha <- observe({
    new <- input$m1_alpha
    old <- isolate(DATA.values$m1_alpha)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_alpha <- new
  })

  save.m1_alpha_err <- observe({
    new <- input$m1_alpha_err
    old <- isolate(DATA.values$m1_alpha_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_alpha_err <- new
  })

  save.m1_beta <- observe({
    new <- input$m1_beta
    old <- isolate(DATA.values$m1_beta)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_beta <- new
  })

  save.m1_beta_err <- observe({
    new <- input$m1_beta_err
    old <- isolate(DATA.values$m1_beta_err)
      
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_beta_err <- new
  })

  save.m1_gamma <- observe({
    new <- input$m1_gamma
    old <- isolate(DATA.values$m1_gamma)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_gamma <- new
  })

  save.m1_gamma_err <- observe({
    new <- input$m1_gamma_err
    old <- isolate(DATA.values$m1_gamma_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_gamma_err <- new
  })

  save.m2_U <- observe({
    new <- input$m2_U
    old <- isolate(DATA.values$m2_U)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_U <- new
  })

  save.m2_U_err <- observe({
    new <- input$m2_U_err
    old <- isolate(DATA.values$m2_U_err)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_U_err <- new
  })

  save.m2_Th <- observe({
    new <- input$m2_Th
    old <- isolate(DATA.values$m2_Th)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_Th <- new
  })

  save.m2_Th_err <- observe({
    new <- input$m2_Th_err
    old <- isolate(DATA.values$m2_Th_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_Th_err <- new
  })

  save.m2_K <- observe({
    new <- input$m2_K
    old <- isolate(DATA.values$m2_K)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_K <- new
  })

  save.m2_K_err <- observe({
    new <- input$m2_K_err
    old <- isolate(DATA.values$m2_K_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_K_err <- new
  })

  save.m2_Rb <- observe({
    new <- input$m2_Rb
    old <- isolate(DATA.values$m2_Rb)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_Rb <- new
  })

  save.m2_Rb_err <- observe({
    new <- input$m2_Rb_err
    old <- isolate(DATA.values$m2_Rb_err)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_Rb_err <- new
  })

  save.m2_K2Rb <- observe({
    new <- input$m2_K2RbBox
    old <- DATA.values$m2_K2Rb

    if(!is.logical(new)){
      if(!is.logical(old)){
        new <- FALSE
      }else{
        new <- old
      }
    }
    # if(is.logical(temp.box) && temp.box){
    #   new <- TRUE
    # }else{
    #   new <- FALSE
    # }

    DATA.values$m2_K2Rb <- new
  })

  save.m2_alpha <- observe({
    new <- input$m2_alpha
    old <- isolate(DATA.values$m2_alpha)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_alpha <- new
  })

  save.m2_alpha_err <- observe({
    new <- input$m2_alpha_err
    old <- isolate(DATA.values$m2_alpha_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_alpha_err <- new
  })

  save.m2_beta <- observe({
    new <- input$m2_beta
    old <- isolate(DATA.values$m2_beta)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_beta <- new
  })

  save.m2_beta_err <- observe({
    new <- input$m2_beta_err
    old <- isolate(DATA.values$m2_beta_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_beta_err <- new
  })

  save.m2_gamma <- observe({
    new <- input$m2_gamma
    old <- isolate(DATA.values$m2_gamma)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_gamma <- new
  })

  save.m2_gamma_err <- observe({
    new <- input$m2_gamma_err
    old <- isolate(DATA.values$m2_gamma_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m2_gamma_err <- new
  })

  save.m3_U <- observe({
    new <- input$m3_U
    old <- isolate(DATA.values$m3_U)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_U <- new
  })

  save.m3_U_err <- observe({
    new <- input$m3_U_err
    old <- isolate(DATA.values$m3_U_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_U_err <- new
  })

  save.m3_Th <- observe({
    new <- input$m3_Th
    old <- isolate(DATA.values$m3_Th)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_Th <- new
  })

  save.m3_Th_err <- observe({
    new <- input$m3_Th_err
    old <- isolate(DATA.values$m3_Th_err)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_Th_err <- new
  })

  save.m3_K <- observe({
    new <- input$m3_K
    old <- isolate(DATA.values$m3_K)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_K <- new
  })

  save.m3_K_err <- observe({
    new <- input$m3_K_err
    old <- isolate(DATA.values$m3_K_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_K_err <- new
  })

  save.m3_Rb <- observe({
    new <- input$m3_Rb
    old <- isolate(DATA.values$m3_Rb)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_Rb <- new
  })

  save.m3_Rb_err <- observe({
    new <- input$m3_Rb_err
    old <- isolate(DATA.values$m3_Rb_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_Rb_err <- new
  })

  save.m3_K2Rb <- observe({
    new <- input$m3_K2RbBox
    old <- isolate(DATA.values$m3_K2Rb)
    
    if(!is.logical(new)){
      if(!is.logical(old)){
        new <- FALSE
      }else{
        new <- old
      }
    }

    # if(is.logical(temp.box) && temp.box){
    #   new <- TRUE
    # }else{
    #   new <- FALSE
    # }

    DATA.values$m3_K2Rb <- new
  })

  save.m3_alpha <- observe({
    new <- input$m3_alpha
    old <- isolate(DATA.values$m3_alpha)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_alpha <- new
  })

  save.m3_alpha_err <- observe({
    new <- input$m3_alpha_err
    old <- isolate(DATA.values$m3_alpha_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_alpha_err <- new
  })

  save.m3_beta <- observe({
    new <- input$m3_beta
    old <- isolate(DATA.values$m3_beta)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_beta <- new
  })

  save.m3_beta_err <- observe({
    new <- input$m3_beta_err
    old <- isolate(DATA.values$m3_beta_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_beta_err <- new
  })

  save.m3_gamma <- observe({
    new <- input$m3_gamma
    old <- isolate(DATA.values$m3_gamma)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_gamma <- new
  })

  save.m3_gamma_err <- observe({
    new <- input$m3_gamma_err
    old <- isolate(DATA.values$m3_gamma_err)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m3_gamma_err <- new
  })

  save.shallowDepth <- observe({
    new <- input$shallowDepthBox
    old <- isolate(DATA.values$shallowDepth)
    
    if(!is.logical(new)){
      if(!is.logical(old)){
        new <- TRUE
      }else{
        new >- old
      }
    }

    # if(is.logical(new.box) && new.box){
    #   new <- TRUE
    # }else{
    #   new <- FALSE
    # }

    DATA.values$shallowDepth <- new
  })

  save.m1_size_min <- observe({
    new <- input$m1_size_min
    old <- isolate(DATA.values$m1_size_min)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 100
      }else{
        new <- old
      }
    }
    
    if(new<1){
      new <- 1
    }else if(new > 1000){
      new <- 1000
    }

    
    DATA.values$m1_size_min <- new
  })

  save.m1_size_max <- observe({
    new <- input$m1_size_max
    old <- isolate(DATA.values$m1_size_max)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 200
      }else{
        new <- old
      }
    }
    
    if(new<1){
      new <- 1
    }else if(new > 1000){
      new <- 1000
    }

    DATA.values$m1_size_max <- new
  })

  save.m1_etch_min <- observe({
    new <- input$m1_etch_min
    old <- isolate(DATA.values$m1_etch_min)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }else if(new>30){
      new <- 30
    }

    DATA.values$m1_etch_min <- new
  })

  save.m1_etch_max <- observe({
    new <- input$m1_etch_max
    old <- isolate(DATA.values$m1_etch_max)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }

    if(new < 0){
      new <- 0
    }else if(new>30){
      new <- 30
    }
    
    DATA.values$m1_etch_max <- new
  })

  save.m1_aValue <- observe({
    new <- input$m1_aValue
    old <- isolate(DATA.values$m1_aValue)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_aValue <- new
  })

  save.m1_aValue_err <- observe({
    new <- input$m1_aValue_err
    old <- isolate(DATA.values$m1_aValue_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$m1_aValue_err <- new
  })

  save.m1_water <- observe({
    new <- input$m1_water
    old <- isolate(DATA.values$m1_water)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }

    if(new < 0){
      new <- 0
    }else if(new > 100){
      new <- 100
    }
    
    DATA.values$m1_water <- new
  })

  save.m1_water_err <- observe({
    new <- input$m1_water_err
    old <- isolate(DATA.values$m1_water_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new)){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }

    if(new < 0){
      new <- 0
    }else if(new > 100){
      new <- 100
    }
    
    DATA.values$m1_water_err <- new
  })

  save.m1_density <- observe({
    new <- input$m1_density
    old <- isolate(DATA.values$m1_density)
      
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        if(DATA.values$context %in% c('flint', 'cave flint')){
          new <- 2.65
        }else{
          new <- 1.8
        }
      }else{
        if(DATA.values$context %in% c('flint', 'cave flint') && old == 1.8){
          new <- 2.65
        }else if(!(DATA.values$context %in% c('flint', 'cave flint')) && old == 2.65){
          new <- 1.8  
        }else{
          new <- old
        }
      }
    }

    if(new < 0){
      new <- 0
    }
    
    DATA.values$m1_density <- new
  })

  save.m1_density_err <- observe({
    new <- input$m1_density_err
    old <- isolate(DATA.values$m1_density_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 0.2
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m1_density_err <- new
  })

  save.m2_water <- observe({
    new <- input$m2_water
    old <- isolate(DATA.values$m2_water)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 5
      }else{
        new <- old
      }
    }

    if(new < 0){
      new <- 0
    }
    
    DATA.values$m2_water <- new
  })

  save.m2_water_err <- observe({
    new <- input$m2_water_err#
    old <- isolate(DATA.values$m2_water_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 2
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }
    
    DATA.values$m2_water_err <- new
  })

  save.m2_density <- observe({
    new <- input$m2_density
    old <- isolate(DATA.values$m2_density)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 1.8
      }else{
        new <- old
      }
    }

    if(new < 0){
      new <- 0
    }
    
    DATA.values$m2_density <- new
  })

  save.m2_density_err <- observe({
    new <- input$m2_density_err
    old <- isolate(DATA.values$m2_density_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 0.1
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m2_density_err <- new
  })

  save.m3_water <- observe({
    new <- input$m3_water
    old <- isolate(DATA.values$m3_water)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m3_water <- new
  })

  save.m3_water_err <- observe({
    new <- input$m3_water_err
    old <- isolate(DATA.values$m3_water_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m3_water_err <- new
  })

  save.m3_density <- observe({
    new <- input$m3_density
    old <- isolate(DATA.values$m3_density)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 1.8
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m3_density <- new
  })

  save.m3_density_err <- observe({
    new <- input$m3_density_err
    old <- isolate(DATA.values$m3_density_err)
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 1.8
      }else{
        new <- old
      }
    }
    
    if(new < 0){
      new <- 0
    }

    DATA.values$m3_density_err <- new
  })

  # save.m2_proportion <- observe({
  #   new <- input$m2_proportion
  # 
  #   old <- isolate(DATA.values$m2_proportion)
  #   
  #   new <- as.character(new)
  #   new <- gsub(",",".", new, fixed = TRUE)
  #   new <- as.numeric(new)
  # 
  #   if(length(new)==0 || !is.finite(new) ){
  #     if(is.null(old)){
  #       new <- 100
  #     }else{
  #       new <- old*100
  #     }
  #   }
  #   
  #   if(new < 0){
  #     new <- 0
  #   }else if(new > 100){
  #     new <- 100
  #   }
  # 
  #   new <- new/100
  # 
  #   #DATA.values$m2_proportion <- new
  #   #DATA.values$m3_proportion <- 1-new
  #   
  # })

  # save.m2_proportion_err <- observe({
  #   new <- input$m2_proportion_err
  #   old <- isolate(DATA.values$m2_proportion_err)
  #   
  #   new <- as.character(new)
  #   new <- gsub(",",".", new, fixed = TRUE)
  #   new <- as.numeric(new)
  # 
  #   if(length(new)==0 || !is.finite(new) ){
  #     if(is.null(old)){
  #       new <- 5
  #     }else{
  #       new <- old*100
  #     }
  #   }
  #   
  #   if(new < 0){
  #     new <- 0
  #   }else if(new > 100){
  #     new <- 100
  #   }
  #   
  #   new <- new/100
  # 
  #   #DATA.values$m2_proportion_err <- new
  #   #DATA.values$m3_proportion_err <- new
  # })

  save.m3_proportion <- observe({
    new <- input$m3_proportion
    old <- isolate(DATA.values$m3_proportion)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 0
      }else{
        new <- old*100
      }
    }
    
    if(new < 0){
      new <- 0
    }else if(new > 100){
      new <- 100
    }

    new <- new/100

    DATA.values$m3_proportion <- new
    DATA.values$m2_proportion <- 1-new
    
  })

  save.m3_proportion_err <- observe({
    new <- input$m3_proportion_err
    old <- isolate(DATA.values$m3_proportion_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      if(is.null(old)){
        new <- 5
      }else{
        new <- old*100
      }
    }
    
    if(new < 0){
      new <- 0
    }else if(new > 100){
      new <- 100
    }

    new <- new/100

    DATA.values$m3_proportion_err <- new
    DATA.values$m2_proportion_err <- new
  })

  save.depth <- observe({
    new <- input$depth
    old <- isolate(DATA.values$depth)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$depth <- new
  })

  save.depth_err <- observe({
    new <- input$depth_err
    old <- isolate(DATA.values$depth_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$depth_err <- new
  })

  save.fieldChange <- observe({
    new <- input$fieldChangeBox
    old <- isolate(DATA.values$fieldChangeBox)
    

    if(!is.logical(new)){
      if(!is.logical(old)){
        new <- TRUE
      }else{
        new <- old
      }
    }
    
    DATA.values$fieldChange <- new
  })

  save.latitude <- observe({
    new <- input$latitude
    old <- isolate(DATA.values$latitude)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$latitude <- new
  })

  save.longitude <- observe({
    new <- input$longitude
    old <- isolate(DATA.values$longitude)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$longitude <- new
  })

  save.altitude <- observe({
    new <- input$altitude
    old <- isolate(DATA.values$altitude)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$altitude <- new
  })

  save.dc <- observe({
    new <- input$dc
    old <- isolate(DATA.values$dc)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$dc <- new
  })

  save.dc_err <- observe({
    new <- input$dc_err
    old <- isolate(DATA.values$dc_err)
    
    new <- as.character(new)
    new <- gsub(",",".", new, fixed = TRUE)
    new <- as.numeric(new)

    if(length(new)==0 || !is.finite(new) ){
      new <- old
    }

    DATA.values$dc_err <- new
  })
  
  #########################################################################################################################

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

    de <- DATA.values$de
    if(length(de)==0 || !is.finite(de) ){
      de <- 'X'
    }

    de_err <- DATA.values$de_err
    if(length(de_err)==0 || !is.finite(de_err) ){
      de_err <- 'X'
    }

    context <- DATA.values$context

    directDc <- DATA.values$directDc

    project <- DATA.values$project
    if(project == ""){
      project <- "unknown"
    }else{
      project <- gsub(" ", "", project, fixed = TRUE)
    }

    sample <- DATA.values$sample
    if(sample==""){
      sample <- "unknown"
    }else{
      sample <- gsub(" ", "", sample, fixed = TRUE)
    }

    data <- DATA.values$date

    mineral <- DATA.values$mineral

    conversionFactor <- DATA.values$conversionFactor
    alphaSizeFactor <- DATA.values$alphaSizeFactor
    betaSizeFactor <- DATA.values$betaSizeFactor
    betaEtchFactor <- DATA.values$betaEtchFactor

    m1_U <- DATA.values$m1_U
    if(is.null(m1_U)){
      m1_U <- 'X'
    }
    m1_U_err <- DATA.values$m1_U_err
    if(is.null(m1_U_err)){
      m1_U_err <- 'X'
    }
    m1_Th <- DATA.values$m1_Th
    if(is.null(m1_Th)){
      m1_Th <- 'X'
    }
    m1_Th_err <- DATA.values$m1_Th_err
    if(is.null(m1_Th_err)){
      m1_Th_err <- 'X'
    }
    m1_K <- DATA.values$m1_K
    if(is.null(m1_K)){
      m1_K <- 'X'
    }
    m1_K_err <- DATA.values$m1_K_err
    if(is.null(m1_K_err)){
      m1_K_err <- 'X'
    }
    m1_Rb <- DATA.values$m1_Rb
    if(is.null(m1_Rb)){
      m1_Rb <- 'X'
    }
    m1_Rb_err <- DATA.values$m1_Rb_err
    if(is.null(m1_Rb_err)){
      m1_Rb_err <- 'X'
    }

    m1_K2Rb <- DATA.values$m1_K2Rb

    m1_alpha <- DATA.values$m1_alpha
    if(is.null(m1_alpha)){
      m1_alpha <- 'X'
    }
    m1_alpha_err <- DATA.values$m1_alpha_err
    if(is.null(m1_alpha_err)){
      m1_alpha_err <- 'X'
    }
    m1_beta <- DATA.values$m1_beta
    if(is.null(m1_beta)){
      m1_beta <- 'X'
    }
    m1_beta_err <- DATA.values$m1_beta_err
    if(is.null(m1_beta_err)){
      m1_beta_err <- 'X'
    }
    m1_gamma <- DATA.values$m1_gamma
    if(is.null(m1_gamma)){
      m1_gamma <- 'X'
    }
    m1_gamma_err <- DATA.values$m1_gamma_err
    if(is.null(m1_gamma_err)){
      m1_gamma_err <- 'X'
    }

    m2_U <- DATA.values$m2_U
    if(is.null(m2_U)){
      m2_U <- 'X'
    }
    m2_U_err <- DATA.values$m2_U_err
    if(is.null(m2_U_err)){
      m2_U_err <- 'X'
    }
    m2_Th <- DATA.values$m2_Th
    if(is.null(m2_Th)){
      m2_Th <- 'X'
    }
    m2_Th_err <- DATA.values$m2_Th_err
    if(is.null(m2_Th_err)){
      m2_Th_err <- 'X'
    }
    m2_K <- DATA.values$m2_K
    if(is.null(m2_K)){
      m2_K <- 'X'
    }
    m2_K_err <- DATA.values$m2_K_err
    if(is.null(m2_K_err)){
      m2_K_err <- 'X'
    }
    m2_Rb <- DATA.values$m2_Rb
    if(is.null(m2_Rb)){
      m2_Rb <- 'X'
    }
    m2_Rb_err <- DATA.values$m2_Rb_err
    if(is.null(m2_Rb_err)){
      m2_Rb_err <- 'X'
    }

    m2_K2Rb <- DATA.values$m2_K2Rb

    m2_alpha <- DATA.values$m2_alpha
    if(is.null(m2_alpha)){
      m2_alpha <- 'X'
    }
    m2_alpha_err <- DATA.values$m2_alpha_err
    if(is.null(m2_alpha_err)){
      m2_alpha_err <- 'X'
    }
    m2_beta <- DATA.values$m2_beta
    if(is.null(m2_beta)){
      m2_beta <- 'X'
    }
    m2_beta_err <- DATA.values$m2_beta_err
    if(is.null(m2_beta_err)){
      m2_beta_err <- 'X'
    }
    m2_gamma <- DATA.values$m2_gamma
    if(is.null(m2_gamma)){
      m2_gamma <- 'X'
    }
    m2_gamma_err <- DATA.values$m2_gamma_err
    if(is.null(m2_gamma_err)){
      m2_gamma_err <- 'X'
    }

    m3_U <- DATA.values$m3_U
    if(is.null(m3_U)){
      m3_U <- 'X'
    }
    m3_U_err <- DATA.values$m3_U_err
    if(is.null(m3_U_err)){
      m3_U_err <- 'X'
    }
    m3_Th <- DATA.values$m3_Th
    if(is.null(m3_Th)){
      m3_Th <- 'X'
    }
    m3_Th_err <- DATA.values$m3_Th_err
    if(is.null(m3_Th_err)){
      m3_Th_err <- 'X'
    }
    m3_K <- DATA.values$m3_K
    if(is.null(m3_K)){
      m3_K <- 'X'
    }
    m3_K_err <- DATA.values$m3_K_err
    if(is.null(m3_K_err)){
      m3_K_err <- 'X'
    }
    m3_Rb <- DATA.values$m3_Rb
    if(is.null(m3_Rb)){
      m3_Rb <- 'X'
    }
    m3_Rb_err <- DATA.values$m3_Rb_err
    if(is.null(m3_Rb_err)){
      m3_Rb_err <- 'X'
    }

    m3_K2Rb <- DATA.values$m3_K2Rb

    m3_alpha <- DATA.values$m3_alpha
    if(is.null(m3_alpha)){
      m3_alpha <- 'X'
    }
    m3_alpha_err <- DATA.values$m3_alpha_err
    if(is.null(m3_alpha_err)){
      m3_alpha_err <- 'X'
    }
    m3_beta <- DATA.values$m3_beta
    if(is.null(m3_beta)){
      m3_beta <- 'X'
    }
    m3_beta_err <- DATA.values$m3_beta_err
    if(is.null(m3_beta_err)){
      m3_beta_err <- 'X'
    }
    m3_gamma <- DATA.values$m3_gamma
    if(is.null(m3_gamma)){
      m3_gamma <- 'X'
    }
    m3_gamma_err <- DATA.values$m3_gamma_err
    if(is.null(m3_gamma_err)){
      m3_gamma_err <- 'X'
    }

    shallowDepth <- DATA.values$shallowDepth

    m1_size_min <- DATA.values$m1_size_min
    m1_size_max <- DATA.values$m1_size_max

    m1_etch_min <- DATA.values$m1_etch_min
    m1_etch_max <- DATA.values$m1_etch_max
    
    m1_aValue <- DATA.values$m1_aValue
    if(is.null(m1_aValue)){
      m1_aValue <- 'X'
    }
    m1_aValue_err <- DATA.values$m1_aValue_err
    if(is.null(m1_aValue_err)){
      m1_aValue_err <- 'X'
    }

    m1_water <- DATA.values$m1_water
    m1_water_err <- DATA.values$m1_water_err

    m1_density <- DATA.values$m1_density
    if(is.null(m1_density)){
      m1_density <- 'X'
    }
    m1_density_err <- DATA.values$m1_density_err
    if(is.null(m1_density_err)){
      m1_density_err <- 'X'
    }

    m2_water <- DATA.values$m2_water
    m2_water_err <- DATA.values$m2_water_err

    m2_density <- DATA.values$m2_density
    if(is.null(m2_density)){
      m2_density <- 'X'
    }
    m2_density_err <- DATA.values$m2_density_err
    if(is.null(m2_density_err)){
      m2_density_err <- 'X'
    }

    m3_water <- DATA.values$m3_water
    m3_water_err <- DATA.values$m3_water_err

    m3_density <- DATA.values$m3_density
    if(is.null(m3_density)){
      m3_density <- 'X'
    }
    m3_density_err <- DATA.values$m3_density_err
    if(is.null(m3_density_err)){
      m3_density_err <- 'X'
    }

    m2_proportion <- DATA.values$m2_proportion
    m2_proportion_err <- DATA.values$m2_proportion_err

    m3_proportion <- DATA.values$m3_proportion
    m3_proportion_err <- DATA.values$m3_proportion_err

    depth <- DATA.values$depth
    if(is.null(depth)){
      depth <- 'X'
    }
    depth_err <- DATA.values$depth_err
    if(is.null(depth_err)){
      depth_err <- 'X'
    }

    fieldChange <- DATA.values$fieldChange

    latitude <- DATA.values$latitude
    longitude <- DATA.values$longitude
    if(is.null(latitude) || is.null(longitude)==0 || directDc){
      latitude <- 'X'
      longitude <- 'X'
    }
    altitude <- DATA.values$altitude
    if(is.null(altitude) || directDc){
      altitude <- 'X'
    }

    dc <- DATA.values$dc
    if(is.null(dc) || !directDc){
      dc <- 'X'
    }
    dc_err <- DATA.values$dc_err
    if(is.null(dc_err) || !directDc){
      dc_err <- 'X'
    }

    if(context == "sediment"){
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
      data$`errExternal K (%)` <- m2_K_err
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

      if(!('X' %in% c(m1_alpha, m1_alpha_err, m1_beta, m1_beta_err))){
        data$`User internal doserate (Gy.ka-1)` <- sum(m1_alpha,m1_beta,na.rm = TRUE)
        data$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(m1_alpha_err^2,m1_beta_err^2,na.rm = TRUE))

      }else if('X' %in% c(m1_alpha, m1_alpha_err)){
        data$`User internal doserate (Gy.ka-1)` <- m1_beta
        data$`errUser internal doserate (Gy.ka-1)` <- m1_beta_err

      }else if('X' %in% c(m1_beta, m1_beta_err)){
        data$`User internal doserate (Gy.ka-1)` <- m1_alpha
        data$`errUser internal doserate (Gy.ka-1)` <- m1_alpha_err

      }else{
        data$`User internal doserate (Gy.ka-1)` <- 'X'
        data$`errUser internal doserate (Gy.ka-1)` <- 'X'
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

        DRAC.int.Dr <- as.numeric(res$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
        DRAC.int.Dr.err <- as.numeric(res$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

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

    }else if(context == "flint"){

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

    }else if(context == "ceramic"){
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

    }else if(context == "cave sediment"){
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
      data$rock$Dr$gamma <- m3_gamma
      data$rock$Dr$gamma.err <- m3_gamma_err

      data$rock$info$water.content <- m3_water
      data$rock$info$water.content.err <- m3_water_err
      data$rock$info$density <- m3_density
      data$rock$info$density.err <- m3_density_err
      data$rock$info$ratio <- m3_proportion
      data$rock$info$ratio.err <- m3_proportion_err



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

    }else if(context == "cave flint"){
      result <- NULL
    }else{
      return(NULL)
    }

    if(!is.null(result)){
      updateNavbarPage(session, "shinyDRAC", "Output")
    }

    return(result)

  })

  ##############################################################################################
  # Output
  ##############################################################################################


  output$outPage <- renderUI({
    fluidRow(column(width = 12,
                    uiOutput("inputTab"),
                    uiOutput("resultTab"))
    )
  })

  output$dracText <- renderUI({

    context <- DATA.values$context

    if(!(context %in% c("sediment", "flint",  "ceramic", "cave sediment"))){
      helpText("This context is still under development.")

    }else{

      dr <- dr_DATA.Dr()

      if(is.null(dr)){
        if(input$dracButton > 0){
          helpText("Missing data")
        }else{
          helpText("Waiting for age calculation")
        }
      }else{
        helpText("age calculated")
      }
    }
  })

  # Results

  output$inputTab <- renderUI({
    fluidRow(column(width = 12,
                    h4("Input table"),
                    DT::dataTableOutput(outputId = "concentrationTable"),
                    checkboxInput(inputId = "concentrationTexBox",label = "LaTeX source",
                                  value = FALSE),
                    verbatimTextOutput(outputId = "concentrationTex")
    ))
  })

  output$resultTab <- renderUI({
    fluidRow(column(width = 12,
                    h4("Result table"),
                    DT::dataTableOutput(outputId = "doseRateTable"),
                    checkboxInput(inputId = "doseRateTexBox",label = "LaTeX source",
                                  value = FALSE),
                    verbatimTextOutput(outputId = "doseRateTex")
    ))
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
    site <- DATA.values$site
    sample <- DATA.values$site

    context <- DATA.values$context
    if(is.null(context)){
      context <- "sediment"
    }

    depth <- as.numeric(DATA.values$depth)
    depth_err <- as.numeric(DATA.values$depth_err)

    m1_U <- as.numeric(DATA.values$m1_U)
    m1_U_err <- as.numeric(DATA.values$m1_U_err)
    m1_Th <- as.numeric(DATA.values$m1_Th)
    m1_Th_err <- as.numeric(DATA.values$m1_Th_err)
    m1_K <- as.numeric(DATA.values$m1_K)
    m1_K_err <- as.numeric(DATA.values$m1_K_err)

    m1_aValue <- as.numeric(DATA.values$m1_aValue)
    m1_aValue_err <- as.numeric(DATA.values$m1_aValue_err)

    m2_U <- as.numeric(DATA.values$m2_U)
    m2_U_err <- as.numeric(DATA.values$m2_U_err)
    m2_Th <- as.numeric(DATA.values$m2_Th)
    m2_Th_err <- as.numeric(DATA.values$m2_Th_err)
    m2_K <- as.numeric(DATA.values$m2_K)
    m2_K_err <- as.numeric(DATA.values$m2_K_err)

    m2_water <- as.numeric(DATA.values$m2_water)
    m2_water_err <- as.numeric(DATA.values$m2_water_err)

    m3_U <- as.numeric(DATA.values$m3_U)
    m3_U_err <- as.numeric(DATA.values$m3_U_err)
    m3_Th <- as.numeric(DATA.values$m3_Th)
    m3_Th_err <- as.numeric(DATA.values$m3_Th_err)
    m3_K <- as.numeric(DATA.values$m3_K)
    m3_K_err <- as.numeric(DATA.values$m3_K_err)

    m3_water <- as.numeric(DATA.values$m3_water)
    m3_water_err <- as.numeric(DATA.values$m3_water_err)

    if(context == "sediment"){
      table <- data.frame(Site = site,
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
            tags$th(rowspan = 2, 'Site'),
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

    }else if(context == "flint"){
      table <- data.frame(Site = site,
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
            tags$th(rowspan = 2, 'Site'),
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

    }else if(context == "ceramic"){
      table <- data.frame(Site = site,
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
            tags$th(rowspan = 2, 'Site'),
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

    }else if(context == "cave sediment"){
      table <- data.frame(Site = site,
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
            tags$th(rowspan = 2, 'Site'),
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
    }else if(context == "cave flint"){
      table <- data.frame(Site = site,
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
            tags$th(rowspan = 2, 'Site'),
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
    site <- DATA.values$site
    sample <- DATA.values$site

    context <- DATA.values$context
    if(is.null(context)){
      context <- "sediment"
    }

    depth <- as.numeric(DATA.values$depth)
    depth_err <- as.numeric(DATA.values$depth_err)

    m1_U <- as.numeric(DATA.values$m1_U)
    m1_U_err <- as.numeric(DATA.values$m1_U_err)
    m1_Th <- as.numeric(DATA.values$m1_Th)
    m1_Th_err <- as.numeric(DATA.values$m1_Th_err)
    m1_K <- as.numeric(DATA.values$m1_K)
    m1_K_err <- as.numeric(DATA.values$m1_K_err)

    m1_aValue <- as.numeric(DATA.values$m1_aValue)
    m1_aValue_err <- as.numeric(DATA.values$m1_aValue_err)

    m2_U <- as.numeric(DATA.values$m2_U)
    m2_U_err <- as.numeric(DATA.values$m2_U_err)
    m2_Th <- as.numeric(DATA.values$m2_Th)
    m2_Th_err <- as.numeric(DATA.values$m2_Th_err)
    m2_K <- as.numeric(DATA.values$m2_K)
    m2_K_err <- as.numeric(DATA.values$m2_K_err)

    m2_water <- as.numeric(DATA.values$m2_water)
    m2_water_err <- as.numeric(DATA.values$m2_water_err)

    m3_U <- as.numeric(DATA.values$m3_U)
    m3_U_err <- as.numeric(DATA.values$m3_U_err)
    m3_Th <- as.numeric(DATA.values$m3_Th)
    m3_Th_err <- as.numeric(DATA.values$m3_Th_err)
    m3_K <- as.numeric(DATA.values$m3_K)
    m3_K_err <- as.numeric(DATA.values$m3_K_err)

    m3_water <- as.numeric(DATA.values$m3_water)
    m3_water_err <- as.numeric(DATA.values$m3_water_err)


    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    if(context == "sediment"){

      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(context =="flint"){
      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Flint} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(context == "ceramic"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Ceramic} & \\multicolumn{4}{c|}{Sediment} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
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

    }else if(context == "cave sediment"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
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

    }else if(context == "cave flint"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{flint} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
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

    context <- DATA.values$context

    site <- DATA.values$site
    sample <- DATA.values$site

    depth <- as.numeric(DATA.values$depth)
    depth_err <- as.numeric(DATA.values$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- numeric()
      alpha.Dr_err <- numeric()
      beta.Dr <- numeric()
      beta.Dr_err <- numeric()
      gamma.Dr <- numeric()
      gamma.Dr_err <- numeric()
      cosmic.Dr <- numeric()
      cosmic.Dr_err <- numeric()

      tot.Dr <- numeric()
      tot.Dr_err <- numeric()

      De <- numeric()
      De_err <- numeric()

      age <- numeric()
      age_err <- numeric()

    }else{
      data <- Dr.values@data

      alpha.Dr <- as.numeric(data$R$alpha.Dr)
      alpha.Dr_err <- as.numeric(data$R$alpha.Dr.err)
      beta.Dr <- as.numeric(data$R$beta.Dr)
      beta.Dr_err <- as.numeric(data$R$beta.Dr.err)
      gamma.Dr <- as.numeric(data$R$gamma.Dr)
      gamma.Dr_err <- as.numeric(data$R$gamma.Dr.err)
      cosmic.Dr <- as.numeric(data$R$cosmic.Dr)
      cosmic.Dr_err <- as.numeric(data$R$cosmic.Dr.err)

      tot.Dr <- as.numeric(data$Dr)
      tot.Dr_err <- as.numeric(data$Dr.err)

      De <- as.numeric(data$R$De)
      De_err <- as.numeric(data$R$De.err)

      age <- as.numeric(data$age)
      age_err <- as.numeric(data$age.err)
    }

    table <- data.frame(site = site,
                        Sample = sample,
                        Depth = paste(depth, "\u00B1", depth_err),
                        Dr = data.frame(alpha = paste(round(alpha.Dr), "\u00B1", round(alpha.Dr_err,3)),
                                        beta = paste(round(beta.Dr,3), "\u00B1", round(beta.Dr_err,3)),
                                        gamma = paste(round(gamma.Dr,3), "\u00B1", round(gamma.Dr_err,3)),
                                        cosmic = paste(round(cosmic.Dr,3), "\u00B1", round(cosmic.Dr_err,3)),
                                        total = paste(round(tot.Dr,3), "\u00B1", round(tot.Dr_err,3)),
                                        De = paste(round(De,3), "\u00B1", round(De_err,3)),
                                        Age = paste(round(age,3), "\u00B1", round(age_err,3))
                                        )
                        )

    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th(rowspan = 2, 'Site'),
          tags$th(rowspan = 2, 'Sample'),
          tags$th(rowspan = 2, 'Depth [m]'),
          tags$th(colspan = 5, 'D\u0309 [Gy/ka]'),
          tags$th(rowspan = 2, 'D\u2091 [Gy]'),
          tags$th(rowspan = 2, 'Age [ka]')
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

    context <- DATA.values$context

    site <- DATA.values$site
    sample <- DATA.values$site

    depth <- as.numeric(DATA.values$depth)
    depth_err <- as.numeric(DATA.values$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- numeric()
      alpha.Dr_err <- numeric()

      beta.Dr <- numeric()
      beta.Dr_err <- numeric()

      gamma.Dr <- numeric()
      gamma.Dr_err <- numeric()

      cosmic.Dr <- numeric()
      cosmic.Dr_err <- numeric()

      tot.Dr <- numeric()
      tot.Dr_err <- numeric()


      De <- numeric()
      De_err <- numeric()

      age <- numeric()
      age_err <- numeric()

    }else{
      data <- Dr.values@data

      alpha.Dr <- as.numeric(data$R$alpha.Dr)
      alpha.Dr_err <- as.numeric(data$R$alpha.Dr.err)

      beta.Dr <- as.numeric(data$R$beta.Dr)
      beta.Dr_err <- as.numeric(data$R$beta.Dr.err)

      gamma.Dr <- as.numeric(data$R$gamma.Dr)
      gamma.Dr_err <- as.numeric(data$R$gamma.Dr.err)

      cosmic.Dr <- as.numeric(data$R$cosmic.Dr)
      cosmic.Dr_err <- as.numeric(data$R$cosmic.Dr.err)

      tot.Dr <- as.numeric(data$Dr)
      tot.Dr_err <- as.numeric(data$Dr.err)

      De <- as.numeric(data$R$De)
      De_err <- as.numeric(data$R$De.err)

      age <- as.numeric(data$age)
      age_err <- as.numeric(data$age.err)
    }

    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    table <- c(table,
               "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}", "\n",
               "\\hline", "\n",
               "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & \\multicolumn{5}{c|}{$\\dot{D}$ [Gy/ka]} & \\multirow{2}{*}{$D_{e}$ [Gy]}& \\multirow{2}{*}{Age [ka]} \\\\", "\n",
               "\\cline{4-8}", "\n",
               "& & & $\\alpha$ [Gy/ka] & $\\beta$ [Gy/ka] & $\\gamma$ [Gy/ka] & $D_c$ [Gy/ka] & Tot. [Gy/ka] & & \\\\", "\n",
               "\\hline", "\n")

    table <- c(table,
               paste(site, "&", sample, "&" ,
                     "$",depth, "\\pm", depth_err, "$", "&",
                     "$",round(alpha.Dr,3) , "\\pm", round(alpha.Dr_err,3), "$", "&",
                     "$",round(beta.Dr,3) , "\\pm", round(beta.Dr_err,3), "$", "&",
                     "$",round(gamma.Dr,3) , "\\pm", round(gamma.Dr_err,3), "$", "&",
                     "$",round(cosmic.Dr,3) , "\\pm", round(cosmic.Dr_err,3), "$", "&",
                     "$",round(tot.Dr,3) , "\\pm", round(tot.Dr_err,3), "$", "&",
                     "$",round(De,3) , "\\pm", round(De_err,3), "$", "&",
                     "$",round(age,3) , "\\pm", round(age_err,3), "$", "\\\\"),
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
  # Help
  ##############################################################################################

  output$helpPage<- renderUI({
    fluidRow(column(width = 12,
                    div(class="helptext",
                        list(
                          h4("Generality"),
                          p("This shiny app rely on different fonction from the R packages 'Luminescence' and 'TLdating'
                                        to produce an estimation of the annual dose rate (D\u0307).
                                        All these functions rely on the web-application DRAC (https://www.aber.ac.uk/en/iges/research-groups/quaternary/luminescence-research-laboratory/dose-rate-calculator/) to produce their results.
                                       "),
                          h4("Input"),
                          p("First, the user have to select the context of the sampling, by default, the app consider that we are dating sediment.
                                       Second, the user have to select the references values that will be used as conversion factor. See the DRAC webpage for more information about the available reference.
                                       Third, the user have to select the kind of measurment he use to determine de different dose rate by ticking the corresponding check boxes.
                                       Forth, the user have to fill in all the visible input box.
                                       Finally the user have to press on the 'Age estimation' button."),
                          h4("Output"),
                          p("Once you press the 'Age estimation' button, a html table containing the results and the mains input is produced.
                                       This table is available in the 'Result' tab.
                                       This tab also contain the TeX code to add these results to a LaTeX manuscript.
                                       This app cannot be use to date multiple sample simultaniously, you have to generate the age of each sample separatly."),
                          h4("Support"),
                          p("This app is currently maintained by David Strebler (david.strebler(at)uni(dash)koeln(dot)de).")
                        ))
                    ))
  })
})
