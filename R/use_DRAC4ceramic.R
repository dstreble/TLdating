#' Calculation of the dose rate for a ceramic sample
#'
#' This function allows to estimating the dose rate for a ceramic which was surrounded by sediment.
#' It call the \link{use_DRAC} and \link{calc_CosmicDoseRate} functions from the R package '\link{Luminescence}'.
#' The function 'use_DRAC' is only compatible wth DRAC version 1.1.
#'
#' @param data
#'  \link{list}: data object create throught the function \link{template_DRAC4ceramic}.
#'
#' @param notification
#'  \link{logical} (default): set to \code{FALSE} if you don't want to see the legal notification.
#'
#' @return
#'  This function return a \linkS4class{TLum.Results} object containing the Age estimation, the dose rates
#'  (total, internal, external, environmental, alpha, beta, gamma and cosmic), the equivalent dose used and their uncertainties.
#'
#' @author David Strebler
#'
#' @export use_DRAC4ceramic


use_DRAC4ceramic <- function (

  data,

  notification = TRUE
){
  ## ------------------------------------------------------------------------------------------

  if(!is(data,"DRAC4ceramic.list")){
    stop("[use_DRAC4ceramic] Error: data has to be create using the 'template_DRAC4ceramic' function.")
  }
  ## ------------------------------------------------------------------------------------------

  ## LEGAL NOTICE ----

  legal.notice <- paste("\t	-------------------- IMPORTANT NOTE ------------------------ \n",
                        "\t This function relies on the functions 'use_DRAC' and 'calc_CosmicDoseRate' \n",
                        "\t from the R package 'Luminescence' to estimate the different dose rate. \n",
                        "\t The function 'use_DRAC' is only compatible with DRAC version 1.1. \n",
                        "\t Before using this function make sure that this is the correct version, otherwise expect unspecified errors.\n",
                        "\t Please ensure you cite the use of DRAC in your work, published or otherwise. Please cite the website name and  \n",
                        "\t version (e.g. DRAC v1.1) and the accompanying journal article:  \n",
                        "\t Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose rate and age calculation for trapped charge  \n",
                        "\t dating. Quaternary Geochronology 28, 54-61. \n",
                        "\t Please ensure you also cite the use of the R package 'Luminescence' (cf. citation('Luminescence')). \n")

  if(notification){
    message(legal.notice)
  }

  comment <- list(legal=legal.notice)

  #######################################################################################################

  # general information

  project <- as.character(data$info$project)
  if(length(project) == 0){
    project <- "Unknown"
  }else{
    project <- gsub(" ", "", project, fixed = TRUE)
  }

  sample <- as.character(data$info$sample)
  if(length(sample) == 0){
    sample <- "Unknown"
  }else{
    sample <- gsub(" ", "", project, fixed = TRUE)
  }

  mineral <- as.character(data$info$mineral)
  if(!(mineral %in% c("F", "Q", "PM"))){
    stop("[use_DRAC4ceramic] Error: data$info$mineral can only be 'Q', 'F' or 'PM'.")
  }

  conversion.factors <- as.character(data$info$conversion.factors)
  if(!(conversion.factors %in% c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013"))){
    stop("[use_DRAC4ceramic] Error: data$info$conversion.factors can only be 'AdamiecAitken1998', 'Guerinetal2011' or 'Liritzisetal2013'.")
  }

  int.alpha.attenuation <- as.character(data$info$alpha.size.attenuation)
  if(!(int.alpha.attenuation %in% c("Bell1980","Brennanetal1991"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$alpha.size.attenuation' can only be 'Bell1980' or 'Brennanetal1991'.")
  }

  int.beta.attenuation <- as.character(data$info$beta.size.attenuation)
  if(!(int.beta.attenuation %in% c("Mejdahl1979","Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.size.attenuation' can only be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F'.")
  }else if(int.beta.attenuation == "Guerinetal2012-Q" && mineral != "Q"){
    warning("[use_DRAC4ceramic]  Warning you use 'Guerinetal2012-Q' for 'data$info$beta.size.attenuation' but material is not 'Q'.")
  }else if(int.beta.attenuation == "Guerinetal2012-F" && mineral != "F"){
    warning("[use_DRAC4ceramic]  Warning you use 'Guerinetal2012-F' for 'data$info$beta.size.attenuation' but material is not 'F'.")
  }

  int.etch.beta.attenuation <- as.character(data$info$beta.etch.attenuation)
  if(!(int.etch.beta.attenuation %in% c("Bell1979","Brennan2003"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.etch.attenuation' can only be 'Bell1979' or 'Brennan2003'.")
  }

  date <- as.numeric(data$info$date)
  if(length(date) == 0){
    date <- as.numeric(format(Sys.Date(),"%Y"))
  }

  ## ------------------------------------------------------------------------------------------

  # Equivalent dose
  De <- data$De$De
  if(is.null(De) || length(De) == 0 || De == "X"){
    De <- "X"
  }else if(is.character(De)){
    De <- gsub(",",".", De, fixed = TRUE)
    De <- as.numeric(De)
  }else if(!is.numeric(De) || !is.finite(De)){
    stop("[use_DRAC4ceramic] Error: 'data$De$De' have to be of type 'numeric' and finite.")
  }

  De.err <- data$De$De.err
  if(is.null(De.err) || length(De.err) == 0 || De.err == "X"){
    De.err <- "X"
  }else if(is.character(De.err)){
    De.err <- gsub(",",".", De.err, fixed = TRUE)
    De.err <- as.numeric(De.err)
  }else if(!is.numeric(De.err) || !is.finite(De.err)){
    stop("[use_DRAC4ceramic] Error: 'data$De$De.err' have to be of type 'numeric' and finite.")
  }
  ## ------------------------------------------------------------------------------------------

  # Internal dose rate
  # By concentration
  int.U <- data$grain$Dr$U
  if(is.null(int.U) || length(int.U) == 0 || int.U == "X"){
    int.U <- "X"
  }else if(is.character(int.U)){
    int.U <- gsub(",",".", int.U, fixed = TRUE)
    int.U <- as.numeric(int.U)
  }else if(!is.numeric(int.U) || !is.finite(int.U)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$U' have to be of type 'numeric' and finite.")
  }

  int.U.err <- data$grain$Dr$U.err
  if(is.null(int.U.err) || length(int.U.err) == 0 || int.U.err == "X"){
    int.U.err <- "X"
  }else if(is.character(int.U.err)){
    int.U.err <- gsub(",",".", int.U.err, fixed = TRUE)
    int.U.err <- as.numeric(int.U.err)
  }else if(!is.numeric(int.U.err) || !is.finite(int.U.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  int.Th <- data$grain$Dr$Th
  if(is.null(int.Th) || length(int.Th) == 0 || int.Th == "X"){
    int.Th <- "X"
  }else if(is.character(int.Th)){
    int.Th <- gsub(",",".", int.Th, fixed = TRUE)
    int.Th <- as.numeric(int.Th)
  }else if(!is.numeric(int.Th) || !is.finite(int.Th)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$Th' have to be of type 'numeric' and finite.")
  }

  int.Th.err <- data$grain$Dr$Th.err
  if(is.null(int.Th.err) || length(int.Th.err) == 0 || int.Th.err == "X"){
    int.Th.err <- "X"
  }else if(is.character(int.Th.err)){
    int.Th.err <- gsub(",",".", int.Th.err, fixed = TRUE)
    int.Th.err <- as.numeric(int.Th.err)
  }else if(!is.numeric(int.Th.err) || !is.finite(int.Th.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  int.K <- data$grain$Dr$K
  if(is.null(int.K) || length(int.K) == 0 || int.K == "X"){
    int.K <- "X"
  }else if(is.character(int.K)){
    int.K <- gsub(",",".", int.K, fixed = TRUE)
    int.K <- as.numeric(int.K)
  }else if(!is.numeric(int.K) || !is.finite(int.K)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$K' have to be of type 'numeric' and finite.")
  }

  int.K.err <- data$grain$Dr$K.err
  if(is.null(int.K.err) || length(int.K.err) == 0 || int.K.err == "X"){
    int.K.err <- "X"
  }else if(is.character(int.K.err)){
    int.K.err <- gsub(",",".", int.K.err, fixed = TRUE)
    int.K.err <- as.numeric(int.K.err)
  }else if(!is.numeric(int.K.err) || !is.finite(int.K.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  int.Rb <- data$grain$Dr$Rb
  if(is.null(int.Rb) || length(int.Rb) == 0 || int.Rb == "X"){
    int.Rb <- 'X'
  }else if(is.character(int.Rb)){
    int.Rb <- gsub(",",".", int.Rb, fixed = TRUE)
    int.Rb <- as.numeric(int.Rb)
  }else if(!is.numeric(int.Rb) || !is.finite(int.Rb)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  int.Rb.err <- data$grain$Dr$Rb.err
  if(is.null(int.Rb.err) || length(int.Rb.err) == 0 || int.Rb.err == "X"){
    int.Rb.err <- 'X'
  }else if(is.character(int.Rb.err)){
    int.Rb.err <- gsub(",",".", int.Rb.err, fixed = TRUE)
    int.Rb.err <- as.numeric(int.Rb.err)
  }else if(!is.numeric(int.Rb.err) || !is.finite(int.Rb.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$Rb' have to be of type 'numeric' and finite.")
  }

  int.K2Rb <- data$grain$Dr$K2Rb
  if(is.null(int.K2Rb) || length(int.K2Rb) == 0){
    if(int.Rb == 'X' || int.Rb.err =='X' ){
      int.K2Rb <- 'Y'
    }else{
      int.K2Rb <- 'N'
    }
  }else if(is.logical(int.K2Rb)){
    if(int.K2Rb){
      int.K2Rb <- 'Y'
    }else{
      int.K2Rb <- 'N'
    }
  }else if(!(int.K2Rb %in% c('Y', 'N')) ){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  int.alpha <- data$grain$Dr$alpha
  if(is.null(int.alpha) || length(int.alpha) == 0 || int.alpha == "X"){
    int.alpha <- 'X'
  }else if(is.character(int.alpha)){
    int.alpha <- gsub(",",".", int.alpha, fixed = TRUE)
    int.alpha <- as.numeric(int.alpha)
  }else if(!is.numeric(int.alpha) || !is.finite(int.alpha)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  int.alpha.err <- data$grain$Dr$alpha.err
  if(is.null(int.alpha.err) || length(int.alpha.err) == 0 || int.alpha.err == "X"){
    int.alpha.err <- 'X'
  }else if(is.character(int.alpha.err)){
    int.alpha.err <- gsub(",",".", int.alpha.err, fixed = TRUE)
    int.alpha.err <- as.numeric(int.alpha.err)
  }else if(!is.numeric(int.alpha.err) || !is.finite(int.alpha.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  int.beta <- data$grain$Dr$beta
  if(is.null(int.beta) || length(int.beta) == 0 || int.beta == "X"){
    int.beta <- 'X'
  }else if(is.character(int.beta)){
    int.beta <- gsub(",",".", int.beta, fixed = TRUE)
    int.beta <- as.numeric(int.beta)
  }else if(!is.numeric(int.beta) || !is.finite(int.beta)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$beta' have to be of type 'numeric' and finite.")
  }

  int.beta.err <- data$grain$Dr$beta.err
  if(is.null(int.beta.err) || length(int.beta.err) == 0 || int.beta.err == "X"){
    int.beta.err <- 'X'
  }else if(is.character(int.beta.err)){
    int.beta.err <- gsub(",",".", int.beta.err, fixed = TRUE)
    int.beta.err <- as.numeric(int.beta.err)
  }else if(!is.numeric(int.beta.err) || !is.finite(int.beta.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  int.gamma <- data$grain$Dr$gamma
  if(is.null(int.gamma) || length(int.gamma) == 0 || int.gamma == "X"){
    int.gamma <- 'X'
  }else if(is.character(int.gamma)){
    int.gamma <- gsub(",",".", int.gamma, fixed = TRUE)
    int.gamma <- as.numeric(int.gamma)
  }else if(!is.numeric(int.gamma) || !is.finite(int.gamma)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  int.gamma.err <- data$grain$Dr$gamma.err
  if(is.null(int.gamma.err) || length(int.gamma.err) == 0 || int.gamma.err == "X"){
    int.gamma.err <- 'X'
  }else if(is.character(int.gamma.err)){
    int.gamma.err <- gsub(",",".", int.gamma.err, fixed = TRUE)
    int.gamma.err <- as.numeric(int.gamma.err)
  }else if(!is.numeric(int.gamma.err) || !is.finite(int.gamma.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$gamma.err' have to be of type 'numeric' and finite.")
  }

  ## ------------------------------------------------------------------------------------------

  # External dose rate
  # By concentration
  ext.U <- data$ceramic$Dr$U
  if(is.null(ext.U) || length(ext.U) == 0 || ext.U == "X"){
    ext.U <- "X"
  }else if(is.character(ext.U)){
    ext.U <- gsub(",",".", ext.U, fixed = TRUE)
    ext.U <- as.numeric(ext.U)
  }else if(!is.numeric(ext.U) || !is.finite(ext.U)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$U' have to be of type 'numeric' and finite.")
  }

  ext.U.err <- data$ceramic$Dr$U.err
  if(is.null(ext.U.err) || length(ext.U.err) == 0 || ext.U.err == "X"){
    ext.U.err <- "X"
  }else if(is.character(ext.U.err)){
    ext.U.err <- gsub(",",".", ext.U.err, fixed = TRUE)
    ext.U.err <- as.numeric(ext.U.err)
  }else if(!is.numeric(ext.U.err) || !is.finite(ext.U.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  ext.Th <- data$ceramic$Dr$Th
  if(is.null(ext.Th) || length(ext.Th) == 0 || ext.Th == "X"){
    ext.Th <- "X"
  }else if(is.character(ext.Th)){
    ext.Th <- gsub(",",".", ext.Th, fixed = TRUE)
    ext.Th <- as.numeric(ext.Th)
  }else if(!is.numeric(ext.Th) || !is.finite(ext.Th)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$Th' have to be of type 'numeric' and finite.")
  }

  ext.Th.err <- data$ceramic$Dr$Th.err
  if(is.null(ext.Th.err) || length(ext.Th.err) == 0 || ext.Th.err == "X"){
    ext.Th.err <- "X"
  }else if(is.character(ext.Th.err)){
    ext.Th.err <- gsub(",",".", ext.Th.err, fixed = TRUE)
    ext.Th.err <- as.numeric(ext.Th.err)
  }else if(!is.numeric(ext.Th.err) || !is.finite(ext.Th.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  ext.K <- data$ceramic$Dr$K
  if(is.null(ext.K) || length(ext.K) == 0 || ext.K == "X"){
    ext.K <- "X"
  }else if(is.character(ext.K)){
    ext.K <- gsub(",",".", ext.K, fixed = TRUE)
    ext.K <- as.numeric(ext.K)
  }else if(!is.numeric(ext.K) || !is.finite(ext.K)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$K' have to be of type 'numeric' and finite.")
  }

  ext.K.err <- data$ceramic$Dr$K.err
  if(is.null(ext.K.err) || length(ext.K.err) == 0 || ext.K.err == "X"){
    ext.K.err <- "X"
  }else if(is.character(ext.K.err)){
    ext.K.err <- gsub(",",".", ext.K.err, fixed = TRUE)
    ext.K.err <- as.numeric(ext.K.err)
  }else if(!is.numeric(ext.K.err) || !is.finite(ext.K.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  ext.Rb <- data$ceramic$Dr$Rb
  if(is.null(ext.Rb) || length(ext.Rb) == 0 || ext.Rb == "X"){
    ext.Rb <- 'X'
  }else if(is.character(ext.Rb)){
    ext.Rb <- gsub(",",".", ext.Rb, fixed = TRUE)
    ext.Rb <- as.numeric(ext.Rb)
  }else if(!is.numeric(ext.Rb) || !is.finite(ext.Rb)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  ext.Rb.err <- data$ceramic$Dr$Rb.err
  if(is.null(ext.Rb.err) || length(ext.Rb.err) == 0 || ext.Rb.err == "X"){
    ext.Rb.err <- 'X'
  }else if(is.character(ext.Rb.err)){
    ext.Rb.err <- gsub(",",".", ext.Rb.err, fixed = TRUE)
    ext.Rb.err <- as.numeric(ext.Rb.err)
  }else if(!is.numeric(ext.Rb.err) || !is.finite(ext.Rb.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$Rb' have to be of type 'numeric' and finite.")
  }

  ext.K2Rb <- data$ceramic$Dr$K2Rb
  if(is.null(ext.K2Rb) || length(ext.K2Rb) == 0){
    if(ext.Rb == 'X' || ext.Rb.err =='X' ){
      ext.K2Rb <- 'Y'
    }else{
      ext.K2Rb <- 'N'
    }
  }else if(is.logical(ext.K2Rb)){
    if(ext.K2Rb){
      ext.K2Rb <- 'Y'
    }else{
      ext.K2Rb <- 'N'
    }
  }else if(!(ext.K2Rb %in% c('Y', 'N')) ){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  ext.alpha <- data$ceramic$Dr$alpha
  if(is.null(ext.alpha) || length(ext.alpha) == 0 || ext.alpha == "X"){
    ext.alpha <- 'X'
  }else if(is.character(ext.alpha)){
    ext.alpha <- gsub(",",".", ext.alpha, fixed = TRUE)
    ext.alpha <- as.numeric(ext.alpha)
  }else if(!is.numeric(ext.alpha) || !is.finite(ext.alpha)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  ext.alpha.err <- data$ceramic$Dr$alpha.err
  if(is.null(ext.alpha.err) || length(ext.alpha.err) == 0 || ext.alpha.err == "X"){
    ext.alpha.err <- 'X'
  }else if(is.character(ext.alpha.err)){
    ext.alpha.err <- gsub(",",".", ext.alpha.err, fixed = TRUE)
    ext.alpha.err <- as.numeric(ext.alpha.err)
  }else if(!is.numeric(ext.alpha.err) || !is.finite(ext.alpha.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  ext.beta <- data$ceramic$Dr$beta
  if(is.null(ext.beta) || length(ext.beta) == 0 || ext.beta == "X"){
    ext.beta <- 'X'
  }else if(is.character(ext.beta)){
    ext.beta <- gsub(",",".", ext.beta, fixed = TRUE)
    ext.beta <- as.numeric(ext.beta)
  }else if(!is.numeric(ext.beta) || !is.finite(ext.beta)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$beta' have to be of type 'numeric' and finite.")
  }

  ext.beta.err <- data$ceramic$Dr$beta.err
  if(is.null(ext.beta.err) || length(ext.beta.err) == 0 || ext.beta.err == "X"){
    ext.beta.err <- 'X'
  }else if(is.character(ext.beta.err)){
    ext.beta.err <- gsub(",",".", ext.beta.err, fixed = TRUE)
    ext.beta.err <- as.numeric(ext.beta.err)
  }else if(!is.numeric(ext.beta.err) || !is.finite(ext.beta.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  ext.gamma <- data$ceramic$Dr$gamma
  if(is.null(ext.gamma) || length(ext.gamma) == 0 || ext.gamma == "X"){
    ext.gamma <- 'X'
  }else if(is.character(ext.gamma)){
    ext.gamma <- gsub(",",".", ext.gamma, fixed = TRUE)
    ext.gamma <- as.numeric(ext.gamma)
  }else if(!is.numeric(ext.gamma) || !is.finite(ext.gamma)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  ext.gamma.err <- data$ceramic$Dr$gamma.err
  if(is.null(ext.gamma.err) || length(ext.gamma.err) == 0 || ext.gamma.err == "X"){
    ext.gamma.err <- 'X'
  }else if(is.character(ext.gamma.err)){
    ext.gamma.err <- gsub(",",".", ext.gamma.err, fixed = TRUE)
    ext.gamma.err <- as.numeric(ext.gamma.err)
  }else if(!is.numeric(ext.gamma.err) || !is.finite(ext.gamma.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$gamma.err' have to be of type 'numeric' and finite.")
  }


  ## ------------------------------------------------------------------------------------------

  # Environmental dose rate
  # By concentration
  env.U <- data$sediment$Dr$U
  if(is.null(env.U) || length(env.U) == 0 || env.U == "X"){
    env.U <- "X"
  }else if(is.character(env.U)){
    env.U <- gsub(",",".", env.U, fixed = TRUE)
    env.U <- as.numeric(env.U)
  }else if(!is.numeric(env.U) || !is.finite(env.U)){
    stop("[use_DRAC4sediment] Error: 'data$sediment$Dr$U' have to be of type 'numeric' and finite.")
  }

  env.U.err <- data$sediment$Dr$U.err
  if(is.null(env.U.err) || length(env.U.err) == 0 || env.U.err == "X"){
    env.U.err <- "X"
  }else if(is.character(env.U.err)){
    env.U.err <- gsub(",",".", env.U.err, fixed = TRUE)
    env.U.err <- as.numeric(env.U.err)
  }else if(!is.numeric(env.U.err) || !is.finite(env.U.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  env.Th <- data$sediment$Dr$Th
  if(is.null(env.Th) || length(env.Th) == 0 || env.Th == "X"){
    env.Th <- "X"
  }else if(is.character(env.Th)){
    env.Th <- gsub(",",".", env.Th, fixed = TRUE)
    env.Th <- as.numeric(env.Th)
  }else if(!is.numeric(env.Th) || !is.finite(env.Th)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$Th' have to be of type 'numeric' and finite.")
  }

  env.Th.err <- data$sediment$Dr$Th.err
  if(is.null(env.Th.err) || length(env.Th.err) == 0 || env.Th.err == "X"){
    env.Th.err <- "X"
  }else if(is.character(env.Th.err)){
    env.Th.err <- gsub(",",".", env.Th.err, fixed = TRUE)
    env.Th.err <- as.numeric(env.Th.err)
  }else if(!is.numeric(env.Th.err) || !is.finite(env.Th.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  env.K <- data$sediment$Dr$K
  if(is.null(env.K) || length(env.K) == 0 || env.K == "X"){
    env.K <- "X"
  }else if(is.character(env.K)){
    env.K <- gsub(",",".", env.K, fixed = TRUE)
    env.K <- as.numeric(env.K)
  }else if(!is.numeric(env.K) || !is.finite(env.K)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$K' have to be of type 'numeric' and finite.")
  }

  env.K.err <- data$sediment$Dr$K.err
  if(is.null(env.K.err) || length(env.K.err) == 0 || env.K.err == "X"){
    env.K.err <- "X"
  }else if(is.character(env.K.err)){
    env.K.err <- gsub(",",".", env.K.err, fixed = TRUE)
    env.K.err <- as.numeric(env.K.err)
  }else if(!is.numeric(env.K.err) || !is.finite(env.K.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  env.Rb <- data$sediment$Dr$Rb
  if(is.null(env.Rb) || length(env.Rb) == 0 || env.Rb == "X"){
    env.Rb <- 'X'
  }else if(is.character(env.Rb)){
    env.Rb <- gsub(",",".", env.Rb, fixed = TRUE)
    env.Rb <- as.numeric(env.Rb)
  }else if(!is.numeric(env.Rb) || !is.finite(env.Rb)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  env.Rb.err <- data$sediment$Dr$Rb.err
  if(is.null(env.Rb.err) || length(env.Rb.err) == 0 || env.Rb.err == "X"){
    env.Rb.err <- 'X'
  }else if(is.character(env.Rb.err)){
    env.Rb.err <- gsub(",",".", env.Rb.err, fixed = TRUE)
    env.Rb.err <- as.numeric(env.Rb.err)
  }else if(!is.numeric(env.Rb.err) || !is.finite(env.Rb.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$Rb' have to be of type 'numeric' and finite.")
  }

  env.K2Rb <- data$sediment$Dr$K2Rb
  if(is.null(env.K2Rb) || length(env.K2Rb) == 0){
    if(env.Rb == 'X' || env.Rb.err =='X' ){
      env.K2Rb <- 'Y'
    }else{
      env.K2Rb <- 'N'
    }
  }else if(is.logical(env.K2Rb)){
    if(env.K2Rb){
      env.K2Rb <- 'Y'
    }else{
      env.K2Rb <- 'N'
    }
  }else if(!(env.K2Rb %in% c('Y', 'N')) ){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  env.alpha <- data$sediment$Dr$alpha
  if(is.null(env.alpha) || length(env.alpha) == 0 || env.alpha == "X"){
    env.alpha <- 'X'
  }else if(is.character(env.alpha)){
    env.alpha <- gsub(",",".", env.alpha, fixed = TRUE)
    env.alpha <- as.numeric(env.alpha)
  }else if(!is.numeric(env.alpha) || !is.finite(env.alpha)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  env.alpha.err <- data$sediment$Dr$alpha.err
  if(is.null(env.alpha.err) || length(env.alpha.err) == 0 || env.alpha.err == "X"){
    env.alpha.err <- 'X'
  }else if(is.character(env.alpha.err)){
    env.alpha.err <- gsub(",",".", env.alpha.err, fixed = TRUE)
    env.alpha.err <- as.numeric(env.alpha.err)
  }else if(!is.numeric(env.alpha.err) || !is.finite(env.alpha.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  env.beta <- data$sediment$Dr$beta
  if(is.null(env.beta) || length(env.beta) == 0 || env.beta == "X"){
    env.beta <- 'X'
  }else if(is.character(env.beta)){
    env.beta <- gsub(",",".", env.beta, fixed = TRUE)
    env.beta <- as.numeric(env.beta)
  }else if(!is.numeric(env.beta) || !is.finite(env.beta)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$beta' have to be of type 'numeric' and finite.")
  }

  env.beta.err <- data$sediment$Dr$beta.err
  if(is.null(env.beta.err) || length(env.beta.err) == 0 || env.beta.err == "X"){
    env.beta.err <- 'X'
  }else if(is.character(env.beta.err)){
    env.beta.err <- gsub(",",".", env.beta.err, fixed = TRUE)
    env.beta.err <- as.numeric(env.beta.err)
  }else if(!is.numeric(env.beta.err) || !is.finite(env.beta.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  env.gamma <- data$sediment$Dr$gamma
  if(is.null(env.gamma) || length(env.gamma) == 0 || env.gamma == "X"){
    env.gamma <- 'X'
  }else if(is.character(env.gamma)){
    env.gamma <- gsub(",",".", env.gamma, fixed = TRUE)
    env.gamma <- as.numeric(env.gamma)
  }else if(!is.numeric(env.gamma) || !is.finite(env.gamma)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  env.gamma.err <- data$sediment$Dr$gamma.err
  if(is.null(env.gamma.err) || length(env.gamma.err) == 0 || env.gamma.err == "X"){
    env.gamma.err <- 'X'
  }else if(is.character(env.gamma.err)){
    env.gamma.err <- gsub(",",".", env.gamma.err, fixed = TRUE)
    env.gamma.err <- as.numeric(env.gamma.err)
  }else if(!is.numeric(env.gamma.err) || !is.finite(env.gamma.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$gamma.err' have to be of type 'numeric' and finite.")
  }

  ## ------------------------------------------------------------------------------------------

  # grain information

  # Internal information
  int.size.min <- data$grain$info$grain.size.min
  if(is.null(int.size.min) || length(int.size.min) == 0 || int.size.min == "X"){
    int.size.min <- 1
  }else if(is.character(int.size.min)){
    int.size.min <- gsub(",",".", int.size.min, fixed = TRUE)
    int.size.min <- as.numeric(int.size.min)
  }else if(!is.numeric(int.size.min) || !is.finite(int.size.min)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' have to be of type 'numeric' and finite.")
  }
  if(int.size.min < 1){
    int.size.min <- 1
  }

  int.size.max <- data$grain$info$grain.size.max
  if(is.null(int.size.max) || length(int.size.max) == 0 || int.size.max == "X"){
    int.size.min <- 1000
  }else if(is.character(int.size.max)){
    int.size.max <- gsub(",",".", int.size.max, fixed = TRUE)
    int.size.max <- as.numeric(int.size.max)
  }else if(!is.numeric(int.size.max) || !is.finite(int.size.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.max' have to be of type 'numeric' and finite.")
  }
  if(int.size.max > 1000){
    int.size.max <- 1000
  }
  if(int.size.min > int.size.max){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$size.min' > 'data$grain$info$int.size.max'")
  }

  int.etch.min <- data$grain$info$grain.etch.min
  if(is.null(int.etch.min) || length(int.etch.min) == 0 || int.etch.min == "X"){
    int.etch.min <- 0
  }else if(is.character(int.etch.min)){
    int.etch.min <- gsub(",",".", int.etch.min, fixed = TRUE)
    int.etch.min <- as.numeric(int.etch.min)
  }else if(!is.numeric(int.etch.min) || !is.finite(int.etch.min)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' have to be of type 'numeric' and finite.")
  }
  if(int.size.min < 0){
    int.size.min <- 0
  }

  int.etch.max <- data$grain$info$grain.etch.max
  if(is.null(int.etch.max) || length(int.etch.max) == 0 || int.etch.max == "X"){
    int.etch.max <- 30
  }else if(is.character(int.etch.max)){
    int.etch.max <- gsub(",",".", int.etch.max, fixed = TRUE)
    int.etch.max <- as.numeric(int.etch.max)
  }else if(!is.numeric(int.etch.max) || !is.finite(int.etch.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.max' have to be of type 'numeric' and finite.")
  }
  if(int.etch.max > 30){
    int.etch.max <- 30
  }
  if(int.etch.min > int.etch.max){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' > 'data$grain$info$grain.etch.max'")
  }

  int.a.value <- data$grain$info$a.value
  if(is.null(int.a.value) || length(int.a.value) == 0 || int.a.value == "X"){
    int.a.value <- 'X'
  }else if(is.character(int.a.value)){
    int.a.value <- gsub(",",".", int.a.value, fixed = TRUE)
    int.a.value <- as.numeric(int.a.value)
  }else if(!is.numeric(int.a.value) || !is.finite(int.a.value)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value' have to be of type 'numeric' and finite.")
  }
  int.a.value.err <- data$grain$info$a.value.err
  if(is.null(int.a.value.err) || length(int.a.value.err) == 0 || int.a.value.err == "X"){
    int.a.value.err <- 'X'
  }else if(is.character(int.a.value.err)){
    int.a.value.err <- gsub(",",".", int.a.value.err, fixed = TRUE)
    int.a.value.err <- as.numeric(int.a.value.err)
  }else if(!is.numeric(int.a.value.err) || !is.finite(int.a.value.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value.err' have to be of type 'numeric' and finite.")
  }
  ## ------------------------------------------------------------------------------------------

  # 'External' information
  max.etch <- 30

  ext.etch.min <- max.etch
  ext.etch.max <- max.etch

  ext.water <- data$ceramic$info$water.content
  if(is.null(ext.water) || length(ext.water) == 0 || ext.water == "X"){
    ext.water <- 5
  }else if(is.character(ext.water)){
    ext.water <- gsub(",",".", ext.water, fixed = TRUE)
    ext.water <- as.numeric(ext.water)
  }else if(!is.numeric(ext.water) || !is.finite(ext.water)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content' have to be of type 'numeric' and finite.")
  }

  ext.water.err <- data$ceramic$info$water.content.err
  if(is.null(ext.water.err) || length(ext.water.err) == 0 || ext.water.err == "X"){
    ext.water.err <- 2
  }else if(is.character(ext.water.err)){
    ext.water.err <- gsub(",",".", ext.water.err, fixed = TRUE)
    ext.water.err <- as.numeric(ext.water.err)
  }else if(!is.numeric(ext.water.err) || !is.finite(ext.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content.err' have to be of type 'numeric' and finite.")
  }

  ext.density <- data$ceramic$info$density
  if(is.null(ext.density) || length(ext.density) == 0 || ext.density == "X"){
    ext.density <- 1.8
  }else if(is.character(ext.density)){
    ext.density <- gsub(",",".", ext.density, fixed = TRUE)
    ext.density <- as.numeric(ext.density)
  }else if(!is.numeric(ext.density) || !is.finite(ext.density)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density' have to be of type 'numeric' and finite.")
  }

  ext.density.err <- data$ceramic$info$density.err
  if(is.null(ext.density.err) || length(ext.density.err) == 0 || ext.density.err == "X"){
    ext.density.err <- 0.1
  }else if(is.character(ext.density.err)){
    ext.density.err <- gsub(",",".", ext.density.err, fixed = TRUE)
    ext.density.err <- as.numeric(ext.density.err)
  }else if(!is.numeric(ext.density.err) || !is.finite(ext.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density.err' have to be of type 'numeric' and finite.")
  }

  ## ------------------------------------------------------------------------------------------

  # Sediment information

  # Env information
  env.water <- data$sediment$info$water.content
  if(is.null(env.water) || length(env.water) == 0 || env.water == "X"){
    env.water <- 5
  }else if(is.character(env.water)){
    env.water <- gsub(",",".", env.water, fixed = TRUE)
    env.water <- as.numeric(env.water)
  }else if(!is.numeric(env.water) || !is.finite(env.water)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content' have to be of type 'numeric' and finite.")
  }

  env.water.err <- data$sediment$info$water.content.err
  if(is.null(env.water.err) || length(env.water.err) == 0 || env.water.err == "X"){
    env.water.err <- 2
  }else if(is.character(env.water.err)){
    env.water.err <- gsub(",",".", env.water.err, fixed = TRUE)
    env.water.err <- as.numeric(env.water.err)
  }else if(!is.numeric(env.water.err) || !is.finite(env.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content.err' have to be of type 'numeric' and finite.")
  }

  env.density <- data$sediment$info$density
  if(is.null(env.density) || length(env.density) == 0 || env.density == "X"){
    env.density <- 1.8
  }else if(is.character(env.density)){
    env.density <- gsub(",",".", env.density, fixed = TRUE)
    env.density <- as.numeric(env.density)
  }else if(!is.numeric(env.density) || !is.finite(env.density)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density' have to be of type 'numeric' and finite.")
  }

  env.density.err <- data$sediment$info$density.err
  if(is.null(env.density.err) || length(env.density.err) == 0 || env.density.err == "X"){
    env.density.err <- 1.8
  }else if(is.character(env.density.err)){
    env.density.err <- gsub(",",".", env.density.err, fixed = TRUE)
    env.density.err <- as.numeric(env.density.err)
  }else if(!is.numeric(env.density.err) || !is.finite(env.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density.err' have to be of type 'numeric' and finite.")
  }

  env.scale4shallow.depth <- data$sediment$info$scale4shallow.depth
  if(is.null(env.scale4shallow.depth) || length(env.scale4shallow.depth) == 0){
    env.scale4shallow.depth <- 'Y'
  }else if(is.logical(env.scale4shallow.depth)){
    if(env.scale4shallow.depth){
      env.scale4shallow.depth <- 'Y'
    }else{
      env.scale4shallow.depth <- 'N'
    }
  }else if(!(env.scale4shallow.depth %in% c('Y', 'N')) ){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$scale4shallow.depth' have to be 'Y' or 'N'.")
  }
  ## ------------------------------------------------------------------------------------------

  # Cosmic dose rate

  #Theoretical

  depth <- data$cosmic$depth
  if(is.null(depth) || length(depth) == 0 || depth == "X"){
    depth <- 'X'
  }else if(is.character(depth)){
    depth <- gsub(",",".", depth, fixed = TRUE)
    depth <- as.numeric(depth)
  }else if(!is.numeric(depth) || !is.finite(depth)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$depth' have to be of type 'numeric' and finite.")
  }

  depth.err <- data$cosmic$depth.err
  if(is.null(depth.err) || length(depth.err) == 0 || depth.err == "X"){
    depth.err <- 'X'
  }else if(is.character(depth.err)){
    depth.err <- gsub(",",".", depth.err, fixed = TRUE)
    depth.err <- as.numeric(depth.err)
  }else if(!is.numeric(depth.err) || !is.finite(depth.err)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$depth.err' have to be of type 'numeric' and finite.")
  }

  latitude <- data$cosmic$latitude
  if(is.null(latitude) || length(latitude) == 0 || latitude == "X"){
    latitude <- 'X'
  }else if(is.character(latitude)){
    latitude <- gsub(",",".", latitude, fixed = TRUE)
    latitude <- as.numeric(latitude)
  }else if(!is.numeric(latitude) || !is.finite(latitude)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$latitude' have to be of type 'numeric' and finite.")
  }

  longitude <- data$cosmic$longitude
  if(is.null(longitude) || length(longitude) == 0 || longitude == "X"){
    longitude <- 'X'
  }else if(is.character(longitude)){
    longitude <- gsub(",",".", longitude, fixed = TRUE)
    longitude <- as.numeric(longitude)
  }else if(!is.numeric(longitude) || !is.finite(longitude)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$longitude' have to be of type 'numeric' and finite.")
  }

  altitude <- data$cosmic$altitude
  if(is.null(altitude) || length(altitude) == 0 || altitude == "X"){
    altitude <- 'X'
  }else if(is.character(altitude)){
    altitude <- gsub(",",".", altitude, fixed = TRUE)
    altitude <- as.numeric(altitude)
  }else if(!is.numeric(altitude) || !is.finite(altitude)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$altitude' have to be of type 'numeric' and finite.")
  }

  #Direct measurement
  cosmic <- data$cosmic$Dr
  if(is.null(cosmic) || length(cosmic) == 0 || cosmic == "X"){
    cosmic <- 'X'
  }else if(is.character(cosmic)){
    cosmic <- gsub(",",".", cosmic, fixed = TRUE)
    cosmic <- as.numeric(cosmic)
  }else if(!is.numeric(cosmic) || !is.finite(cosmic)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$Dr' have to be of type 'numeric' and finite.")
  }

  cosmic.err <- data$cosmic$Dr.err
  if(is.null(cosmic.err) || length(cosmic.err) == 0 || cosmic.err == "X"){
    cosmic.err <- 'X'
  }else if(is.character(cosmic.err)){
    cosmic.err <- gsub(",",".", cosmic.err, fixed = TRUE)
    cosmic.err <- as.numeric(cosmic.err)
  }else if(!is.numeric(cosmic.err) || !is.finite(cosmic.err)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$Dr.err' have to be of type 'numeric' and finite.")
  }


  # For calc_CosmicDoseRate
  corr.fieldChanges <- data$cosmic$corr.fieldChanges
  if(is.null(corr.fieldChanges) || length(corr.fieldChanges) == 0){
    corr.fieldChanges <- 'Y'
  }else if(is.logical(corr.fieldChanges)){
    if(corr.fieldChanges){
      corr.fieldChanges <- 'Y'
    }else{
      corr.fieldChanges <- 'N'
    }
  }else if(!(corr.fieldChanges %in% c('Y', 'N')) ){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$corr.fieldChanges' have to be 'Y' or 'N'.")
  }

  if(cosmic == 'X'){
    directDc <- FALSE
    cosmic <- 'X'
    cosmic.err <- 'X'
  }else if(latitude == 'X' || longitude == 'X'){
    directDc <- TRUE
    latitude <- 'X'
    longitude <- 'X'
    altitude <- 'X'
  }

  ##########################################################################################################

  ## ------------------------------------------------------------------------------------------
  # First run, Internal & External dose (grain)

  int.input <- template_DRAC(notification = notification)

  int.input$`Project ID` <- project
  int.input$`Sample ID` <- sample
  int.input$Mineral <- mineral
  int.input$`Conversion factors` <- conversion.factors

  int.input$`Internal U (ppm)` <-  int.U
  int.input$`errInternal U (ppm)` <- int.U.err
  int.input$`Internal Th (ppm)` <- int.Th
  int.input$`errInternal Th (ppm)` <- int.Th.err
  int.input$`Internal K (%)` <- int.K
  int.input$`errInternal K (%)` <- int.K.err
  int.input$`Rb (ppm)` <- int.Rb
  int.input$`errRb (ppm)` <- int.Rb.err
  int.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  #####
  if(is.finite(int.alpha) && is.finite(int.beta)){
    int.input$`User internal doserate (Gy.ka-1)` <- int.alpha + int.beta
  }else if(is.finite(int.alpha)){
    int.input$`User internal doserate (Gy.ka-1)` <- int.alpha
  }else if(is.finite(int.beta)){
    int.input$`User internal doserate (Gy.ka-1)` <- int.beta
  }else{
    int.input$`User internal doserate (Gy.ka-1)` <- 'X'
  }

  if(is.finite(int.alpha.err) && is.finite(int.beta.err)){
    int.input$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(int.alpha^2, int.beta^2, na.rm = TRUE))
  }else if(is.finite(int.alpha)){
    int.input$`errUser internal doserate (Gy.ka-1)` <- int.alpha.err
  }else if(is.finite(int.beta)){
    int.input$`errUser internal doserate (Gy.ka-1)` <- int.beta.err
  }else{
    int.input$`errUser internal doserate (Gy.ka-1)` <- 'X'
  }
  ######


  int.input$`ExternalU (ppm)` <-  ext.U
  int.input$`errExternal U (ppm)` <- ext.U.err
  int.input$`External Th (ppm)` <- ext.Th
  int.input$`errExternal Th (ppm)` <- ext.Th.err
  int.input$`External K (%)` <- ext.K
  int.input$`errExternal K (%)` <- ext.K.err
  int.input$`External Rb (ppm)` <- ext.Rb
  int.input$`errExternal Rb (ppm)` <- ext.Rb.err
  int.input$`Calculate external Rb from K conc?` <- ext.K2Rb

  #####
  int.input$`User external alphadoserate (Gy.ka-1)` <- ext.alpha
  int.input$`errUser external alphadoserate (Gy.ka-1)` <- ext.alpha.err
  int.input$`User external betadoserate (Gy.ka-1)` <- ext.beta
  int.input$`errUser external betadoserate (Gy.ka-1)` <- ext.beta.err
  int.input$`User external gamma doserate (Gy.ka-1)` <- ext.gamma
  int.input$`errUser external gammadoserate (Gy.ka-1)` <- ext.gamma.err
  #####

  int.input$`Grain size min (microns)` <- int.size.min
  int.input$`Grain size max (microns)` <- int.size.max

  int.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  int.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  int.input$`Etch depth min (microns)` <- int.etch.min
  int.input$`Etch depth max (microns)` <- int.etch.max

  int.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  int.input$`a-value` <- int.a.value
  int.input$`erra-value` <- int.a.value.err

  int.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext.water
  int.input$`errWater content %` <- ext.water.err

  int.input$`Depth (m)` <- depth
  int.input$`errDepth (m)` <- depth.err

  int.input$`Overburden density (g cm-3)` <- env.density
  int.input$`errOverburden density (g cm-3)` <- env.density.err

  int.input$`Latitude (decimal degrees)` <- latitude
  int.input$`Longitude (decimal degrees)` <- longitude
  int.input$`Altitude (m)` <- altitude

  #####
  int.input$`User cosmicdoserate (Gy.ka-1)` <- cosmic
  int.input$`errUser cosmicdoserate (Gy.ka-1)` <- cosmic.err
  #####

  int.input$`De (Gy)` <- De
  int.input$`errDe (Gy)` <- De.err

  int.output <- use_DRAC(int.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # Second run, Evironmental dose (sediment)

  env.input <- template_DRAC(notification = FALSE)

  env.input$`Project ID` <- project
  env.input$`Sample ID` <- sample
  env.input$Mineral <- mineral
  env.input$`Conversion factors` <- conversion.factors

  env.input$`ExternalU (ppm)` <-  env.U
  env.input$`errExternal U (ppm)` <- env.U.err
  env.input$`External Th (ppm)` <- env.Th
  env.input$`errExternal Th (ppm)` <- env.Th.err
  env.input$`External K (%)` <- env.K
  env.input$`errExternal K (%)` <- env.K.err
  env.input$`External Rb (ppm)` <- env.Rb
  env.input$`errExternal Rb (ppm)` <- env.Rb.err
  env.input$`Calculate external Rb from K conc?` <- env.K2Rb

  ####
  env.input$`User external alphadoserate (Gy.ka-1)` <- env.alpha
  env.input$`errUser external alphadoserate (Gy.ka-1)` <- env.alpha.err
  env.input$`User external betadoserate (Gy.ka-1)` <- env.beta
  env.input$`errUser external betadoserate (Gy.ka-1)` <- env.beta.err
  env.input$`User external gamma doserate (Gy.ka-1)` <- env.gamma
  env.input$`errUser external gammadoserate (Gy.ka-1)` <- env.gamma.err
  ####

  env.input$`Scale gammadoserate at shallow depths?` <- env.scale4shallow.depth

  env.input$`Grain size min (microns)` <- int.size.min
  env.input$`Grain size max (microns)` <- int.size.max

  env.input$`a-value` <- int.a.value
  env.input$`erra-value` <- int.a.value.err

  env.input$`Water content ((wet weight - dry weight)/dry weight) %` <- env.water
  env.input$`errWater content %` <- env.water.err

  env.input$`Depth (m)` <- depth
  env.input$`errDepth (m)` <- depth.err
  env.input$`Overburden density (g cm-3)` <- env.density
  env.input$`errOverburden density (g cm-3)` <- env.density.err
  env.input$`Latitude (decimal degrees)` <- latitude
  env.input$`Longitude (decimal degrees)` <- longitude
  env.input$`Altitude (m)` <- altitude

  #####
  int.input$`User cosmicdoserate (Gy.ka-1)` <- cosmic
  int.input$`errUser cosmicdoserate (Gy.ka-1)` <- cosmic.err
  #####

  env.input$`De (Gy)` <- De
  env.input$`errDe (Gy)` <- De.err

  env.output <- use_DRAC(env.input,verbose=FALSE)

  # --------------------------------------------------------------------------------------------------------------------------

  # De
  De <- as.numeric(int.output$DRAC$highlights$`De (Gy)`)
  De.err <- as.numeric(int.output$DRAC$highlights$`errDe (Gy)`)

  # Combining the 2 runs...

  temp.int.alpha <- as.numeric(int.output$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
  temp.int.alpha.err <- as.numeric(int.output$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

  temp.int.beta <- as.numeric(int.output$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
  temp.int.beta.err <- as.numeric(int.output$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

  temp.ext.alpha <- as.numeric(int.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext.alpha.err <- as.numeric(int.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext.beta <- as.numeric(int.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext.beta.err <- as.numeric(int.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext.gamma <- as.numeric(int.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext.gamma.err <- as.numeric(int.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.env.alpha <- as.numeric(env.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.env.alpha.err <- as.numeric(env.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.env.beta <-  as.numeric(env.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.env.beta.err <- as.numeric(env.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.env.gamma <- as.numeric(env.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.env.gamma.err <- as.numeric(env.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.cosmic <- as.numeric(env.output$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
  temp.cosmic.err <- as.numeric(env.output$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])

  # --------------------------------------------------------------------------------------------------------------------------

  # Estimation of external gamma over/under.estimation

  if(temp.env.gamma > temp.ext.gamma){
    gamma.over <- abs(temp.env.gamma-temp.ext.gamma)/temp.ext.gamma*100

    message.gamma <- paste("The gamma dose rate coming from the grain is probably overestimated (+",round(gamma.over,2)," %).",sep="")
  }else if(temp.env.gamma > temp.ext.gamma){
    gamma.under <- abs(temp.env.gamma-temp.ext.gamma)/temp.ext.gamma

    message.gamma <- paste("The gamma dose rate coming from the grain is probably underestimated (-",round(gamma.under,2)," %).",sep="")
  }else{
    message.gamma <- "The environment and the grain produce similar gamma dose rate. \n"
  }

  comment <- c(comment, gamma=message.gamma)

  # DRAC results
  # Partial dose rate
  # DRAC.int.Dr <- temp.int.alpha+temp.int.beta
  # DRAC.int.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.int.beta^2))
  DRAC.int.Dr <- as.numeric(int.output$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
  DRAC.int.Dr.err <- as.numeric(int.output$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

  DRAC.ext.Dr <- temp.ext.alpha+temp.ext.beta
  DRAC.ext.Dr.err <- sqrt(sum(temp.ext.alpha.err^2, temp.ext.beta^2))

  DRAC.env.Dr <- temp.env.gamma+temp.cosmic
  DRAC.env.Dr.err <- sqrt(sum(temp.env.gamma.err^2, temp.cosmic.err^2))

  DRAC.alpha.Dr <- temp.int.alpha+temp.ext.alpha
  DRAC.alpha.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.ext.alpha^2))

  DRAC.beta.Dr <- temp.int.beta+temp.ext.beta
  DRAC.beta.Dr.err <- sqrt(sum(temp.int.beta.err^2, temp.ext.beta^2))

  DRAC.gamma.Dr <- temp.env.gamma
  DRAC.gamma.Dr.err <- temp.env.gamma.err

  DRAC.cosmic.Dr <- temp.cosmic
  DRAC.cosmic.Dr.err <- temp.cosmic.err

  # Total dose rate
  DRAC.Dr <- sum(DRAC.int.Dr,
                 DRAC.ext.Dr,
                 DRAC.env.Dr,
                 na.rm = TRUE)

  DRAC.Dr.err <- sqrt(sum(DRAC.int.Dr.err^2,
                          DRAC.ext.Dr.err^2,
                          DRAC.env.Dr.err^2,
                          na.rm = TRUE))

  # Age
  DRAC.age <- De/DRAC.Dr
  DRAC.age.err <- DRAC.age*sqrt(sum((De.err/De)^2,(DRAC.Dr.err/DRAC.Dr)^2))


  # output
  DRAC.result <- list(Age = DRAC.age,
                      Age.err =DRAC.age.err,
                      De=De,
                      De.err = De.err,
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

  # Correction of the cosmic dose rate using 'calc_CosmicDoseRate'

  if(is.finite(DRAC.age) && !directDc){
    R.cosmic <- calc_CosmicDoseRate(depth = depth,
                                    density = env.density,
                                    latitude = latitude,
                                    longitude = longitude,
                                    altitude = altitude,
                                    est.age = DRAC.age,
                                    error = DRAC.cosmic.Dr.err/DRAC.cosmic.Dr,
                                    corr.fieldChanges = corr.fieldChanges)

    R.cosmic.Dr <- R.cosmic$summary$dc
    R.cosmic.Dr.err <- R.cosmic$summary$dc*R.cosmic$args$error

  }else{
    R.cosmic.Dr <- DRAC.cosmic.Dr
    R.cosmic.Dr.err <- DRAC.cosmic.Dr.err
  }


  # R results

  # Partial dose rate
  # R.int.Dr <- temp.int.alpha+temp.int.beta
  # R.int.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.int.beta^2))
  R.int.Dr <- as.numeric(int.output$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
  R.int.Dr.err <- as.numeric(int.output$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

  R.ext.Dr <- temp.ext.alpha+temp.ext.beta
  R.ext.Dr.err <- sqrt(sum(temp.ext.alpha.err^2, temp.ext.beta^2))

  R.env.Dr <- temp.env.gamma+R.cosmic.Dr
  R.env.Dr.err <- sqrt(sum(temp.env.gamma.err^2, R.cosmic.Dr.err^2))

  R.alpha.Dr <- temp.int.alpha+temp.ext.alpha
  R.alpha.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.ext.alpha^2))

  R.beta.Dr <- temp.int.beta+temp.ext.beta
  R.beta.Dr.err <- sqrt(sum(temp.int.beta.err^2, temp.ext.beta^2))

  R.gamma.Dr <- temp.env.gamma
  R.gamma.Dr.err <- temp.env.gamma.err

  # Total dose rate
  R.Dr <- sum(R.int.Dr,
              R.ext.Dr,
              R.env.Dr,
              na.rm = TRUE)

  R.Dr.err <- sqrt(sum(R.int.Dr.err^2,
                       R.ext.Dr.err^2,
                       R.env.Dr.err^2,
                       na.rm = TRUE))

  # Age
  R.age <- De/R.Dr
  R.age.err <- R.age*sqrt(sum((De.err/De)^2,(R.Dr.err/R.Dr)^2))



  # Output
  R.result <- list(Age = R.age,
                   Age.err =R.age.err,
                   De=De,
                   De.err = De.err,
                   Dr = R.Dr,
                   Dr.err = R.Dr.err,
                   int.Dr = R.int.Dr,
                   int.Dr.err = R.int.Dr.err,
                   ext.Dr = R.ext.Dr,
                   ext.Dr.err = R.ext.Dr.err,
                   env.Dr = R.env.Dr,
                   env.Dr.err = R.env.Dr.err,
                   alpha.Dr = R.alpha.Dr,
                   alpha.Dr.err = R.alpha.Dr.err,
                   beta.Dr = R.beta.Dr,
                   beta.Dr.err = R.beta.Dr.err,
                   gamma.Dr = R.gamma.Dr,
                   gamma.Dr.err = R.gamma.Dr.err,
                   cosmic.Dr = R.cosmic.Dr,
                   cosmic.Dr.err = R.cosmic.Dr.err)


  age.CE <- date - R.age*1000
  age.CE.err <- R.age.err*1000

  if(!is.finite(age.CE)){
    message.CE <- "The age of the ceramic is unknown."
  }else if(age.CE > 0){
    message.CE <- paste("The heating of the ceramic occured around", round(age.CE), "\u00b1", round(age.CE.err), "CE.")
  }else if(abs(age.CE)<100){
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-1), "\u00b1", round(age.CE.err,-1), " BCE.")
  }else if(abs(age.CE)<1000){
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-2), "\u00b1", round(age.CE.err,-2), " BCE.")
  }else{
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-3), "\u00b1", round(age.CE.err,-3), " BCE.")
  }

  message.project <- paste("For the sample", sample, "of the project", project)
  message.De <- paste("The equivalent dose is:", round(De,3), "\u00b1", round(De.err,3), "Gy.")
  message.Dr <- paste("The dose rate is:", round(R.Dr,3), "\u00b1", round(R.Dr.err,3), "Gy/ka.")
  message.Age <- paste("The age is estimated as:", round(R.age,3), "\u00b1", round(R.age.err,3), "ka.")

  output <- paste("\t [use_DRAC4ceramic] \n ",
                  "\t \n",
                  paste("\t", message.project, "\n"),
                  "\t --------------------------------------------------------- \n ",
                  paste("\t", message.De, "\n"),
                  paste("\t", message.Dr, "\n"),
                  paste("\t", message.gamma, "\n"),
                  "\t --------------------------------------------------------- \n",
                  paste("\t", message.Age,"\n"),
                  paste("\n \t", message.CE, "\n"))


  cat(output)

  result <- list(age=R.age,
                 age.err=R.age.err,
                 Dr=R.Dr,
                 Dr.err=R.Dr.err,
                 DRAC = DRAC.result,
                 R = R.result,
                 comment=comment)

  new.TLum.Results.use_DRAC4ceramic <- set_TLum.Results(data = result,
                                                        originator = "use_DRAC4ceramic")

  return (new.TLum.Results.use_DRAC4ceramic)
}
