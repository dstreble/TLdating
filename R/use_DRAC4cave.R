#' Calculation of the dose rate for a sediment sample coming from a cave
#'
#' This function allows to estimating the dose rate for a grain which was surrounded by sediment and rocks.
#' It call the \link{use_DRAC} and \link{calc_CosmicDoseRate} functions from the R package '\link{Luminescence}'.
#' The function 'use_DRAC' is only compatible wth DRAC version 1.1.
#'
#' @param data
#'  \link{list}: data object create throught the function \link{template_DRAC4cave}.
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
#' @export use_DRAC4cave


use_DRAC4cave <- function (

  data,

  notification = TRUE
){
  ## ------------------------------------------------------------------------------------------

  if(!is(data,"DRAC4cave.list")){
    stop("[use_DRAC4cave] Error: data has to be create using the 'template_DRAC4cave' function.")
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
    stop("[use_DRAC4cave] Error: data$info$mineral can only be 'Q', 'F' or 'PM'.")
  }

  conversion.factors <- as.character(data$info$conversion.factors)
  if(!(conversion.factors %in% c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013"))){
    stop("[use_DRAC4cave] Error: data$info$conversion.factors can only be 'AdamiecAitken1998', 'Guerinetal2011' or 'Liritzisetal2013'.")
  }

  int.alpha.attenuation <- as.character(data$info$alpha.size.attenuation)
  if(!(int.alpha.attenuation %in% c("Bell1980","Brennanetal1991"))){
    stop("[use_DRAC4cave] Error: 'data$info$alpha.size.attenuation' can only be 'Bell1980' or 'Brennanetal1991'.")
  }

  int.beta.attenuation <- as.character(data$info$beta.size.attenuation)
  if(!(int.beta.attenuation %in% c("Mejdahl1979","Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F"))){
    stop("[use_DRAC4cave] Error: 'data$info$beta.size.attenuation' can only be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F'.")
  }else if(int.beta.attenuation == "Guerinetal2012-Q" && mineral != "Q"){
    warning("[use_DRAC4cave]  Warning you use 'Guerinetal2012-Q' for 'data$info$beta.size.attenuation' but material is not 'Q'.")
  }else if(int.beta.attenuation == "Guerinetal2012-F" && mineral != "F"){
    warning("[use_DRAC4cave]  Warning you use 'Guerinetal2012-F' for 'data$info$beta.size.attenuation' but material is not 'F'.")
  }

  int.etch.beta.attenuation <- as.character(data$info$beta.etch.attenuation)
  if(!(int.etch.beta.attenuation %in% c("Bell1979","Brennan2003"))){
    stop("[use_DRAC4cave] Error: 'data$info$beta.etch.attenuation' can only be 'Bell1979' or 'Brennan2003'.")
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
    stop("[use_DRAC4cave] Error: 'data$De$De' have to be of type 'numeric' and finite.")
  }

  De.err <- data$De$De.err
  if(is.null(De.err) || length(De.err) == 0 || De.err == "X"){
    De.err <- "X"
  }else if(is.character(De.err)){
    De.err <- gsub(",",".", De.err, fixed = TRUE)
    De.err <- as.numeric(De.err)
  }else if(!is.numeric(De.err) || !is.finite(De.err)){
    stop("[use_DRAC4cave] Error: 'data$De$De.err' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$U' have to be of type 'numeric' and finite.")
  }

  int.U.err <- data$grain$Dr$U.err
  if(is.null(int.U.err) || length(int.U.err) == 0 || int.U.err == "X"){
    int.U.err <- "X"
  }else if(is.character(int.U.err)){
    int.U.err <- gsub(",",".", int.U.err, fixed = TRUE)
    int.U.err <- as.numeric(int.U.err)
  }else if(!is.numeric(int.U.err) || !is.finite(int.U.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  int.Th <- data$grain$Dr$Th
  if(is.null(int.Th) || length(int.Th) == 0 || int.Th == "X"){
    int.Th <- "X"
  }else if(is.character(int.Th)){
    int.Th <- gsub(",",".", int.Th, fixed = TRUE)
    int.Th <- as.numeric(int.Th)
  }else if(!is.numeric(int.Th) || !is.finite(int.Th)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$Th' have to be of type 'numeric' and finite.")
  }

  int.Th.err <- data$grain$Dr$Th.err
  if(is.null(int.Th.err) || length(int.Th.err) == 0 || int.Th.err == "X"){
    int.Th.err <- "X"
  }else if(is.character(int.Th.err)){
    int.Th.err <- gsub(",",".", int.Th.err, fixed = TRUE)
    int.Th.err <- as.numeric(int.Th.err)
  }else if(!is.numeric(int.Th.err) || !is.finite(int.Th.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  int.K <- data$grain$Dr$K
  if(is.null(int.K) || length(int.K) == 0 || int.K == "X"){
    int.K <- "X"
  }else if(is.character(int.K)){
    int.K <- gsub(",",".", int.K, fixed = TRUE)
    int.K <- as.numeric(int.K)
  }else if(!is.numeric(int.K) || !is.finite(int.K)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$K' have to be of type 'numeric' and finite.")
  }

  int.K.err <- data$grain$Dr$K.err
  if(is.null(int.K.err) || length(int.K.err) == 0 || int.K.err == "X"){
    int.K.err <- "X"
  }else if(is.character(int.K.err)){
    int.K.err <- gsub(",",".", int.K.err, fixed = TRUE)
    int.K.err <- as.numeric(int.K.err)
  }else if(!is.numeric(int.K.err) || !is.finite(int.K.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  int.Rb <- data$grain$Dr$Rb
  if(is.null(int.Rb) || length(int.Rb) == 0 || int.Rb == "X"){
    int.Rb <- 'X'
  }else if(is.character(int.Rb)){
    int.Rb <- gsub(",",".", int.Rb, fixed = TRUE)
    int.Rb <- as.numeric(int.Rb)
  }else if(!is.numeric(int.Rb) || !is.finite(int.Rb)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  int.Rb.err <- data$grain$Dr$Rb.err
  if(is.null(int.Rb.err) || length(int.Rb.err) == 0 || int.Rb.err == "X"){
    int.Rb.err <- 'X'
  }else if(is.character(int.Rb.err)){
    int.Rb.err <- gsub(",",".", int.Rb.err, fixed = TRUE)
    int.Rb.err <- as.numeric(int.Rb.err)
  }else if(!is.numeric(int.Rb.err) || !is.finite(int.Rb.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$Rb' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  int.alpha <- data$grain$Dr$alpha
  if(is.null(int.alpha) || length(int.alpha) == 0 || int.alpha == "X"){
    int.alpha <- 'X'
  }else if(is.character(int.alpha)){
    int.alpha <- gsub(",",".", int.alpha, fixed = TRUE)
    int.alpha <- as.numeric(int.alpha)
  }else if(!is.numeric(int.alpha) || !is.finite(int.alpha)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  int.alpha.err <- data$grain$Dr$alpha.err
  if(is.null(int.alpha.err) || length(int.alpha.err) == 0 || int.alpha.err == "X"){
    int.alpha.err <- 'X'
  }else if(is.character(int.alpha.err)){
    int.alpha.err <- gsub(",",".", int.alpha.err, fixed = TRUE)
    int.alpha.err <- as.numeric(int.alpha.err)
  }else if(!is.numeric(int.alpha.err) || !is.finite(int.alpha.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  int.beta <- data$grain$Dr$beta
  if(is.null(int.beta) || length(int.beta) == 0 || int.beta == "X"){
    int.beta <- 'X'
  }else if(is.character(int.beta)){
    int.beta <- gsub(",",".", int.beta, fixed = TRUE)
    int.beta <- as.numeric(int.beta)
  }else if(!is.numeric(int.beta) || !is.finite(int.beta)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$beta' have to be of type 'numeric' and finite.")
  }

  int.beta.err <- data$grain$Dr$beta.err
  if(is.null(int.beta.err) || length(int.beta.err) == 0 || int.beta.err == "X"){
    int.beta.err <- 'X'
  }else if(is.character(int.beta.err)){
    int.beta.err <- gsub(",",".", int.beta.err, fixed = TRUE)
    int.beta.err <- as.numeric(int.beta.err)
  }else if(!is.numeric(int.beta.err) || !is.finite(int.beta.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  int.gamma <- data$grain$Dr$gamma
  if(is.null(int.gamma) || length(int.gamma) == 0 || int.gamma == "X"){
    int.gamma <- 'X'
  }else if(is.character(int.gamma)){
    int.gamma <- gsub(",",".", int.gamma, fixed = TRUE)
    int.gamma <- as.numeric(int.gamma)
  }else if(!is.numeric(int.gamma) || !is.finite(int.gamma)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  int.gamma.err <- data$grain$Dr$gamma.err
  if(is.null(int.gamma.err) || length(int.gamma.err) == 0 || int.gamma.err == "X"){
    int.gamma.err <- 'X'
  }else if(is.character(int.gamma.err)){
    int.gamma.err <- gsub(",",".", int.gamma.err, fixed = TRUE)
    int.gamma.err <- as.numeric(int.gamma.err)
  }else if(!is.numeric(int.gamma.err) || !is.finite(int.gamma.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$Dr$gamma.err' have to be of type 'numeric' and finite.")
  }

  ## ------------------------------------------------------------------------------------------

  # External dose rate
  # By concentration
  ext1.U <- data$sediment$Dr$U
  if(is.null(ext1.U) || length(ext1.U) == 0 || ext1.U == "X"){
    ext1.U <- "X"
  }else if(is.character(ext1.U)){
    ext1.U <- gsub(",",".", ext1.U, fixed = TRUE)
    ext1.U <- as.numeric(ext1.U)
  }else if(!is.numeric(ext1.U) || !is.finite(ext1.U)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$U' have to be of type 'numeric' and finite.")
  }

  ext1.U.err <- data$sediment$Dr$U.err
  if(is.null(ext1.U.err) || length(ext1.U.err) == 0 || ext1.U.err == "X"){
    ext1.U.err <- "X"
  }else if(is.character(ext1.U.err)){
    ext1.U.err <- gsub(",",".", ext1.U.err, fixed = TRUE)
    ext1.U.err <- as.numeric(ext1.U.err)
  }else if(!is.numeric(ext1.U.err) || !is.finite(ext1.U.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  ext1.Th <- data$sediment$Dr$Th
  if(is.null(ext1.Th) || length(ext1.Th) == 0 || ext1.Th == "X"){
    ext1.Th <- "X"
  }else if(is.character(ext1.Th)){
    ext1.Th <- gsub(",",".", ext1.Th, fixed = TRUE)
    ext1.Th <- as.numeric(ext1.Th)
  }else if(!is.numeric(ext1.Th) || !is.finite(ext1.Th)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$Th' have to be of type 'numeric' and finite.")
  }

  ext1.Th.err <- data$sediment$Dr$Th.err
  if(is.null(ext1.Th.err) || length(ext1.Th.err) == 0 || ext1.Th.err == "X"){
    ext1.Th.err <- "X"
  }else if(is.character(ext1.Th.err)){
    ext1.Th.err <- gsub(",",".", ext1.Th.err, fixed = TRUE)
    ext1.Th.err <- as.numeric(ext1.Th.err)
  }else if(!is.numeric(ext1.Th.err) || !is.finite(ext1.Th.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  ext1.K <- data$sediment$Dr$K
  if(is.null(ext1.K) || length(ext1.K) == 0 || ext1.K == "X"){
    ext1.K <- "X"
  }else if(is.character(ext1.K)){
    ext1.K <- gsub(",",".", ext1.K, fixed = TRUE)
    ext1.K <- as.numeric(ext1.K)
  }else if(!is.numeric(ext1.K) || !is.finite(ext1.K)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$K' have to be of type 'numeric' and finite.")
  }

  ext1.K.err <- data$sediment$Dr$K.err
  if(is.null(ext1.K.err) || length(ext1.K.err) == 0 || ext1.K.err == "X"){
    ext1.K.err <- "X"
  }else if(is.character(ext1.K.err)){
    ext1.K.err <- gsub(",",".", ext1.K.err, fixed = TRUE)
    ext1.K.err <- as.numeric(ext1.K.err)
  }else if(!is.numeric(ext1.K.err) || !is.finite(ext1.K.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  ext1.Rb <- data$sediment$Dr$Rb
  if(is.null(ext1.Rb) || length(ext1.Rb) == 0 || ext1.Rb == "X"){
    ext1.Rb <- 'X'
  }else if(is.character(ext1.Rb)){
    ext1.Rb <- gsub(",",".", ext1.Rb, fixed = TRUE)
    ext1.Rb <- as.numeric(ext1.Rb)
  }else if(!is.numeric(ext1.Rb) || !is.finite(ext1.Rb)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  ext1.Rb.err <- data$sediment$Dr$Rb.err
  if(is.null(ext1.Rb.err) || length(ext1.Rb.err) == 0 || ext1.Rb.err == "X"){
    ext1.Rb.err <- 'X'
  }else if(is.character(ext1.Rb.err)){
    ext1.Rb.err <- gsub(",",".", ext1.Rb.err, fixed = TRUE)
    ext1.Rb.err <- as.numeric(ext1.Rb.err)
  }else if(!is.numeric(ext1.Rb.err) || !is.finite(ext1.Rb.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$Rb' have to be of type 'numeric' and finite.")
  }

  ext1.K2Rb <- data$sediment$Dr$K2Rb
  if(is.null(ext1.K2Rb) || length(ext1.K2Rb) == 0){
    if(ext1.Rb == 'X' || ext1.Rb.err =='X' ){
      ext1.K2Rb <- 'Y'
    }else{
      ext1.K2Rb <- 'N'
    }
  }else if(is.logical(ext1.K2Rb)){
    if(ext1.K2Rb){
      ext1.K2Rb <- 'Y'
    }else{
      ext1.K2Rb <- 'N'
    }
  }else if(!(ext1.K2Rb %in% c('Y', 'N')) ){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  ext1.alpha <- data$sediment$Dr$alpha
  if(is.null(ext1.alpha) || length(ext1.alpha) == 0 || ext1.alpha == "X"){
    ext1.alpha <- 'X'
  }else if(is.character(ext1.alpha)){
    ext1.alpha <- gsub(",",".", ext1.alpha, fixed = TRUE)
    ext1.alpha <- as.numeric(ext1.alpha)
  }else if(!is.numeric(ext1.alpha) || !is.finite(ext1.alpha)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  ext1.alpha.err <- data$sediment$Dr$alpha.err
  if(is.null(ext1.alpha.err) || length(ext1.alpha.err) == 0 || ext1.alpha.err == "X"){
    ext1.alpha.err <- 'X'
  }else if(is.character(ext1.alpha.err)){
    ext1.alpha.err <- gsub(",",".", ext1.alpha.err, fixed = TRUE)
    ext1.alpha.err <- as.numeric(ext1.alpha.err)
  }else if(!is.numeric(ext1.alpha.err) || !is.finite(ext1.alpha.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  ext1.beta <- data$sediment$Dr$beta
  if(is.null(ext1.beta) || length(ext1.beta) == 0 || ext1.beta == "X"){
    ext1.beta <- 'X'
  }else if(is.character(ext1.beta)){
    ext1.beta <- gsub(",",".", ext1.beta, fixed = TRUE)
    ext1.beta <- as.numeric(ext1.beta)
  }else if(!is.numeric(ext1.beta) || !is.finite(ext1.beta)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$beta' have to be of type 'numeric' and finite.")
  }

  ext1.beta.err <- data$sediment$Dr$beta.err
  if(is.null(ext1.beta.err) || length(ext1.beta.err) == 0 || ext1.beta.err == "X"){
    ext1.beta.err <- 'X'
  }else if(is.character(ext1.beta.err)){
    ext1.beta.err <- gsub(",",".", ext1.beta.err, fixed = TRUE)
    ext1.beta.err <- as.numeric(ext1.beta.err)
  }else if(!is.numeric(ext1.beta.err) || !is.finite(ext1.beta.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  ext1.gamma <- data$sediment$Dr$gamma
  if(is.null(ext1.gamma) || length(ext1.gamma) == 0 || ext1.gamma == "X"){
    ext1.gamma <- 'X'
  }else if(is.character(ext1.gamma)){
    ext1.gamma <- gsub(",",".", ext1.gamma, fixed = TRUE)
    ext1.gamma <- as.numeric(ext1.gamma)
  }else if(!is.numeric(ext1.gamma) || !is.finite(ext1.gamma)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  ext1.gamma.err <- data$sediment$Dr$gamma.err
  if(is.null(ext1.gamma.err) || length(ext1.gamma.err) == 0 || ext1.gamma.err == "X"){
    ext1.gamma.err <- 'X'
  }else if(is.character(ext1.gamma.err)){
    ext1.gamma.err <- gsub(",",".", ext1.gamma.err, fixed = TRUE)
    ext1.gamma.err <- as.numeric(ext1.gamma.err)
  }else if(!is.numeric(ext1.gamma.err) || !is.finite(ext1.gamma.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$gamma.err' have to be of type 'numeric' and finite.")
  }


  ## ------------------------------------------------------------------------------------------

  # Environmental dose rate
  # By concentration
  ext2.U <- data$rock$Dr$U
  if(is.null(ext2.U) || length(ext2.U) == 0 || ext2.U == "X"){
    ext2.U <- "X"
  }else if(is.character(ext2.U)){
    ext2.U <- gsub(",",".", ext2.U, fixed = TRUE)
    ext2.U <- as.numeric(ext2.U)
  }else if(!is.numeric(ext2.U) || !is.finite(ext2.U)){
    stop("[use_DRAC4sediment] Error: 'data$rock$Dr$U' have to be of type 'numeric' and finite.")
  }

  ext2.U.err <- data$rock$Dr$U.err
  if(is.null(ext2.U.err) || length(ext2.U.err) == 0 || ext2.U.err == "X"){
    ext2.U.err <- "X"
  }else if(is.character(ext2.U.err)){
    ext2.U.err <- gsub(",",".", ext2.U.err, fixed = TRUE)
    ext2.U.err <- as.numeric(ext2.U.err)
  }else if(!is.numeric(ext2.U.err) || !is.finite(ext2.U.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$U.err' have to be of type 'numeric' and finite.")
  }

  ext2.Th <- data$rock$Dr$Th
  if(is.null(ext2.Th) || length(ext2.Th) == 0 || ext2.Th == "X"){
    ext2.Th <- "X"
  }else if(is.character(ext2.Th)){
    ext2.Th <- gsub(",",".", ext2.Th, fixed = TRUE)
    ext2.Th <- as.numeric(ext2.Th)
  }else if(!is.numeric(ext2.Th) || !is.finite(ext2.Th)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$Th' have to be of type 'numeric' and finite.")
  }

  ext2.Th.err <- data$rock$Dr$Th.err
  if(is.null(ext2.Th.err) || length(ext2.Th.err) == 0 || ext2.Th.err == "X"){
    ext2.Th.err <- "X"
  }else if(is.character(ext2.Th.err)){
    ext2.Th.err <- gsub(",",".", ext2.Th.err, fixed = TRUE)
    ext2.Th.err <- as.numeric(ext2.Th.err)
  }else if(!is.numeric(ext2.Th.err) || !is.finite(ext2.Th.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$Th.err' have to be of type 'numeric' and finite.")
  }

  ext2.K <- data$rock$Dr$K
  if(is.null(ext2.K) || length(ext2.K) == 0 || ext2.K == "X"){
    ext2.K <- "X"
  }else if(is.character(ext2.K)){
    ext2.K <- gsub(",",".", ext2.K, fixed = TRUE)
    ext2.K <- as.numeric(ext2.K)
  }else if(!is.numeric(ext2.K) || !is.finite(ext2.K)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$K' have to be of type 'numeric' and finite.")
  }

  ext2.K.err <- data$rock$Dr$K.err
  if(is.null(ext2.K.err) || length(ext2.K.err) == 0 || ext2.K.err == "X"){
    ext2.K.err <- "X"
  }else if(is.character(ext2.K.err)){
    ext2.K.err <- gsub(",",".", ext2.K.err, fixed = TRUE)
    ext2.K.err <- as.numeric(ext2.K.err)
  }else if(!is.numeric(ext2.K.err) || !is.finite(ext2.K.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$K.err' have to be of type 'numeric' and finite.")
  }

  ext2.Rb <- data$rock$Dr$Rb
  if(is.null(ext2.Rb) || length(ext2.Rb) == 0 || ext2.Rb == "X"){
    ext2.Rb <- 'X'
  }else if(is.character(ext2.Rb)){
    ext2.Rb <- gsub(",",".", ext2.Rb, fixed = TRUE)
    ext2.Rb <- as.numeric(ext2.Rb)
  }else if(!is.numeric(ext2.Rb) || !is.finite(ext2.Rb)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$Rb' have to be of type 'numeric' and finite.")
  }
  ext2.Rb.err <- data$rock$Dr$Rb.err
  if(is.null(ext2.Rb.err) || length(ext2.Rb.err) == 0 || ext2.Rb.err == "X"){
    ext2.Rb.err <- 'X'
  }else if(is.character(ext2.Rb.err)){
    ext2.Rb.err <- gsub(",",".", ext2.Rb.err, fixed = TRUE)
    ext2.Rb.err <- as.numeric(ext2.Rb.err)
  }else if(!is.numeric(ext2.Rb.err) || !is.finite(ext2.Rb.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$Rb' have to be of type 'numeric' and finite.")
  }

  ext2.K2Rb <- data$rock$Dr$K2Rb
  if(is.null(ext2.K2Rb) || length(ext2.K2Rb) == 0){
    if(ext2.Rb == 'X' || ext2.Rb.err =='X' ){
      ext2.K2Rb <- 'Y'
    }else{
      ext2.K2Rb <- 'N'
    }
  }else if(is.logical(ext2.K2Rb)){
    if(ext2.K2Rb){
      ext2.K2Rb <- 'Y'
    }else{
      ext2.K2Rb <- 'N'
    }
  }else if(!(ext2.K2Rb %in% c('Y', 'N')) ){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$K2Rb' have to be 'Y' or 'N'.")
  }

  # Direct evaluation
  ext2.alpha <- data$rock$Dr$alpha
  if(is.null(ext2.alpha) || length(ext2.alpha) == 0 || ext2.alpha == "X"){
    ext2.alpha <- 'X'
  }else if(is.character(ext2.alpha)){
    ext2.alpha <- gsub(",",".", ext2.alpha, fixed = TRUE)
    ext2.alpha <- as.numeric(ext2.alpha)
  }else if(!is.numeric(ext2.alpha) || !is.finite(ext2.alpha)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$alpha' have to be of type 'numeric' and finite.")
  }

  ext2.alpha.err <- data$rock$Dr$alpha.err
  if(is.null(ext2.alpha.err) || length(ext2.alpha.err) == 0 || ext2.alpha.err == "X"){
    ext2.alpha.err <- 'X'
  }else if(is.character(ext2.alpha.err)){
    ext2.alpha.err <- gsub(",",".", ext2.alpha.err, fixed = TRUE)
    ext2.alpha.err <- as.numeric(ext2.alpha.err)
  }else if(!is.numeric(ext2.alpha.err) || !is.finite(ext2.alpha.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$alpha.err' have to be of type 'numeric' and finite.")
  }

  ext2.beta <- data$rock$Dr$beta
  if(is.null(ext2.beta) || length(ext2.beta) == 0 || ext2.beta == "X"){
    ext2.beta <- 'X'
  }else if(is.character(ext2.beta)){
    ext2.beta <- gsub(",",".", ext2.beta, fixed = TRUE)
    ext2.beta <- as.numeric(ext2.beta)
  }else if(!is.numeric(ext2.beta) || !is.finite(ext2.beta)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$beta' have to be of type 'numeric' and finite.")
  }

  ext2.beta.err <- data$rock$Dr$beta.err
  if(is.null(ext2.beta.err) || length(ext2.beta.err) == 0 || ext2.beta.err == "X"){
    ext2.beta.err <- 'X'
  }else if(is.character(ext2.beta.err)){
    ext2.beta.err <- gsub(",",".", ext2.beta.err, fixed = TRUE)
    ext2.beta.err <- as.numeric(ext2.beta.err)
  }else if(!is.numeric(ext2.beta.err) || !is.finite(ext2.beta.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$beta.err' have to be of type 'numeric' and finite.")
  }

  ext2.gamma <- data$rock$Dr$gamma
  if(is.null(ext2.gamma) || length(ext2.gamma) == 0 || ext2.gamma == "X"){
    ext2.gamma <- 'X'
  }else if(is.character(ext2.gamma)){
    ext2.gamma <- gsub(",",".", ext2.gamma, fixed = TRUE)
    ext2.gamma <- as.numeric(ext2.gamma)
  }else if(!is.numeric(ext2.gamma) || !is.finite(ext2.gamma)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$gamma' have to be of type 'numeric' and finite.")
  }

  ext2.gamma.err <- data$rock$Dr$gamma.err
  if(is.null(ext2.gamma.err) || length(ext2.gamma.err) == 0 || ext2.gamma.err == "X"){
    ext2.gamma.err <- 'X'
  }else if(is.character(ext2.gamma.err)){
    ext2.gamma.err <- gsub(",",".", ext2.gamma.err, fixed = TRUE)
    ext2.gamma.err <- as.numeric(ext2.gamma.err)
  }else if(!is.numeric(ext2.gamma.err) || !is.finite(ext2.gamma.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$Dr$gamma.err' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.max' have to be of type 'numeric' and finite.")
  }
  if(int.size.max > 1000){
    int.size.max <- 1000
  }
  if(int.size.min > int.size.max){
    stop("[use_DRAC4cave] Error: 'data$grain$info$size.min' > 'data$grain$info$int.size.max'")
  }

  int.etch.min <- data$grain$info$grain.etch.min
  if(is.null(int.etch.min) || length(int.etch.min) == 0 || int.etch.min == "X"){
    int.etch.min <- 0
  }else if(is.character(int.etch.min)){
    int.etch.min <- gsub(",",".", int.etch.min, fixed = TRUE)
    int.etch.min <- as.numeric(int.etch.min)
  }else if(!is.numeric(int.etch.min) || !is.finite(int.etch.min)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.max' have to be of type 'numeric' and finite.")
  }
  if(int.etch.max > 30){
    int.etch.max <- 30
  }
  if(int.etch.min > int.etch.max){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' > 'data$grain$info$grain.etch.max'")
  }

  int.a.value <- data$grain$info$a.value
  if(is.null(int.a.value) || length(int.a.value) == 0 || int.a.value == "X"){
    int.a.value <- 'X'
  }else if(is.character(int.a.value)){
    int.a.value <- gsub(",",".", int.a.value, fixed = TRUE)
    int.a.value <- as.numeric(int.a.value)
  }else if(!is.numeric(int.a.value) || !is.finite(int.a.value)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value' have to be of type 'numeric' and finite.")
  }
  int.a.value.err <- data$grain$info$a.value.err
  if(is.null(int.a.value.err) || length(int.a.value.err) == 0 || int.a.value.err == "X"){
    int.a.value.err <- 'X'
  }else if(is.character(int.a.value.err)){
    int.a.value.err <- gsub(",",".", int.a.value.err, fixed = TRUE)
    int.a.value.err <- as.numeric(int.a.value.err)
  }else if(!is.numeric(int.a.value.err) || !is.finite(int.a.value.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value.err' have to be of type 'numeric' and finite.")
  }
  ## ------------------------------------------------------------------------------------------

  # 'Sediment' information
  max.etch <- 30

  ext1.etch.min <- max.etch
  ext1.etch.max <- max.etch

  ext1.water <- data$sediment$info$water.content
  if(is.null(ext1.water) || length(ext1.water) == 0 || ext1.water == "X"){
    ext1.water <- 5
  }else if(is.character(ext1.water)){
    ext1.water <- gsub(",",".", ext1.water, fixed = TRUE)
    ext1.water <- as.numeric(ext1.water)
  }else if(!is.numeric(ext1.water) || !is.finite(ext1.water)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' have to be of type 'numeric' and finite.")
  }

  ext1.water.err <- data$sediment$info$water.content.err
  if(is.null(ext1.water.err) || length(ext1.water.err) == 0 || ext1.water.err == "X"){
    ext1.water.err <- 2
  }else if(is.character(ext1.water.err)){
    ext1.water.err <- gsub(",",".", ext1.water.err, fixed = TRUE)
    ext1.water.err <- as.numeric(ext1.water.err)
  }else if(!is.numeric(ext1.water.err) || !is.finite(ext1.water.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content.err' have to be of type 'numeric' and finite.")
  }

  ext1.density <- data$sediment$info$density
  if(is.null(ext1.density) || length(ext1.density) == 0 || ext1.density == "X"){
    ext1.density <- 1.8
  }else if(is.character(ext1.density)){
    ext1.density <- gsub(",",".", ext1.density, fixed = TRUE)
    ext1.density <- as.numeric(ext1.density)
  }else if(!is.numeric(ext1.density) || !is.finite(ext1.density)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' have to be of type 'numeric' and finite.")
  }

  ext1.density.err <- data$sediment$info$density.err
  if(is.null(ext1.density.err) || length(ext1.density.err) == 0 || ext1.density.err == "X"){
    ext1.density.err <- 0.1
  }else if(is.character(ext1.density.err)){
    ext1.density.err <- gsub(",",".", ext1.density.err, fixed = TRUE)
    ext1.density.err <- as.numeric(ext1.density.err)
  }else if(!is.numeric(ext1.density.err) || !is.finite(ext1.density.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density.err' have to be of type 'numeric' and finite.")
  }

  ext1.scale4shallow.depth <- data$rock$info$scale4shallow.depth
  if(is.null(ext1.scale4shallow.depth) || length(ext1.scale4shallow.depth) == 0){
    ext1.scale4shallow.depth <- 'Y'
  }else if(is.logical(ext1.scale4shallow.depth)){
    if(ext1.scale4shallow.depth){
      ext1.scale4shallow.depth <- 'Y'
    }else{
      ext1.scale4shallow.depth <- 'N'
    }
  }else if(!(ext1.scale4shallow.depth %in% c('Y', 'N')) ){
    stop("[use_DRAC4cave] Error: 'data$sediment$Dr$scale4shallow.depth' have to be 'Y' or 'N'.")
  }

  ## ------------------------------------------------------------------------------------------

  # Rock information

  # Env information
  ext2.water <- data$rock$info$water.content
  if(is.null(ext2.water) || length(ext2.water) == 0 || ext2.water == "X"){
    ext2.water <- 5
  }else if(is.character(ext2.water)){
    ext2.water <- gsub(",",".", ext2.water, fixed = TRUE)
    ext2.water <- as.numeric(ext2.water)
  }else if(!is.numeric(ext2.water) || !is.finite(ext2.water)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$water.content' have to be of type 'numeric' and finite.")
  }

  ext2.water.err <- data$rock$info$water.content.err
  if(is.null(ext2.water.err) || length(ext2.water.err) == 0 || ext2.water.err == "X"){
    ext2.water.err <- 2
  }else if(is.character(ext2.water.err)){
    ext2.water.err <- gsub(",",".", ext2.water.err, fixed = TRUE)
    ext2.water.err <- as.numeric(ext2.water.err)
  }else if(!is.numeric(ext2.water.err) || !is.finite(ext2.water.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$water.content.err' have to be of type 'numeric' and finite.")
  }

  ext2.density <- data$rock$info$density
  if(is.null(ext2.density) || length(ext2.density) == 0 || ext2.density == "X"){
    ext2.density <- 1.8
  }else if(is.character(ext2.density)){
    ext2.density <- gsub(",",".", ext2.density, fixed = TRUE)
    ext2.density <- as.numeric(ext2.density)
  }else if(!is.numeric(ext2.density) || !is.finite(ext2.density)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$density' have to be of type 'numeric' and finite.")
  }

  ext2.density.err <- data$rock$info$density.err
  if(is.null(ext2.density.err) || length(ext2.density.err) == 0 || ext2.density.err == "X"){
    ext2.density.err <- 1.8
  }else if(is.character(ext2.density.err)){
    ext2.density.err <- gsub(",",".", ext2.density.err, fixed = TRUE)
    ext2.density.err <- as.numeric(ext2.density.err)
  }else if(!is.numeric(ext2.density.err) || !is.finite(ext2.density.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$density.err' have to be of type 'numeric' and finite.")
  }

  ext2.ratio <- data$rock$info$ratio
  if(is.null(ext2.ratio) || length(ext2.ratio) == 0 || ext2.ratio == "X"){
    ext2.ratio <- 0
  }else if(is.character(ext2.ratio)){
    ext2.ratio <- gsub(",",".", ext2.ratio, fixed = TRUE)
    ext2.ratio <- as.numeric(ext2.ratio)
  }else if(!is.numeric(ext2.ratio) || !is.finite(ext2.ratio)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$ratio' have to be of type 'numeric' and finite.")
  }else if(ext2.ratio<0 || ext2.ratio>100){
    stop("[use_DRAC4cave] Error: 'data$rock$info$ratio' > 1 or < 0.")
  }

  ext2.ratio.err <- data$rock$info$ratio.err
  if(is.null(ext2.ratio.err) || length(ext2.ratio.err) == 0 || ext2.ratio.err == "X"){
    ext2.ratio.err <- 0
  }else if(is.character(ext2.ratio.err)){
    ext2.ratio.err <- gsub(",",".", ext2.ratio.err, fixed = TRUE)
    ext2.ratio.err <- as.numeric(ext2.ratio.err)
  }else if(!is.numeric(ext2.ratio.err) || !is.finite(ext2.ratio.err)){
    stop("[use_DRAC4cave] Error: 'data$rock$info$density.err' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$cosmic$depth' have to be of type 'numeric' and finite.")
  }

  depth.err <- data$cosmic$depth.err
  if(is.null(depth.err) || length(depth.err) == 0 || depth.err == "X"){
    depth.err <- 'X'
  }else if(is.character(depth.err)){
    depth.err <- gsub(",",".", depth.err, fixed = TRUE)
    depth.err <- as.numeric(depth.err)
  }else if(!is.numeric(depth.err) || !is.finite(depth.err)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$depth.err' have to be of type 'numeric' and finite.")
  }

  latitude <- data$cosmic$latitude
  if(is.null(latitude) || length(latitude) == 0 || latitude == "X"){
    latitude <- 'X'
  }else if(is.character(latitude)){
    latitude <- gsub(",",".", latitude, fixed = TRUE)
    latitude <- as.numeric(latitude)
  }else if(!is.numeric(latitude) || !is.finite(latitude)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$latitude' have to be of type 'numeric' and finite.")
  }

  longitude <- data$cosmic$longitude
  if(is.null(longitude) || length(longitude) == 0 || longitude == "X"){
    longitude <- 'X'
  }else if(is.character(longitude)){
    longitude <- gsub(",",".", longitude, fixed = TRUE)
    longitude <- as.numeric(longitude)
  }else if(!is.numeric(longitude) || !is.finite(longitude)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$longitude' have to be of type 'numeric' and finite.")
  }

  altitude <- data$cosmic$altitude
  if(is.null(altitude) || length(altitude) == 0 || altitude == "X"){
    altitude <- 'X'
  }else if(is.character(altitude)){
    altitude <- gsub(",",".", altitude, fixed = TRUE)
    altitude <- as.numeric(altitude)
  }else if(!is.numeric(altitude) || !is.finite(altitude)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$altitude' have to be of type 'numeric' and finite.")
  }

  #Direct measurement
  cosmic <- data$cosmic$Dr
  if(is.null(cosmic) || length(cosmic) == 0 || cosmic == "X"){
    cosmic <- 'X'
  }else if(is.character(cosmic)){
    cosmic <- gsub(",",".", cosmic, fixed = TRUE)
    cosmic <- as.numeric(cosmic)
  }else if(!is.numeric(cosmic) || !is.finite(cosmic)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$Dr' have to be of type 'numeric' and finite.")
  }

  cosmic.err <- data$cosmic$Dr.err
  if(is.null(cosmic.err) || length(cosmic.err) == 0 || cosmic.err == "X"){
    cosmic.err <- 'X'
  }else if(is.character(cosmic.err)){
    cosmic.err <- gsub(",",".", cosmic.err, fixed = TRUE)
    cosmic.err <- as.numeric(cosmic.err)
  }else if(!is.numeric(cosmic.err) || !is.finite(cosmic.err)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$Dr.err' have to be of type 'numeric' and finite.")
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
    stop("[use_DRAC4cave] Error: 'data$cosmic$corr.fieldChanges' have to be 'Y' or 'N'.")
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

  run1.input <- template_DRAC(notification = notification)

  run1.input$`Project ID` <- project
  run1.input$`Sample ID` <- sample
  run1.input$Mineral <- mineral
  run1.input$`Conversion factors` <- conversion.factors

  run1.input$`Internal U (ppm)` <-  int.U
  run1.input$`errInternal U (ppm)` <- int.U.err
  run1.input$`Internal Th (ppm)` <- int.Th
  run1.input$`errInternal Th (ppm)` <- int.Th.err
  run1.input$`Internal K (%)` <- int.K
  run1.input$`errInternal K (%)` <- int.K.err
  run1.input$`Rb (ppm)` <- int.Rb
  run1.input$`errRb (ppm)` <- int.Rb.err
  run1.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  #####
  if(is.finite(int.alpha) && is.finite(int.beta)){
    run1.input$`User internal doserate (Gy.ka-1)` <- int.alpha + int.beta
  }else if(is.finite(int.alpha)){
    run1.input$`User internal doserate (Gy.ka-1)` <- int.alpha
  }else if(is.finite(int.beta)){
    run1.input$`User internal doserate (Gy.ka-1)` <- int.beta
  }else{
    run1.input$`User internal doserate (Gy.ka-1)` <- 'X'
  }

  if(is.finite(int.alpha.err) && is.finite(int.beta.err)){
    run1.input$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(int.alpha^2, int.beta^2, na.rm = TRUE))
  }else if(is.finite(int.alpha)){
    run1.input$`errUser internal doserate (Gy.ka-1)` <- int.alpha.err
  }else if(is.finite(int.beta)){
    run1.input$`errUser internal doserate (Gy.ka-1)` <- int.beta.err
  }else{
    run1.input$`errUser internal doserate (Gy.ka-1)` <- 'X'
  }
  ######

  run1.input$`ExternalU (ppm)` <-  ext1.U
  run1.input$`errExternal U (ppm)` <- ext1.U.err
  run1.input$`External Th (ppm)` <- ext1.Th
  run1.input$`errExternal Th (ppm)` <- ext1.Th.err
  run1.input$`External K (%)` <- ext1.K
  run1.input$`errExternal K (%)` <- ext1.K.err
  run1.input$`External Rb (ppm)` <- ext1.Rb
  run1.input$`errExternal Rb (ppm)` <- ext1.Rb.err
  run1.input$`Calculate external Rb from K conc?` <- ext1.K2Rb

  #####
  run1.input$`User external alphadoserate (Gy.ka-1)` <- ext1.alpha
  run1.input$`errUser external alphadoserate (Gy.ka-1)` <- ext1.alpha.err
  run1.input$`User external betadoserate (Gy.ka-1)` <- ext1.beta
  run1.input$`errUser external betadoserate (Gy.ka-1)` <- ext1.beta.err
  run1.input$`User external gamma doserate (Gy.ka-1)` <- ext1.gamma
  run1.input$`errUser external gammadoserate (Gy.ka-1)` <- ext1.gamma.err
  #####


  run1.input$`Grain size min (microns)` <- int.size.min
  run1.input$`Grain size max (microns)` <- int.size.max

  run1.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  run1.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  run1.input$`Etch depth min (microns)` <- int.etch.min
  run1.input$`Etch depth max (microns)` <- int.etch.max

  run1.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  run1.input$`a-value` <- int.a.value
  run1.input$`erra-value` <- int.a.value.err

  run1.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext1.water
  run1.input$`errWater content %` <- ext1.water.err

  run1.input$`Depth (m)` <- depth
  run1.input$`errDepth (m)` <- depth.err

  run1.input$`Overburden density (g cm-3)` <- ext2.density
  run1.input$`errOverburden density (g cm-3)` <- ext2.density.err

  run1.input$`Latitude (decimal degrees)` <- latitude
  run1.input$`Longitude (decimal degrees)` <- longitude
  run1.input$`Altitude (m)` <- altitude

  #####
  run1.input$`User cosmicdoserate (Gy.ka-1)` <- cosmic
  run1.input$`errUser cosmicdoserate (Gy.ka-1)` <- cosmic.err
  #####

  run1.input$`De (Gy)` <- De
  run1.input$`errDe (Gy)` <- De.err

  run1.output <- use_DRAC(run1.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # Second run, Evironmental dose (sediment)

  run2.input <- template_DRAC(notification = notification)

  run2.input$`Project ID` <- project
  run2.input$`Sample ID` <- sample
  run2.input$Mineral <- mineral
  run2.input$`Conversion factors` <- conversion.factors

  run2.input$`Internal U (ppm)` <-  int.U
  run2.input$`errInternal U (ppm)` <- int.U.err
  run2.input$`Internal Th (ppm)` <- int.Th
  run2.input$`errInternal Th (ppm)` <- int.Th.err
  run2.input$`Internal K (%)` <- int.K
  run2.input$`errInternal K (%)` <- int.K.err
  run2.input$`Rb (ppm)` <- int.Rb
  run2.input$`errRb (ppm)` <- int.Rb.err
  run2.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  #####
  if(is.finite(int.alpha) && is.finite(int.beta)){
    run2.input$`User internal doserate (Gy.ka-1)` <- int.alpha + int.beta
  }else if(is.finite(int.alpha)){
    run2.input$`User internal doserate (Gy.ka-1)` <- int.alpha
  }else if(is.finite(int.beta)){
    run2.input$`User internal doserate (Gy.ka-1)` <- int.beta
  }else{
    run2.input$`User internal doserate (Gy.ka-1)` <- 'X'
  }

  if(is.finite(int.alpha.err) && is.finite(int.beta.err)){
    run2.input$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(int.alpha^2, int.beta^2, na.rm = TRUE))
  }else if(is.finite(int.alpha)){
    run2.input$`errUser internal doserate (Gy.ka-1)` <- int.alpha.err
  }else if(is.finite(int.beta)){
    run2.input$`errUser internal doserate (Gy.ka-1)` <- int.beta.err
  }else{
    run2.input$`errUser internal doserate (Gy.ka-1)` <- 'X'
  }
  ######

  run2.input$`ExternalU (ppm)` <-  ext2.U
  run2.input$`errExternal U (ppm)` <- ext2.U.err
  run2.input$`External Th (ppm)` <- ext2.Th
  run2.input$`errExternal Th (ppm)` <- ext2.Th.err
  run2.input$`External K (%)` <- ext2.K
  run2.input$`errExternal K (%)` <- ext2.K.err
  run2.input$`External Rb (ppm)` <- ext2.Rb
  run2.input$`errExternal Rb (ppm)` <- ext2.Rb.err
  run2.input$`Calculate external Rb from K conc?` <- ext2.K2Rb

  #####
  run2.input$`User external alphadoserate (Gy.ka-1)` <- ext2.alpha
  run2.input$`errUser external alphadoserate (Gy.ka-1)` <- ext2.alpha.err
  run2.input$`User external betadoserate (Gy.ka-1)` <- ext2.beta
  run2.input$`errUser external betadoserate (Gy.ka-1)` <- ext2.beta.err
  run2.input$`User external gamma doserate (Gy.ka-1)` <- ext2.gamma
  run2.input$`errUser external gammadoserate (Gy.ka-1)` <- ext2.gamma.err
  #####


  run2.input$`Grain size min (microns)` <- int.size.min
  run2.input$`Grain size max (microns)` <- int.size.max

  run2.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  run2.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  run2.input$`Etch depth min (microns)` <- int.etch.min
  run2.input$`Etch depth max (microns)` <- int.etch.max

  run2.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  run2.input$`a-value` <- int.a.value
  run2.input$`erra-value` <- int.a.value.err

  run2.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext2.water
  run2.input$`errWater content %` <- ext2.water.err

  run2.input$`Depth (m)` <- depth
  run2.input$`errDepth (m)` <- depth.err

  run2.input$`Overburden density (g cm-3)` <- ext2.density
  run2.input$`errOverburden density (g cm-3)` <- ext2.density.err

  run2.input$`Latitude (decimal degrees)` <- latitude
  run2.input$`Longitude (decimal degrees)` <- longitude
  run2.input$`Altitude (m)` <- altitude

  #####
  run2.input$`User cosmicdoserate (Gy.ka-1)` <- cosmic
  run2.input$`errUser cosmicdoserate (Gy.ka-1)` <- cosmic.err
  #####

  run2.input$`De (Gy)` <- De
  run2.input$`errDe (Gy)` <- De.err

  run2.output <- use_DRAC(run2.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # De
  De <- as.numeric(run1.output$DRAC$highlights$`De (Gy)`)
  De.err <- as.numeric(run1.output$DRAC$highlights$`errDe (Gy)`)

  # Combining the 2 runs...


  temp.int.alpha <- as.numeric(run1.output$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
  temp.int.alpha.err <- as.numeric(run1.output$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

  temp.int.beta <- as.numeric(run1.output$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
  temp.int.beta.err <- as.numeric(run1.output$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

  temp.ext1.alpha <- as.numeric(run1.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext1.alpha.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext1.beta <- as.numeric(run1.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext1.beta.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext1.gamma <- as.numeric(run1.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext1.gamma.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.ext2.alpha <- as.numeric(run2.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext2.alpha.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext2.beta <-  as.numeric(run2.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext2.beta.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext2.gamma <- as.numeric(run2.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext2.gamma.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.cosmic <- as.numeric(run1.output$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
  temp.cosmic.err <- as.numeric(run1.output$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])

  ratio <- ext2.ratio
  ratio.err <- ext2.ratio.err

  temp.ext.alpha <- temp.ext1.alpha
  temp.ext.alpha.err <- temp.ext1.alpha.err

  temp.ext.beta <- temp.ext1.beta
  temp.ext.beta.err <- temp.ext1.beta.err

  corr.ext1.gamma <- (1-ext2.ratio)*temp.ext1.gamma
  corr.ext1.gamma.err <- sqrt((ext2.ratio.err/ext2.ratio)^2 + (temp.ext1.gamma.err/temp.ext1.gamma)^2)*corr.ext1.gamma

  corr.ext2.gamma <- ext2.ratio*temp.ext2.gamma
  corr.ext2.gamma.err <- sqrt((ext2.ratio.err/ext2.ratio)^2 + (temp.ext1.gamma.err/temp.ext1.gamma)^2)*corr.ext2.gamma

  temp.env.gamma <-  corr.ext1.gamma + corr.ext2.gamma
  temp.env.gamma.err <- sqrt( corr.ext1.gamma.err^2 + corr.ext2.gamma.err^2)
  # --------------------------------------------------------------------------------------------------------------------------

  # DRAC results
  # Partial dose rate
  # DRAC.int.Dr <- temp.int.alpha+temp.int.beta
  # DRAC.int.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.int.beta^2))
  DRAC.int.Dr <- as.numeric(run1.output$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
  DRAC.int.Dr.err <- as.numeric(run1.output$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

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
                 DRAC.env.Dr)

  DRAC.Dr.err <- sqrt(sum(DRAC.int.Dr.err^2,
                          DRAC.ext.Dr.err^2,
                          DRAC.env.Dr.err^2))

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
                                    density = ext1.density,
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
  R.int.Dr <- as.numeric(run1.output$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
  R.int.Dr.err <- as.numeric(run1.output$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

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

  message.project <- paste("For the sample", sample, "of the project", project)
  message.De <- paste("The equivalent dose is:", round(De,3), "\u00b1", round(De.err,3), "Gy.")
  message.Dr <- paste("The dose rate is:", round(R.Dr,3), "\u00b1", round(R.Dr.err,3), "Gy/ka.")
  message.Age <- paste("The age is estimated as:", round(R.age,3), "\u00b1", round(R.age.err,3), "ka.")

  if(!is.finite(age.CE)){
    message.CE <- "The age of the sediment is unknown."
  }else if(age.CE > 0){
    message.CE <- paste("The burial of the sediment occured around", round(age.CE), "\u00b1", round(age.CE.err), "CE.")
  }else if(abs(age.CE)<100){
    message.CE <- paste("The burial of the sediment occured around", round(abs(age.CE),-1), "\u00b1", round(age.CE.err,-1), " BCE.")
  }else if(abs(age.CE)<1000){
    message.CE <- paste("The burial of the sediment occured around", round(abs(age.CE),-2), "\u00b1", round(age.CE.err,-2), " BCE.")
  }else{
    message.CE <- paste("The burial of the sediment occured around", round(abs(age.CE),-3), "\u00b1", round(age.CE.err,-3), " BCE.")
  }

  output <- paste("\t [use_DRAC4cave] \n ",
                  "\t \n",
                  paste("\t", message.project, "\n"),
                  "\t --------------------------------------------------------- \n ",
                  paste("\t", message.De, "\n"),
                  paste("\t", message.Dr, "\n"),
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

  new.TLum.Results.use_DRAC4cave <- set_TLum.Results(data = result,
                                                     originator = "use_DRAC4cave")

  return (new.TLum.Results.use_DRAC4cave)
}
