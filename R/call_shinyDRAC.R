#' ShinyDRAC application
#'
#' This function calls the 'shinyDRAC' application.
#'
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' \dontrun{
#' call_ShinyDRAC()
#' }
#' @export call_shinyDRAC

call_shinyDRAC <- function(){

  appDir <- system.file("shinyDRAC", package = "TLdating")

  shiny::runApp(appDir,display.mode = 'normal')

}
