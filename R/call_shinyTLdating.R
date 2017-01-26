#' ShinyTLdating application
#'
#' This function calls the 'shinyTLdating' application.
#'
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' \dontrun{
#' call_shinyTLdating()
#' }
#' @export call_shinyTLdating

call_shinyTLdating <- function(){

  appDir <- system.file("shinyTLdating", package = "TLdating")

  shiny::runApp(appDir,display.mode = 'normal')

}
