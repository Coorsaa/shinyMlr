#' @title Run shinyMlr
#' 
#' @description 
#' Run a local instance of shinyMlr.
#'
#' @param ... [\code{any}]\cr
#'   Additional arguments passed to shiny's
#'   \code{runApp()} function.
#' @examples
#' \dontrun{
#'   runShinyMlr()
#' }
#' @import shiny
#' @import shinythemes
#' @export
runShinyMlr = function(...) {
  runApp(appDir = system.file("application", package = "shinyMlr"), ...)
}
