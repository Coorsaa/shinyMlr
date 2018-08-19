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
  appDir = system.file("shinyMlr", package = "shinyMlr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyMlr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
