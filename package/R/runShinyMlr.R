#' Run a local instance of shinyMlr
#'
#' @param ... [\code{any}]\cr
#'   Additional arguments passed to shiny's
#'   \code{runApp()} function.
#' @examples
#' \dontrun{
#'   runShinyMlr()
#' }
#' @seealso downloadShinyMlr
#' @import shiny
#' @import checkmate
#' @export
runShinyMlr = function(...) {
  assertList(extra.args, names = "named")
  runApp(appDir = system.file("application", package = "shinyMlr"), ...)
}
