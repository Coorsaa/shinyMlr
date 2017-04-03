#' Run a local instance of shinyMlr
#'
#' @param ... [\code{any}]\cr
#'   Additional arguments passed to shiny's
#'   \code{runApp()} function.
#' @examples
#' \dontrun{
#'   library(shinyMlr)
#'   downloadShinyMlr()
#'    runShinyMlr()
#' }
#' @seealso downloadShinyMlr
#' @export
runShinyMlr = function(...) {
  assertList(extra.args, names = "named")
  runApp(appDir = system.file("application", package = "shinyMlr"), ...)
}
