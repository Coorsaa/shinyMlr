#' Run a local instance of shinyMlr
#'
#' @param path [\code{character(1)}]\cr
#'   The directory containing the application folder.
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
runShinyMlr = function(path = getwd(), ...) {
  assertCharacter(path, len = 1L)
  extra.args = list(...)
  assertList(extra.args, names = "named",
    .var.name = "runApp arguments")
  path = makeAppPath(path)
  runApp(path, ...)
}
