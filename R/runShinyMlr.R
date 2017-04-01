#' Run a local instance of shinyMlr
#'
#' @param path The directory containing the intRo folder
#' @param ... Additional arguments passed to shiny's
#'   \code{runApp()} function.
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#'   library(shinyMlr)
#'   downloadShinyMlr()
#'    runShinyMlr()
#' }
runShinyMlr = function(path = getwd(), ...) {
    path = file.path(path, "shinyMlr")
    runApp(path, ...)
}
