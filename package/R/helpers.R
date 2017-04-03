# handle imports
#' @import checkmate
#' @import shiny
#' @importFrom git2r clone

# FIXME: Check if more conservative import of shiny makes sense.

makeAppPath = function(path) {
  file.path(path, "shinyMlr")
}
