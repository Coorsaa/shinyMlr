#' Download an instance of shinyMlr to your machine
#'
#' @param path [\code{character(1)}]\cr
#'   The directory to download the application to.
#' @examples
#' \dontrun{
#'     downloadShinyMlr()
#' }
#' @seealso runShinyMlr
#' @export
downloadShinyMlr = function(path = getwd()) {
  assertCharacter(path, len = 1L)
  path = makeAppPath(path)
  message("Downloading shinyMlr to ", path)
  clone("https://github.com/mlr-org/shinyMlr",
    local_path = path, branch = "master")
}
