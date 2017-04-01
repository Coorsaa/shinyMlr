#' Download an instance of shinyMlr to your machine
#'
#' @param path The directory to download the application to
#' @export
#' @importFrom git2r clone
#' @examples
#' \dontrun{
#'     downloadShinyMlr()
#' }
downloadShinyMlr = function(path = getwd()) {
    message("Downloading shinyMlr to ", path, "/shinyMlr")
    path = file.path(path, "shinyMlr")
    clone("https://github.com/gammarama/shinyMlr",
      local_path = path, branch = "application")
}
