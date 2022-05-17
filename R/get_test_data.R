#' Get path to example data
#'
#' There are sample data in `inst/extdata`
#' directory. This function make them easy to access
#' Code reused from tidyverse/readr
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' gridr_example()
#' gridr_example("best.gpkg")
gridr_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "gridr"))
  } else {
    system.file("extdata", file, package = "gridr", mustWork = TRUE)
  }
}
