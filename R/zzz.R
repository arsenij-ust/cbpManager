#' @importFrom shiny addResourcePath

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
}
