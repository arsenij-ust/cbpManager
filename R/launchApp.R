#' @title
#' Launch cbpManager
#'
#' @description
#' Launch the cbpManager Shiny application.
#'
#' @param studyDir Path to study folder containing studies of cBioPortal.
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#' @return shiny application object
#'
#' @examples
#' if ( interactive() ) {
#'   launchApp()
#' }
#'
#' @import shiny
#' @import shinydashboard
#' @export launchApp


# wrapper for shiny::shinyApp()
launchApp <- function(
  studyDir = NULL,
  ...
) {

  ##--------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ##--------------------------------------------------------------------------##
  cbpManager.options <- list(
    "studyDir" = studyDir,
    "cbpManager_root" = system.file(package="cbpManager")
  )

  ##--------------------------------------------------------------------------##
  ## Launch cbpManager
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching cbpManager\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(
    ui = shinyAppUI,
    server = shinyAppServer,
    ...)
}
