#' @title
#' Launch cbpManager
#'
#' @description
#' Launch the cbpManager Shiny application.
#'
#' @param studyDir Path to study folder containing studies of cBioPortal.
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#' @param logDir Path where a logfile should be saved. If NULL, logs are
#' not stored
#' @return shiny application object
#'
#' @examples
#' if (interactive()) {
#'   launchApp()
#' }
#' @export launchApp


# wrapper for shiny::shinyApp()
launchApp <- function(
                      studyDir = NULL,
                      logDir = NULL,
                      ...) {

  ## --------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ## --------------------------------------------------------------------------##
  cbpManager.options <<- list(
    "studyDir" = studyDir,
    "logDir" = logDir,
    "cbpManager_root" = system.file(package = "cbpManager")
  )

  ## --------------------------------------------------------------------------##
  ## Launch cbpManager
  ## --------------------------------------------------------------------------##
  message(
    paste0(
      "##---------------------------------------------------------------------------##\n",
      "## Launching cbpManager\n",
      "##---------------------------------------------------------------------------##"
    )
  )
  app <- shiny::shinyApp(
    ui = shinyAppUI,
    server = shinyAppServer
  )
  runApp(app, ...)
}
