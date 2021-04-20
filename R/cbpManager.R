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
#' @param returnAppObj Logical value, whether to return the app object if set to
#' TRUE. Default behavior: directly runs the app (FALSE)
#'
#' @return shiny application object
#'
#' @examples
#' if (interactive()) {
#'   cbpManager()
#' }
#' @export cbpManager
# wrapper for shiny::shinyApp()
cbpManager <- function(
                      studyDir = NULL,
                      logDir = NULL,
                      returnAppObj = FALSE,
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
    "##---------------------------------------------------------------------------##\n",
    "## Launching cbpManager\n",
    "##---------------------------------------------------------------------------##"
  )
  
  # set max file size for MAF-file upload - 1GB
  options(shiny.maxRequestSize = 1024 * 1024^2)
  
  app <- shiny::shinyApp(
    ui = shinyAppUI,
    server = shinyAppServer
  )

  if(returnAppObj){
    return(app)
  }

  runApp(app, ...)
}
