#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @return nothing to return
# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {

  # logo  ---------------------------------------------------------------
  output$logo <- renderImage(
    {
      return(
        list(
          src = system.file("www", "logo.png", package = "cbpManager"),
          contentType = "image/png",
          alt = "cbpManager_logo",
          width = "150px", height = "auto", style = "display: block; margin-left: auto; margin-right: auto;"
        )
      )
    },
    deleteFile = FALSE
  )

  # workaround for websocket connection timeout ####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })

  ## take path from 'cbpManager.options' if it is set
  if (
    exists("cbpManager.options") &&
      !is.null(cbpManager.options[["studyDir"]]) &&
      dir.exists(cbpManager.options[["studyDir"]])
  ) {
    study_dir <<- cbpManager.options$studyDir

    ## if none of the above apply, get path of example study dir
  } else {
    study_dir <<- system.file("study", package = "cbpManager")
  }

  ## take path from 'cbpManager.options' if it is set
  if (
    exists("cbpManager.options") &&
      !is.null(cbpManager.options[["logDir"]]) &&
      dir.exists(cbpManager.options[["logDir"]])
  ) {
    logDir <<- cbpManager.options$logDir

    ## if none of the above apply, get path of example study dir
  } else {
    logDir <<- NULL
  }
  
  # Modal dialog for Session info button
  observeEvent(input$openSessionInfo, {
    showModal(
      modalDialog(
        title = "Session information",
        size = "l",
        fade = TRUE,
        footer = NULL,
        easyClose = TRUE,
        tagList(tags$code("> sessionInfo()"),
                renderPrint({
                  sessionInfo()
                }))
      )
    )
  })
  
  # Modal dialog for Session info button
  observeEvent(input$openAbout, {
    showModal(
      modalDialog(
        title = "About cbpManager",
        size = "l",
        fade = TRUE,
        footer = NULL,
        easyClose = TRUE,
        tagList(includeMarkdown(
          system.file("apphelp", "about.md", package = "cbpManager")
        ),
        renderPrint({
          citation("cbpManager")
        }))
      )
    )
  })


  # Tab 1 Study Metadata ---------------------------------------------------------------
  # oncotree cancer types
  oncotree <<- jsonlite::fromJSON(system.file("extdata", "oncotree.json", package = "cbpManager"))
  oncotree$code <<- tolower(oncotree$code)
  tumor_tissue_site <<- unique(oncotree$tissue[which(!is.na(oncotree$tissue))])
  cancer_type <<- unique(oncotree$mainType[which(!is.na(oncotree$mainType))])
  oncotree_code <<- unique(oncotree$code[which(!is.na(oncotree$code))])
  cancer_type_detailed <<- unique(oncotree$name[which(!is.na(oncotree$name))])

  # generate study-wide patient list
  source(system.file("reactives", "reactivesStudyTab.R", package = "cbpManager"), local = TRUE)
  # Tab 2 Patient ---------------------------------------------------------------
  # read table with predefined colnames
  patientCols <- read.table(system.file("extdata", "predefined_patient_cols.tsv", package = "cbpManager"), header = TRUE, sep = "\t")
  source(system.file("reactives", "reactivesPatientTab.R", package = "cbpManager"), local = TRUE)
  # Tab 3 Sample ---------------------------------------------------------------
  # read table with predefined colnames
  sampleCols <- read.table(system.file("extdata", "predefined_sample_cols.tsv", package = "cbpManager"), header = TRUE, sep = "\t")
  source(system.file("reactives", "reactivesSampleTab.R", package = "cbpManager"), local = TRUE)
  # Tab 4 Mutations ---------------------------------------------------------------
  source(system.file("reactives", "reactivesMutationsTab.R", package = "cbpManager"), local = TRUE)
  # Tab 5 Timelines  ---------------------------------------------------------------
  source(system.file("reactives", "reactivesTimelineTab.R", package = "cbpManager"), local = TRUE)
  # Tab 6 Resource data  ---------------------------------------------------------------
  source(system.file("reactives", "reactivesResourceTab.R", package = "cbpManager"), local=TRUE)
  # Tab 7 Validation  ---------------------------------------------------------------
  source(system.file("reactives", "reactivesValidationTab.R", package = "cbpManager"), local=TRUE)
}
