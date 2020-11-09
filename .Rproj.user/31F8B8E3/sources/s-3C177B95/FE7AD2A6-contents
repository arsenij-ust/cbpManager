#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'


# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {

  # workaround for websocket connection timeout ####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })

  study_dir <<- file.path(system.file(package = "cbpManager"), "study")

  # Tab 1 Study Metadata ---------------------------------------------------------------

  # oncotree cancer types
  oncotree <<- jsonlite::fromJSON(system.file("www", "oncotree.json", package = "cbpManager"))
  oncotree$code <<- tolower(oncotree$code)
  unique(oncotree$tissue)
  tumor_tissue_site <<- unique(oncotree$tissue[which(!is.na(oncotree$tissue))])
  cancer_type <<- unique(oncotree$mainType[which(!is.na(oncotree$mainType))])
  oncotree_code <<- unique(oncotree$code[which(!is.na(oncotree$code))])
  cancer_type_detailed <<- unique(oncotree$name[which(!is.na(oncotree$name))])

  # generate study-wide patient list

  source(system.file("reactives", "reactivesStudyTab.R", package = "cbpManager"), local=TRUE)

  # Tab 2 Patient ---------------------------------------------------------------

  # read table with predefined colnames
  patientCols <- read.table(system.file("www", "predefined_patient_cols.tsv", package = "cbpManager"), header = TRUE, sep = "\t")

  source(system.file("reactives", "reactivesPatientTab.R", package = "cbpManager"), local=TRUE)

  # Tab 3 Sample ---------------------------------------------------------------
  # read table with predefined colnames
  sampleCols <- read.table(system.file("www", "predefined_sample_cols.tsv", package = "cbpManager"), header = TRUE, sep = "\t")

  source(system.file("reactives", "reactivesSampleTab.R", package = "cbpManager"), local=TRUE)

  # Tab 4 Mutations ---------------------------------------------------------------
  source(system.file("reactives", "reactivesMutationsTab.R", package = "cbpManager"), local=TRUE)

  # Tab 5 Timelines  ---------------------------------------------------------------
  source(system.file("reactives", "reactivesTimelineTab.R", package = "cbpManager"), local=TRUE)

  # Tab 6 Validation  ---------------------------------------------------------------
  source(system.file("reactives", "reactivesValidationTab.R", package = "cbpManager"), local=TRUE)
}
