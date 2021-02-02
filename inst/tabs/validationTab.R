validationTab <- tabItem(
  tabName = "validation",
  h2("Validate study"),
  fluidRow(
    width = 12,
    box(
      title = "Run validation", width = 12,
      actionButton("runValidation", "Validate", class = "btn-success"),
      downloadButton("downloadValidation", "Download"),
      htmlOutput("validation")
    )
    # box(
    #   title="Description",
    #   htmltools::HTML(
    #     "<p><b>Check if all required files exist to successfully upload the study files.</b>"
    #   ),
    #   width = 12
    # ),
    # box(
    #   title="Run validation",
    #   tags$head(tags$style('.valid {color:green;} .invalid {color:red;}')),
    #   actionButton("runValidation", "Validate", class = "btn-success"),
    #   width = 12,
    #   uiOutput("validateUI")
    # )
  )
)
