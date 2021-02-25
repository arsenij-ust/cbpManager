validationTab <- tabItem(
  tabName = "validation",
  h2("Validate study"),
  fluidRow(
    width = 12,
    box(title="Run validation",width = 12,
      includeMarkdown(system.file("www", "descriptionValidationTab.md", package = "cbpManager")),
      actionButton("runValidation", "Validate", class = "btn-success"),
      downloadButton("downloadValidation", "Download"),
      #uiOutput("validateUI"),
      #htmlOutput("validation")
      #verbatimTextOutput("value")
    ),
    box(
      title="Results",
      width = 12,
      uiOutput("validateUI")
    )
  )
)
