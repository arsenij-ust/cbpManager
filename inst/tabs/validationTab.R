validationTab <- tabItem(
  tabName = "validation",
  h2("Validate study"),
  fluidRow(
    width = 12,
    box(title="Run validation",width = 12,
      includeMarkdown(system.file("apphelp", "descriptionValidationTab.md", package = "cbpManager")),
      actionButton("runValidation", "Validate", class = "btn-success")
    ),
    box(
      title="Results",
      width = 12,
      uiOutput("validateUI") %>% withSpinner()
    )
  )
)
