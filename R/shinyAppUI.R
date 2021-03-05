#' Shiny app server object
#' create the shiny application user interface

shinyAppUI <- dashboardPage(
  dashboardHeader(
    title = "cbpManager",
    tags$li(a(
      href = "https://arsenij-ust.github.io/cbpManager/index.html",
      icon("home"),
      title = "Homepage",
      style = "cursor: pointer;"),
      class = "dropdown"),
    tags$li(a(
      href = "https://arsenij-ust.github.io/cbpManager/articles/intro.html",
      icon("question-circle"),
      title = "Vignette",
      style = "cursor: pointer;"),
      class = "dropdown"),
    tags$li(a(
      href = "https://github.com/arsenij-ust/cbpManager",
      icon("github"),
      title = "Github repository",
      style = "cursor: pointer;"),
      class = "dropdown")
  ),
  dashboardSidebar(
    rintrojs::introjsUI(),
    sidebarMenu(
      tags$div(style = "position: absolute; top: -100px;", textOutput("clock")),
      # logo
      tags$head(
        tags$style(
          type = "text/css",
          "#logo {
          max-width: 150px;
          width: 150px;
          height: auto;
          display: block;
          margin-left: auto;
          margin-right: auto;}"
        )
      ),
      imageOutput("logo", height = "auto", width = "150px"),
      # Sidebar items
      menuItem("Study", tabName = "meta_study"),
      menuItem("Patient", tabName = "patient"),
      menuItem("Sample", tabName = "sample"),
      menuItem("Mutations", tabName = "mutations"),
      menuItem("Timelines", tabName = "timelines"),
      menuItem("Validation", tabName = "validation")
    ),
    tags$br(),
    tags$head(
      tags$style(
        type = "text/css",
        "#ui_loaded_study_info {
          color: black;}"
      )
    ),
    uiOutput("ui_loaded_study_info")
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("#shiny-notification-panel {
             position:fixed;
             top: calc(50%);
             left: calc(50% - 150px);
             }
             "
        )
      )
    ),
    tabItems(
      # Tab 1 Study Metadata - studyTab.R
      studyTab,

      # Tab 2 Patient - patientTab.R
      patientTab,

      # Tab 3 Sample - sampleTab.R
      sampleTab,

      # Tab 4 Mutations - mutationsTab.R
      mutationsTab,

      # Tab 5 Timelines - timelineTab.R
      timelineTab,

      # Tab 6 Validation - validationTab.R
      validationTab
    )
  )
)
