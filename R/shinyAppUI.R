#' Shiny app server object
#' create the shiny application user interface

shinyAppUI <- dashboardPage(
  dashboardHeader(
    title = "cbpManager",
    tags$li(a(
      href = "https://arsenij-ust.github.io/cbpManager/index.html",
      icon("home"),
      title = "Homepage",
      target="_blank",
      rel="noopener noreferrer",
      style = "cursor: pointer;"),
      class = "dropdown"),
    tags$li(a(
      href = "https://arsenij-ust.github.io/cbpManager/articles/intro.html",
      icon("question-circle"),
      title = "Vignette",
      target="_blank",
      rel="noopener noreferrer",
      style = "cursor: pointer;"),
      class = "dropdown"),
    tags$li(a(
      href = "https://github.com/arsenij-ust/cbpManager",
      icon("github"),
      title = "Github repository",
      target="_blank",
      rel="noopener noreferrer",
      style = "cursor: pointer;"),
      class = "dropdown"),
    tags$li(actionLink("openSessionInfo",
                       label = "",
                       icon = icon("clipboard-list"),
                       title = "Session info"), class = "dropdown"),
    tags$li(actionLink("openAbout",
                       label = "",
                       icon = icon("info"),
                       title = "About cbpManager"), class = "dropdown")
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
      menuItem("Resources", tabName = "resource"),
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
    uiOutput("ui_loaded_study_info"),
    div(style = "width: 230px; padding-left:15px; padding-right:15px", DT::DTOutput("ui_change_tracker")),
    tags$style(".fa-check-circle {color:#00a65a}"),
    tags$style(".fa-exclamation-circle {color:#e74c3c}"),
    textOutput("package_version"),
    tags$head(
      tags$style("
      #package_version {
        position: absolute;
        bottom: 0;
        align: center;
        padding-left: 80px;
      }

    ")
    )
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

      # Tab Resource data - resourceTab.R
      resourceTab,

      # Tab 6 Validation - validationTab.R
      validationTab
    )
  )
)
