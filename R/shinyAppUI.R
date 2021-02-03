#' Shiny app server object
#' create the shiny application user interface

shinyAppUI <- dashboardPage(
  dashboardHeader(title = "cbpManager"),
  dashboardSidebar(
    sidebarMenu(
      # logo
      tags$div(style = "position: absolute; top: -100px;", textOutput("clock")),
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
      menuItem("Study Metadata", tabName = "meta_study"),
      menuItem("Patient", tabName = "patient"),
      menuItem("Sample", tabName = "sample"),
      menuItem("Mutations", tabName = "mutations"),
      menuItem("Timelines", tabName = "timelines")
      # ,
      # menuItem("Validation", tabName = "validation")
    )
  ),
  dashboardBody(
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
      timelineTab

      # ,
      # Tab 6 Validation - validationTab.R
      # validationTab
    )
  )
)
