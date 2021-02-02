sampleTab <- tabItem(
  tabName = "sample",
  h2("Sample"),
  fluidRow(
    width = 12,
    box(
      title = "Description",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      includeMarkdown(system.file("www", "descriptionSampleTab1.md", package = "cbpManager")),
      width = 6
    ),
    box(
      title = "Sample from cBioPortal",
      collapsible = TRUE,
      collapsed = FALSE,
      solidHeader = TRUE,
      tags$head(
        tags$style(
          type = "text/css",
          "#sampleDataImg img {max-width: 100%; width: 100%; height: auto}"
        )
      ),
      imageOutput("sampleDataImg", height = "auto"), width = 6
    )
  ),
  fluidRow(
    width = 12,
    box(
      title = "Columns",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      column(
        6,
        includeMarkdown(system.file("www", "descriptionSampleTab2.md", package = "cbpManager")),
      ),
      column(
        6,
        includeMarkdown(system.file("www", "descriptionSampleTab3.md", package = "cbpManager")),
      ),
      width = 12
    )
  ),
  fluidRow(
    width = 12,
    box(
      title = "Sample manager",
      actionButton("NewSample", "Add sample", icon = icon("plus", lib = "glyphicon")),
      actionButton("EditSample", "Edit sample", icon = icon("pencil", lib = "glyphicon")),
      actionButton("DeleteSample", "Delete sample", icon = icon("remove", lib = "glyphicon")),
      actionButton("AddColumnSample", "Add column(s)", icon = icon("plus-sign", lib = "glyphicon")),
      actionButton("DeleteColumnSample", "Delete column(s)", icon = icon("minus-sign", lib = "glyphicon")),
      actionButton("SaveDataSample", "Save", class = "btn-success", icon = icon("saved", lib = "glyphicon")),
      br(), br(),
      DT::DTOutput("sampleTable"),
      width = 12
    )
  )
)
