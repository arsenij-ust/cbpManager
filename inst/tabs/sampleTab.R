sampleTab <- tabItem(
  tabName = "sample",
  div(style="display:inline-block",h2("Sample")),
  div(style="display:inline-block; padding-bottom:15px; margin-left:30px",bsButton(
    "tour_sample",
    label = "Tour",
    icon = icon("question"),
    style = "info",
    size = "extra-small"
  )),
  fluidRow(
    width = 12,
    column(
      width = 6,
      id = "sample_description",
      box(
        title = "Description",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        includeMarkdown(system.file("apphelp", "descriptionSampleTab1.md", package = "cbpManager")),
        width = NULL
      )
    ),
    column(
      width = 6,
      id = "sample_img",
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
        imageOutput("sampleDataImg", height = "auto"),
        width = NULL
      )
    )
  ),
  fluidRow(
    id = "sample_col_description",
    width = 12,
    box(
      title = "Columns",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      column(
        6,
        includeMarkdown(system.file("apphelp", "descriptionSampleTab2.md", package = "cbpManager")),
      ),
      column(
        6,
        includeMarkdown(system.file("apphelp", "descriptionSampleTab3.md", package = "cbpManager")),
      ),
      width = 12
    )
  ),
  fluidRow(
    id = "sample_main",
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
