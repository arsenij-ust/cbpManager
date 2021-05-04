cnaTab <- tabItem(
  tabName = "cna",
  div(style="display:inline-block",h2("Copy Number Data")),
  div(style="display:inline-block; padding-bottom:15px; margin-left:30px",bsButton(
    "tour_cna",
    label = "Tour",
    icon = icon("question"),
    style = "info",
    size = "extra-small"
  )),
  fluidRow(
    width = 12,
    column(
      width = 6,
      id = "cna_description",
      box(
        title = "Description",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        includeMarkdown(system.file("apphelp", "descriptionCnaTab.md", package = "cbpManager")),
        width = NULL
      )
    ),
    column(
      width = 6,
      id = "cna_img",
      box(
        title = "Sample from cBioPortal",
        collapsible = TRUE,
        collapsed = FALSE,
        solidHeader = TRUE,
        tags$head(
          tags$style(
            type = "text/css",
            "#CopyNumberDataImg img {max-width: 100%; width: 100%; height: auto}"
          )
        ),
        imageOutput(
          "CopyNumberDataImg",
          height = "auto"
        ),
        width = NULL
      )
    )
  ),
  fluidRow(
    width = 12,
    box(
      title = "Change default settings",
      column(
        3,
        checkboxInput("cna_checkbox", "Change global description", value = FALSE)
      ),
      column(
        9,
        conditionalPanel(
          condition = 'input.cna_checkbox == 1',
          textInput(inputId = "cna_description", label = "Change global description")
        )
      ),
      width = 12
    )
  ),
  fluidRow(
    width = 12,
    box(
      column(
        3,
        div(id = "chooseCNADiv",
            fileInput("chooseCNA", "Choose Copy Number Data File",
                      multiple = FALSE,
                      accept = c(
                        "text/tsv",
                        "text/tab-separated-values,text/plain",
                        ".tsv", ".txt"
                      )
            )
        ),
        actionButton("saveCNA", "Save file", class = "btn-success")
      ),
      column(
        9,
        DT::DTOutput("CNAdata")
      ),
      width = 12
    )
  )
)