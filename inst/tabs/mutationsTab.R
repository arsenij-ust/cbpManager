mutationsTab <- tabItem(
  tabName = "mutations",
  div(style="display:inline-block",h2("Mutation")),
  div(style="display:inline-block; padding-bottom:15px; margin-left:30px",bsButton(
    "tour_mutation",
    label = "Tour",
    icon = icon("question"),
    style = "info",
    size = "extra-small"
  )),
  fluidRow(
    width = 12,
    column(
      width = 6,
      id = "mutation_description",
      box(
        title = "Description",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        includeMarkdown(system.file("apphelp", "descriptionMutationTab.md", package = "cbpManager")),
        width = NULL
      )
    ),
    column(
      width = 6,
      id = "mutation_img",
      box(
        title = "Sample from cBioPortal",
        collapsible = TRUE,
        collapsed = FALSE,
        solidHeader = TRUE,
        tags$head(
          tags$style(
            type = "text/css",
            "#MutDataImg img {max-width: 100%; width: 100%; height: auto}"
          )
        ),
        imageOutput(
          "MutDataImg",
          height = "auto"
        ),
        width = NULL
      )
    )
  ),
  fluidRow(
    title = "Mutation data manager",
    id = "chooseMAFDiv",
    width = 12,
    box(
      column(
        3,
        div(id = "chooseMAFDiv",
            fileInput("chooseMAF", "Choose MAF File",
                      multiple = FALSE,
                      accept = c(
                        "text/tsv",
                        "text/tab-separated-values,text/plain",
                        ".tsv", ".txt", ".maf", ".MAF"
                      )
            )
        ),
        actionButton("AddPreview", "Add uploaded data to existing mutation data", class = "btn-success")
      ),
      column(
        9,
        DT::DTOutput("previewMAF")
      ),
      width = 12
    )
  ),
  fluidRow(
    id = "maf_main",
    width = 12,
    box(
      actionButton("NewMAFentry", "Add annotation", icon = icon("plus", lib = "glyphicon")),
      actionButton("EditMAFentry", "Edit annotation", icon = icon("pencil", lib = "glyphicon")),
      actionButton("DeleteMAFentry", "Delete annotation", icon = icon("remove", lib = "glyphicon")),
      actionButton("AddColumnMAFentry", "Add column(s)", icon = icon("plus-sign", lib = "glyphicon")),
      actionButton("DeleteColumnMAFentry", "Delete column(s)", icon = icon("minus-sign", lib = "glyphicon")),
      actionButton("saveMAF", "Save", class = "btn-success", icon = icon("saved", lib = "glyphicon")),
      br(), br(),
      DT::DTOutput("mafTable"),
      width = 12
    )
  )
)
