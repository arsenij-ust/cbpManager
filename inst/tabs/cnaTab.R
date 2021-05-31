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
    column(
      width = 12,
      id = "cna_metadata",
      box(
        title = "Change metadata of copy number alteration",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        width = NULL,
        column(
          6,
          div(
            div(
              textInput(inputId = "cna_profile_name", label = "Change profile name:")
            ),
            div(
              style = "display: inline-block;vertical-align: middle;",
              popify(
                bsButton(
                  "q8",
                  label = "",
                  icon = icon("question"),
                  style = "info",
                  size = "extra-small"
                ),
                "Profile name",
                "Designation of the input data."
              )
            )
          ),
          div(
            div(
              textInput(inputId = "cna_profile_description", label = "Change profile description:")
            ),
            div(
              style = "display: inline-block;vertical-align: middle;",
              popify(
                bsButton(
                  "q9",
                  label = "",
                  icon = icon("question"),
                  style = "info",
                  size = "extra-small"
                ),
                "Profile description",
                "Key for the copy number level specification used for each gene-sample combination."
              )
            )
          ),
          actionButton("saveMetadata", "Save metadata", class = "btn-success")
        ),
        column(
          6,
          h5(strong("Current profile name:")),
          verbatimTextOutput("curr_profile_name"),
          
          h5(strong("Current profile description:")),
          verbatimTextOutput("curr_profile_description")
        )
      )
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

