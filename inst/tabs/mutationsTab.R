mutationsTab <- tabItem(
  tabName = "mutations",
  h2("Mutations"),
  fluidRow(
    width = 12,
    box(
      title="Description",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      includeMarkdown(system.file("www", "descriptionMutationTab.md", package = "cbpManager")),
      width = 6
      ),
    box(
      title="Sample from cBioPortal",
      collapsible = TRUE,
      collapsed = FALSE,
      solidHeader = TRUE,
      tags$head(
        tags$style(
          type="text/css",
          "#MutDataImg img {max-width: 100%; width: 100%; height: auto}"
        )
      ),
      imageOutput(
        "MutDataImg",
        height = "auto"
      ),
      width = 6
    )
    ),
  fluidRow(
    width = 12,
    box(
      column(
        3,
        fileInput("chooseMAF", "Choose MAF File",
                  multiple = FALSE,
                  accept = c("text/tsv",
                             "text/tab-separated-values,text/plain",
                             ".tsv", ".txt", ".maf", ".MAF"
                  )
        ),
        actionButton("saveMAF", "Save MAF file", class = "btn-success")
      ),
      column(
        9,
        DT::DTOutput("MAFdata")
      ),
      width = 12
    )
  )
)
