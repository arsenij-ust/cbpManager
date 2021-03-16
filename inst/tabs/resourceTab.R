resourceTab <- tabItem(
  tabName = "resource",
  div(style="display:inline-block",h2("Resources")),
  div(style="display:inline-block; padding-bottom:15px; margin-left:30px",bsButton(
    "tour_resource",
    label = "Tour",
    icon = icon("question"),
    style = "info",
    size = "extra-small"
  )),
  fluidRow(
    column(width = 4,
           id = "resource_description",
           box(width = NULL,
               title="Description",
               collapsible = TRUE,
               collapsed = TRUE,
               solidHeader = TRUE,
               includeMarkdown(system.file("apphelp", "descriptionResourceTab.md", package = "cbpManager")),
           )
    ),
    column(width = 8,
           id = "resource_img",
           box(width = NULL,
               title="Example view from cBioPortal",
               collapsible = TRUE,
               collapsed = FALSE,
               solidHeader = TRUE,
               tags$head(
                 tags$style(
                   type="text/css",
                   "#resourceDataImg img {max-width: 100%; width: 100%; height: auto}"
                 )
               ),
               imageOutput("resourceDataImg", height = "auto")
           )
    )
  ),
  fluidRow(
    id = "define_resources",
    width = 12,
    box(width = 12,
        title="Define resource types",
        addRowRc_UI("Define_rc"),
        editRowRc_UI("Define_rc"),
        deleteRowRc_UI("Define_rc"),
        saveResource_UI("Define_rc"),
        br(),
        br(),
        DT::DTOutput("rcDefinitionTable")
    )
  ),
  div(
    id = "resources_tabs",
    tabsetPanel(
      type="tabs",
      tabPanel(title="Study",
               fluidRow(
                 width = 12,
                 box(
                   title="Resources of the study",
                   addRowRc_UI("Study_rc"),
                   editRowRc_UI("Study_rc"),
                   deleteRow_UI("Study_rc"),
                   saveResource_UI("Study_rc"),
                   br(), br(),
                   DT::DTOutput("rcStudyTable"),
                   width = 12
                 )
               )
      ),
      tabPanel(title="Patients",
               fluidRow(
                 width = 12,
                 box(
                   title="Resources of patients",
                   addRowRc_UI("Patients_rc"),
                   editRowRc_UI("Patients_rc"),
                   deleteRow_UI("Patients_rc"),
                   saveResource_UI("Patients_rc"),
                   br(), br(),
                   DT::DTOutput("rcPatientTable"),
                   width = 12
                 )
               )
      ),
      tabPanel(title="Samples",
               fluidRow(
                 width = 12,
                 box(
                   title="Resources of samples",
                   addRow_UI("Samples_rc"),
                   editRowRc_UI("Samples_rc"),
                   deleteRow_UI("Samples_rc"),
                   saveResource_UI("Samples_rc"),
                   br(), br(),
                   DT::DTOutput("rcSampleTable"),
                   width = 12
                 )
               )
      )
    )
  )
)
