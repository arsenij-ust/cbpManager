resourceTab <- tabItem(
  tabName = "resource",
  h2("Resources"),
  fluidRow(
    column(width = 4,
           box(width = NULL,
               title="Description",
               collapsible = TRUE,
               collapsed = TRUE,
               solidHeader = TRUE,
               includeMarkdown(system.file("www", "descriptionResourceTab.md", package = "cbpManager")),
           )
    ),
    column(width = 8,
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
    width = 12,
    box(width = 12,
        title="Define resource types",
        actionButton("AddEntry_rc", label = "Add", icon=icon("plus",lib="glyphicon")),
        actionButton("EditEntry_rc", label = "Edit", icon=icon("pencil",lib="glyphicon")),
        actionButton("DeleteEntry_rc", label = "Delete", icon=icon("remove",lib="glyphicon")),
        actionButton("SaveTimeline_rc", label = "Save",  class = "btn-success", icon=icon("saved",lib="glyphicon")),
        br(),
        br(),
        DT::DTOutput("resource_definition")
    )
  ),
  tabsetPanel(
    type="tabs",
    id="resource_tabs",
    tabPanel(title="Study",
             fluidRow(
               width = 12,
               box(
                 title="Resources of the study",
                 addRow_UI("Study_rc"),
                 editRow_UI("Study_rc"),
                 deleteRow_UI("Study_rc"),
                 saveTimeline_UI("Study_rc"),
                 br(), br(),
                 DT::DTOutput("Study_rc_table"),
                 width = 12
               )
             )
    ),
    tabPanel(title="Patients",
             fluidRow(
               width = 12,
               box(
                 title="Resources of patients",
                 addRow_UI("Patients_rc"),
                 editRow_UI("Patients_rc"),
                 deleteRow_UI("Patients_rc"),
                 saveTimeline_UI("Patients_rc"),
                 br(), br(),
                 DT::DTOutput("Patients_rc_table"),
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
                 editRow_UI("Samples_rc"),
                 deleteRow_UI("Samples_rc"),
                 saveTimeline_UI("Samples_rc"),
                 br(), br(),
                 DT::DTOutput("Samples_rc_table"),
                 width = 12
               )
             )
    )
  )
)
