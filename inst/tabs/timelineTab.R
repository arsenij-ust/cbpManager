timelineTab <- tabItem(
  tabName = "timelines",
  h2("Timelines"),
  fluidRow(
    column(width = 4,
           box(width = NULL,
               title="Description",
               collapsible = TRUE,
               collapsed = TRUE,
               solidHeader = TRUE,
               includeMarkdown(system.file("www", "descriptionTimelineTab.md", package = "cbpManager")),
           )
    ),
    column(width = 8,
           box(width = NULL,
               title="Sample from cBioPortal",
               collapsible = TRUE,
               collapsed = FALSE,
               solidHeader = TRUE,
               tags$head(
                 tags$style(
                   type="text/css",
                   "#timelineDataImg img {max-width: 100%; width: 100%; height: auto}"
                 )
               ),
               imageOutput("timelineDataImg", height = "auto")
           ),
           box(width = NULL,
               title = "Add date of the first diagnosis to a Patient ID",
               actionButton("datesAdd", "Add date"),
               actionButton("datesEdit", "Edit date"),
               actionButton("datesDelete", "Delete date"),
               actionButton("datesSave", "Save", class = "btn-success"),
               br(), br(),
               DT::DTOutput("dateTable")
               )
           )
    ),
  tabsetPanel(
    type="tabs",
    id="timeline_tabs",
    tabPanel(title="Treatment",
             fluidRow(
               width = 12,
               box(
                 title="Add treatment to timeline",
                 addRow_UI("Treatment"),
                 editRow_UI("Treatment"),
                 deleteRow_UI("Treatment"),
                 addColumn_UI("Treatment"),
                 deleteColumn_UI("Treatment"),
                 saveTimeline_UI("Treatment"),
                 br(), br(),
                 DT::DTOutput("treatmentTable"),
                 tableOutput("data"),
                 width = 12
               )
             )
    ),
    tabPanel(title="Surgery",
             fluidRow(
               width = 12,
               box(
                 title="Add surgery to timeline",
                 addRow_UI("Surgery"),
                 editRow_UI("Surgery"),
                 deleteRow_UI("Surgery"),
                 addColumn_UI("Surgery"),
                 deleteColumn_UI("Surgery"),
                 saveTimeline_UI("Surgery"),
                 br(), br(),
                 DT::DTOutput("surgeryTable"),
                 width = 12
               )
             )
    ),
    tabPanel(title="Status",
             fluidRow(
               width = 12,
               box(
                 title="Add status to timeline",
                 addRow_UI("Status"),
                 editRow_UI("Status"),
                 deleteRow_UI("Status"),
                 addColumn_UI("Status"),
                 deleteColumn_UI("Status"),
                 saveTimeline_UI("Status"),
                 br(), br(),
                 DT::DTOutput("statusTable"),
                 width = 12
               )
             )
    )
  ),
  fluidRow(
    width = 12,
    box(
      width = 12,
      title="Add custom timeline tracks",
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        textInput(
          "customTrackID",
          label = NULL,
          width = "400px",
          placeholder = "Name of timeline track")),
      div(style="display: inline-block;vertical-align:top",
          radioButtons("timelineMode", label = NULL,
                       choices = list(
                         "timeline",
                         "timepoint"),
                       selected = 1)),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        actionButton("addTrack", "Add track")),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        uiOutput("selectTrackUI")),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        actionButton("editTrack", "Edit track", class = "btn-success")),

    ),
    box(width = 12,
        actionButton("AddEntry_ct", label = "Add", icon=icon("plus",lib="glyphicon")),
        actionButton("EditEntry_ct", label = "Edit", icon=icon("pencil",lib="glyphicon")),
        actionButton("DeleteEntry_ct", label = "Delete", icon=icon("remove",lib="glyphicon")),
        actionButton("AddColumn_ct", label = "Add column", icon=icon("plus-sign",lib="glyphicon")),
        actionButton("DeleteColumn_ct", label = "Delete column", icon=icon("minus-sign",lib="glyphicon")),
        actionButton("SaveTimeline_ct", label = "Save",  class = "btn-success", icon=icon("saved",lib="glyphicon")),
        br(),
        br(),
        DT::DTOutput("Table_ct")
    )
  )
)
