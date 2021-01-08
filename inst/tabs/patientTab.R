patientTab <- tabItem(
  tabName = "patient",
  h2("Patient"),
  fluidRow(
    width = 12,
    box(
      title="Description",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      includeMarkdown(system.file("www", "descriptionPatientTab.md", package = "cbpManager")),
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
        "#patientDataImg img {max-width: 100%; width: 100%; height: auto}"
      )
    ),
    imageOutput("patientDataImg", height = "auto"),
    width = 6
  )
  ),
  fluidRow(
    width = 12,
    box(
      title="Patient manager",
      actionButton("NewPatient", "Add patient", icon=icon("plus",lib="glyphicon")),
      actionButton("EditPatient", "Edit patient", icon=icon("pencil",lib="glyphicon")),
      actionButton("ImportPatient", "Import patient", icon=icon("save",lib="glyphicon")),
      actionButton("DeletePatient", "Delete patient", icon=icon("remove",lib="glyphicon")),
      actionButton("AddColumnPatient", "Add column(s)", icon=icon("plus-sign",lib="glyphicon")),
      actionButton("DeleteColumnPatient", "Delete column(s)", icon=icon("minus-sign",lib="glyphicon")),
      actionButton("SaveDataPatient", "Save", class = "btn-success", icon=icon("saved",lib="glyphicon")),
      br(), br(),
      DT::DTOutput("patientTable"),
      width = 12
    )
  )
)
