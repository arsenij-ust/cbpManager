patientTab <- tabItem(
  tabName = "patient",
  div(style="display:inline-block",h2("Patient")),
  div(style="display:inline-block; padding-bottom:15px; margin-left:30px",bsButton(
    "tour_patient",
    label = "Tour",
    icon = icon("question"),
    style = "info",
    size = "extra-small"
  )),
  fluidRow(
    width = 12,
    column(6,
      id = "patient_description",
      box(
        title = "Description",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        includeMarkdown(system.file("apphelp", "descriptionPatientTab.md", package = "cbpManager")),
        width = NULL
      ),
    ),
    column(6,
      id = "patient_sample_img",
      box(
        title = "Sample from cBioPortal",
        collapsible = TRUE,
        collapsed = FALSE,
        solidHeader = TRUE,
        tags$head(
          tags$style(
            type = "text/css",
            "#patientDataImg img {max-width: 100%; width: 100%; height: auto}"
          )
        ),
        imageOutput("patientDataImg", height = "auto"),
        width = NULL
      )
    )
  ),
  fluidRow(
    id = "patient_main",
    width = 12,
    box(
      title = "Patient manager",
      actionButton("NewPatient", "Add patient", icon = icon("plus", lib = "glyphicon")),
      actionButton("EditPatient", "Edit patient", icon = icon("pencil", lib = "glyphicon")),
      actionButton("ImportPatient", "Import patient", icon = icon("save", lib = "glyphicon")),
      actionButton("DeletePatient", "Delete patient", icon = icon("remove", lib = "glyphicon")),
      actionButton("AddColumnPatient", "Add column(s)", icon = icon("plus-sign", lib = "glyphicon")),
      actionButton("DeleteColumnPatient", "Delete column(s)", icon = icon("minus-sign", lib = "glyphicon")),
      actionButton("SaveDataPatient", "Save", class = "btn-success", icon = icon("saved", lib = "glyphicon")),
      br(), br(),
      DT::DTOutput("patientTable"),
      width = 12
    )
  )
)
