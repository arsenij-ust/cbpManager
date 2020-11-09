patientTab <- tabItem(
  tabName = "patient",
  h2("Patient"),
  fluidRow(
    width = 12,
    box(
      title="Description",
      htmltools::HTML(
        "<p><b>Add data to patients, which are shown in cBioPortal in form of the table</b> (see image on the right.)
        </br>
        The first two rows contain a <b>short name</b>  (1st row) and a <b>longer name</b> (2nd row) of the clinical attribute.
        <ul>
        <li>The first column <b>'Patient_ID'</b> is <b>obligatory</b> and should contain a unique patient identifier. (Please do not override the cells containing 'Patient Identifier' and 'Patient identifier' in the first column.)
        </li>
        </ul>
        Further columns contain variable clinical attributes. You can select a predefined attribute. These attributes are described in the following. Optionally you can set a custom named atttribute.
        <ul>
        <li><b>'OS_STATUS'</b>: Overall patient survival status. Possible values: 1:DECEASED, 0:LIVING</li>
        <li><b>'OS_MONTHS'</b>: Overall survival in months since initial diagnosis.</li>
        <li><b>'DFS_STATUS'</b>: Disease free status since initial treatment. Possible values: 0:DiseaseFree, 1:Recurred/Progressed</li>
        <li><b>'DFS_MONTHS'</b>: Disease free (months) since initial treatment.</li>
        <li><b>'PATIENT_DISPLAY_NAME'</b>: Patient display name (string).</li>
        <li><b>'GENDER'</b>: Gender or sex of the patient (string).</li>
        <li><b>'AGE'</b>: Age at which the condition or disease was first diagnosed, in years (number).</li>
        </ul>
        <b>Double-click</b> on a cell in the table to edit the table.
        </br>
        To confirm the input press <b>'CTRL+Enter'</b>.
        </br>
        Add a new patient (row) by clicking on the button <b>'Add Patient'</b>.
        </br>
        <b>'Save changes'</b> saves the edited table to the final file.</p>"
                        ),
                        width = 6
                    ),
                    box(
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
