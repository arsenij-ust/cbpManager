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
      htmltools::HTML(
        "
        <p><b>Add clinical data, which are shown in cBioPortal in form of the table</b> (see image on the right.)</b></p></br>
        <p><b>Data:</b></p>
        <p>The first two rows contain a <b>short name</b>  (1st row) and a <b>longer name</b> (2nd row) of the clinical attribute.</p>
        <ul>
        <li>The first column <b>'PATIENT_ID'</b> is <b>obligatory</b> and should contain a unique patient identifier. (Please do not override the cells containing 'Patient Identifier' and 'Patient identifier' in the first column.)
        </li>
        </ul>
        <p>Several pre-defined attributes are described in the following:</p>
        <ul>
        <li><b>'OS_STATUS'</b>:   Overall patient survival status. Possible values: 1:DECEASED, 0:LIVING</li>
        <li><b>'OS_MONTHS'</b>:   Overall survival in months since initial diagnosis.</li>
        <li><b>'DFS_STATUS'</b>:  Disease free status since initial treatment. Possible values: 0:DiseaseFree, 1:Recurred/Progressed</li>
        <li><b>'DFS_MONTHS'</b>:  Disease free (months) since initial treatment.</li>
        <li><b>'PATIENT_DISPLAY_NAME'</b>: Patient display name (string).</li>
        <li><b>'GENDER'</b>:      Gender or sex of the patient (string).</li>
        <li><b>'AGE'</b>:         Age at which the condition or disease was first diagnosed, in years (number).</li>
        </ul>
        </br>

        <p><b>Edit table:</b></p>
        <ul>
        <li><b>Add patient:</b> opens a dialog window with input fileds per column of the table.</li>
        <li><b>Edit patient:</b> select a row by clicking on it and then click on 'Edit patient' to edit the values in this row.</li>
        <li><b>Delete patient:</b> select a row by clicking on it and then click on 'Delete patient' to delete the selected entry.</li>
        <li><b>Add column(s):</b> after clicking on 'Add column' choose whether you want to add a pre-defined column or create a custom column. In case of a custom column, you need to provide a 'column name', a 'short name' and a 'long name' for this column.</li>
        <li><b>Delete column(s):</b> choose the columns you want to delete.</li>
        <li><b>Save:</b> saves the edited table to the final file.</li>
        </ul>
        "
      ),
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
