sampleTab <- tabItem(
  tabName = "sample",
  h2("Sample"),
  fluidRow(
    width = 12,
    box(
      title="Description",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      htmltools::HTML(
        "<p><b>Add sample data to patients, which are shown in cBioPortal in form of the table</b> (see image on the right.)</br>The first two rows contain a <b>short name</b>  (1st row) and a <b>longer name</b> (2nd row) of the sample attribute.</p>
        <p><b>Edit table:</b></p>
        <ul>
        <li><b>Add sample:</b> opens a dialog window with input fileds per column of the table.</li>
        <li><b>Edit sample:</b> select a row by clicking on it and then click on 'Edit patient' to edit the values in this row.</li>
        <li><b>Delete sample:</b> select a row by clicking on it and then click on 'Delete sample' to delete the selected entry.</li>
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
          "#sampleDataImg img {max-width: 100%; width: 100%; height: auto}"
        )
      ),
      imageOutput("sampleDataImg", height = "auto"), width = 6
    )
    ),
  fluidRow(
    width = 12,
    box(
      title="Columns",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      column(
        6,
        htmltools::HTML(
          "<h4>Required columns</h4>
          <ul>
          <li>The first column <b>'PATIENT_ID'</b> is <b>obligatory</b> and contains patient identifier. (Please do not override the cells containing 'Patient Identifier' and 'Patient identifier' in the first column.) </li><li>The second column <b>'SAMPLE_ID'</b> is also <b>required</b> and contains a unique sample identifier.
          </li>
          </ul>
          <p>By adding 'PATIENT_ID' here, cBioPortal will map the given sample to this patient. This enables one to associate multiple samples to one patient. For example, a single patient may have had multiple biopsies, each of which has been genomically profiled. </p>
          <h4>Optional columns</h4>
          <p>Further columns contain variable sample attributes. You can select a predefined attribute. These attributes are described in the following. Optionally you can set a custom named atttribute.
          </br>
          </br>
          The following columns are required for the pan-cancer summary statistics tab:</p>
          <ul>
          <li><b>'CANCER_TYPE'</b>: Overrides study wide cancer type.</li>
          <li><b>'CANCER_TYPE_DETAILED'</b>: Cancer Type Detailed, a sub-type of the specified 'CANCER_TYPE'.</li>
          </ul>"
        )
        ),
      column(
        6,
        htmltools::HTML(
          "<p>The following columns affect the header of the patient view by adding text to the samples in the header:</p>
          <ul>
          <li><b>'SAMPLE_DISPLAY_NAME'</b>: displayed in addition to the ID.</li>
          <li><b>'SAMPLE_CLASS'</b></li>
          <li><b>'METASTATIC_SITE'</b> or <b>'PRIMARY_SITE'</b>: Override 'TUMOR_SITE' (patient level attribute) depending on sample type.</li>
          </ul>
          <p>The following columns additionally affect the Timeline data visualization:</p>
          <ul>
          <li><b>'OTHER_SAMPLE_ID'</b></li>
          <li><b>'SAMPLE_TYPE'</b>, <b>'TUMOR_TISSUE_SITE'</b> or <b>'TUMOR_TYPE'</b>: gives sample icon in the timeline a color.
          <ul>
          <li>If set to recurrence, recurred, progression or progressed: orange</li>
          <li>If set to metastatic or metastasis: red</li>
          <li>If set to primary or otherwise: black-</li>
          </ul>
          </li>
          </ul>
          </p>"
        )
        ),
      width = 12
        )
      ),
  fluidRow(
    width = 12,
    box(
      title="Sample manager",
      actionButton("NewSample", "Add sample", icon=icon("plus",lib="glyphicon")),
      actionButton("EditSample", "Edit sample", icon=icon("pencil",lib="glyphicon")),
      actionButton("DeleteSample", "Delete sample", icon=icon("remove",lib="glyphicon")),
      actionButton("AddColumnSample", "Add column(s)", icon=icon("plus-sign",lib="glyphicon")),
      actionButton("DeleteColumnSample", "Delete column(s)", icon=icon("minus-sign",lib="glyphicon")),
      actionButton("SaveDataSample", "Save", class = "btn-success", icon=icon("saved",lib="glyphicon")),
      br(), br(),
      DT::DTOutput("sampleTable"),
      width = 12
    )
  )
  # fluidRow(
  #   width = 12,
  #   box(
  #     column(
  #       3,
  #       selectInput(
  #         "sampleSelectSpecificColumn",
  #         label = "Add predefined attribute",
  #         choices = c("SAMPLE_DISPLAY_NAME",
  #                     "SAMPLE_CLASS",
  #                     "METASTATIC_SITE",
  #                     "PRIMARY_SITE",
  #                     "OTHER_SAMPLE_ID",
  #                     "SAMPLE_TYPE",
  #                     "TUMOR_TISSUE_SITE",
  #                     "TUMOR_TYPE",
  #                     patientSampleCols
  #         )
  #       ),
  #       actionButton("sampleAddSpecificColumn", "Add predefined Attribute")
  #     ),
  #     column(
  #       3,
  #       textInput(
  #         "sampleCustomColumn",
  #         label = "Set name of custom attriburte",
  #         value = "ATTRIBUTE"
  #       ),
  #       actionButton("sampleAddCustomColumn", "Add custom Attribute")
  #     ),
  #     column(
  #       3,
  #       selectInput(
  #         "sampleSelDelCol",
  #         "Select column for deletion",
  #         choices = NULL),
  #       actionButton("sampleDelCol", "Delete column")
  #     ),
  #     column(
  #       3,
  #       actionButton("sampleAddRow", "Add sample"),
  #       actionButton("sampleSave", "Save changes", class = "btn-success")
  #     ),
  #     DTOutput("sampleTable"),
  #     width = 12
  #   )
  # )
)
