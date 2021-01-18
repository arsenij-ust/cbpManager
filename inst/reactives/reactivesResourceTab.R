# image
output$resourceDataImg <- renderImage({
  return(
    list(
      src = system.file("www", "resource_data.PNG", package = "cbpManager"),
      contentType = "image/png",
      alt = "timeline-example",
      width = "auto"
    )
  )
}, deleteFile = FALSE)

# Resource definition ---------------------------------------------------------------
# Data table output
output$rcDefinitionTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_definition,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE
    )))
  )
})

# add Resource definition entry ---------------------------------------------------------------
rcDefinition_addRow <- callModule(
  module = addRowRc_Server,
  id = "Define_rc",
  data = reactive(loadedData$data_resource_definition),
  patient_ids = NULL,
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "definition"
)
observe({
  loadedData$data_resource_definition <- rcDefinition_addRow()
})

# edit Resource definition entry ---------------------------------------------------------------
rcDefinition_editRow <- callModule(
  module = editRowRc_Server,
  id = "Define_rc",
  data = reactive(loadedData$data_resource_definition),
  patient_ids = NULL,
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "definition",
  selected_row = reactive(input$rcDefinitionTable_rows_selected)
)
observe({
  loadedData$data_resource_definition <- rcDefinition_editRow()
})

# delete Resource definition entry ---------------------------------------------------------------
rcDefinition_delRow <- callModule(
  module = deleteRowRc_Server,
  id = "Define_rc",
  data = reactive(loadedData$data_resource_definition),
  selected_row = reactive(input$rcDefinitionTable_rows_selected),
  sample_data = reactive(loadedData$data_resource_sample),
  patient_data = reactive(loadedData$data_resource_patient),
  study_data = reactive(loadedData$data_resource_study),
  mode="recursive"
)
observe({
  loadedData$data_resource_definition <- rcDefinition_delRow()$df
  loadedData$data_resource_sample <- rcDefinition_delRow()$sample
  loadedData$data_resource_patient <- rcDefinition_delRow()$patient
  loadedData$data_resource_study <- rcDefinition_delRow()$study
})

# save Resource definition data ---------------------------------------------------------------
callModule(
  module = saveResource_Server,
  id = "Define_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_definition),
  data_filename = "data_resource_definition.txt",
  meta_filename = "meta_resource_definition.txt",
  resource_type = "DEFINITION"
)

# Resource study ---------------------------------------------------------------
output$rcStudyTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_study,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE
    )))
  )
})

# add Resource study entry ---------------------------------------------------------------
rcStudy_addRow <- callModule(
  module = addRowRc_Server,
  id = "Study_rc",
  data = reactive(loadedData$data_resource_study),
  patient_ids = NULL,
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "study"
)
observe({
  loadedData$data_resource_study <- rcStudy_addRow()
})

# edit Resource study entry ---------------------------------------------------------------
rcStudy_editRow <- callModule(
  module = editRowRc_Server,
  id = "Study_rc",
  data = reactive(loadedData$data_resource_study),
  patient_ids = NULL,
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "study",
  selected_row = reactive(input$rcStudyTable_rows_selected)
)
observe({
  loadedData$data_resource_study <- rcStudy_editRow()
})

# delete Resource study entry ---------------------------------------------------------------
rcStudy_delRow <- callModule(
  module = deleteRow_Server,
  id = "Study_rc",
  data = reactive(loadedData$data_resource_study),
  selected_row = reactive(input$rcStudyTable_rows_selected)
)
observe({
  loadedData$data_resource_study <- rcStudy_delRow()
})

# save Resource study data ---------------------------------------------------------------
callModule(
  module = saveResource_Server,
  id = "Study_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_study),
  data_filename = "data_resource_study.txt",
  meta_filename = "meta_resource_study.txt",
  resource_type = "STUDY"
)

# Resource patient ---------------------------------------------------------------
output$rcPatientTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_patient,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE
    )))
  )
})

# add Resource patient entry ---------------------------------------------------------------
rcPatient_addRow <- callModule(
  module = addRowRc_Server,
  id = "Patients_rc",
  data = reactive(loadedData$data_resource_patient),
  patient_ids = reactive(patient_id_list$ids),
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "patient"
)
observe({
  loadedData$data_resource_patient <- rcPatient_addRow()
})

# edit Resource patient entry ---------------------------------------------------------------
rcPatient_editRow <- callModule(
  module = editRowRc_Server,
  id = "Patients_rc",
  data = reactive(loadedData$data_resource_patient),
  patient_ids = reactive(patient_id_list$ids),
  sample_ids = NULL,
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "patient",
  selected_row = reactive(input$rcPatientTable_rows_selected)
)
observe({
  loadedData$data_resource_patient <- rcPatient_editRow()
})

# delete Resource patient entry ---------------------------------------------------------------
rcPatient_delRow <- callModule(
  module = deleteRow_Server,
  id = "Patients_rc",
  data = reactive(loadedData$data_resource_patient),
  selected_row = reactive(input$rcPatientTable_rows_selected)
)
observe({
  loadedData$data_resource_patient <- rcPatient_delRow()
})

# save Resource patient data ---------------------------------------------------------------
callModule(
  module = saveResource_Server,
  id = "Patients_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_patient),
  data_filename = "data_resource_patient.txt",
  meta_filename = "meta_resource_patient.txt",
  resource_type = "PATIENT"
)

# Resource sample ---------------------------------------------------------------
output$rcSampleTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_sample,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE
    )))
  )
})

# add Resource sample entry ---------------------------------------------------------------
rcSample_addRow <- callModule(
  module = addRowRc_Server,
  id = "Samples_rc",
  data = reactive(loadedData$data_resource_sample),
  patient_ids = reactive(patient_id_list$ids),
  sample_ids = reactive(sample_id_df$ids),
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "sample"
)
observe({
  loadedData$data_resource_sample <- rcSample_addRow()
})

# edit Resource sample entry ---------------------------------------------------------------
rcSample_editRow <- callModule(
  module = editRowRc_Server,
  id = "Samples_rc",
  data = reactive(loadedData$data_resource_sample),
  patient_ids = reactive(patient_id_list$ids),
  sample_ids = reactive(sample_id_df$ids),
  resource_ids = reactive(loadedData$data_resource_definition),
  resource_type = "sample",
  selected_row = reactive(input$rcSampleTable_rows_selected)
)
observe({
  loadedData$data_resource_sample <- rcSample_editRow()
})

# delete Resource sample entry ---------------------------------------------------------------
rcSample_delRow <- callModule(
  module = deleteRow_Server,
  id = "Samples_rc",
  data = reactive(loadedData$data_resource_sample),
  selected_row = reactive(input$rcSampleTable_rows_selected)
)
observe({
  loadedData$data_resource_sample <- rcSample_delRow()
})

# save Resource sample data ---------------------------------------------------------------
callModule(
  module = saveResource_Server,
  id = "Samples_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_sample),
  data_filename = "data_resource_sample.txt",
  meta_filename = "meta_resource_sample.txt",
  resource_type = "SAMPLE"
)

