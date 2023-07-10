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

# tour  ---------------------------------------------------------------
observeEvent(input$tour_resource, {
  tour <- read.delim(system.file("apphelp", "tour_resource.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# Resource definition ---------------------------------------------------------------
# Data table output
output$rcDefinitionTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_definition,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25)
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
observeEvent(rcDefinition_addRow(),{
  loadedData$data_resource_definition <- rcDefinition_addRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(rcDefinition_editRow(),{
  loadedData$data_resource_definition <- rcDefinition_editRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
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

observeEvent(rcDefinition_delRow()$df,{
  loadedData$data_resource_definition <- rcDefinition_delRow()$df
  loadedData$data_resource_sample <- rcDefinition_delRow()$sample
  loadedData$data_resource_patient <- rcDefinition_delRow()$patient
  loadedData$data_resource_study <- rcDefinition_delRow()$study
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save Resource definition data ---------------------------------------------------------------
rcDefinition_save <- callModule(
  module = saveResource_Server,
  id = "Define_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_definition),
  data_filename = "data_resource_definition.txt",
  meta_filename = "meta_resource_definition.txt",
  resource_type = "DEFINITION"
)
observeEvent(rcDefinition_save(),{
  # change tracker
  check <- rcDefinition_save()
  study_tracker$df[5, "Saved"] <- as.character(icon("check-circle"))
})


# Resource study ---------------------------------------------------------------
output$rcStudyTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_study,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25)
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
observeEvent(rcStudy_addRow(),{
  loadedData$data_resource_study <- rcStudy_addRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(rcStudy_editRow(),{
  loadedData$data_resource_study <- rcStudy_editRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete Resource study entry ---------------------------------------------------------------
rcStudy_delRow <- callModule(
  module = deleteRow_Server,
  id = "Study_rc",
  data = reactive(loadedData$data_resource_study),
  selected_row = reactive(input$rcStudyTable_rows_selected)
)
observeEvent(rcStudy_delRow(),{
  loadedData$data_resource_study <- rcStudy_delRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save Resource study data ---------------------------------------------------------------
rcStudy_save <- callModule(
  module = saveResource_Server,
  id = "Study_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_study),
  data_filename = "data_resource_study.txt",
  meta_filename = "meta_resource_study.txt",
  resource_type = "STUDY"
)
observeEvent(rcStudy_save(),{
  # change tracker
  check <- rcStudy_save()
  study_tracker$df[5, "Saved"] <- as.character(icon("check-circle"))
})

# Resource patient ---------------------------------------------------------------
output$rcPatientTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_patient,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25)
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
observeEvent(rcPatient_addRow(),{
  loadedData$data_resource_patient <- rcPatient_addRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(rcPatient_editRow(),{
  loadedData$data_resource_patient <- rcPatient_editRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete Resource patient entry ---------------------------------------------------------------
rcPatient_delRow <- callModule(
  module = deleteRow_Server,
  id = "Patients_rc",
  data = reactive(loadedData$data_resource_patient),
  selected_row = reactive(input$rcPatientTable_rows_selected)
)
observeEvent(rcPatient_delRow(),{
  loadedData$data_resource_patient <- rcPatient_delRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save Resource patient data ---------------------------------------------------------------
rcPatient_save <- callModule(
  module = saveResource_Server,
  id = "Patients_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_patient),
  data_filename = "data_resource_patient.txt",
  meta_filename = "meta_resource_patient.txt",
  resource_type = "PATIENT"
)
observeEvent(rcPatient_save(),{
  # change tracker
  check <- rcPatient_save()
  study_tracker$df[5, "Saved"] <- as.character(icon("check-circle"))
})

# Resource sample ---------------------------------------------------------------
output$rcSampleTable <- DT::renderDT({
  DT::datatable(
    loadedData$data_resource_sample,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25)
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
observeEvent(rcSample_addRow(),{
  loadedData$data_resource_sample <- rcSample_addRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(rcSample_editRow(),{
  loadedData$data_resource_sample <- rcSample_editRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete Resource sample entry ---------------------------------------------------------------
rcSample_delRow <- callModule(
  module = deleteRow_Server,
  id = "Samples_rc",
  data = reactive(loadedData$data_resource_sample),
  selected_row = reactive(input$rcSampleTable_rows_selected)
)
observeEvent(rcSample_delRow(),{
  loadedData$data_resource_sample <- rcSample_delRow()
  # change tracker
  study_tracker$df[5, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save Resource sample data ---------------------------------------------------------------
rcSample_save <- callModule(
  module = saveResource_Server,
  id = "Samples_rc",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_resource_sample),
  data_filename = "data_resource_sample.txt",
  meta_filename = "meta_resource_sample.txt",
  resource_type = "SAMPLE"
)
observeEvent(rcSample_save(),{
  # change tracker
  check <- rcSample_save()
  study_tracker$df[5, "Saved"] <- as.character(icon("check-circle"))
})
