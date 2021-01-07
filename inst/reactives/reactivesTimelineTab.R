# image
output$timelineDataImg <- renderImage({
  return(
    list(
      src = system.file("www", "timeline.png", package = "cbpManager"),
      contentType = "image/png",
      alt = "timeline-example",
      width = "auto"
    )
  )
}, deleteFile = FALSE)

# dates of first diagnosis ---------------------------------------------------------------
# Data table output
output$dateTable <- DT::renderDT({
  if (!is.null(loadedData$dates_first_diagnosis)) {
    DT::datatable(
      loadedData$dates_first_diagnosis,
      escape = FALSE,
      selection = 'single',
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      )
    )
  }
})

# add patient date

# show modalDialog for new date entry
observeEvent(input$datesAdd, {
  showModal(
    modalDialog(
      size = "m",
      title = "Add date of first diagnosis",
      p("The patient should be present in the Patient tab."),
      uiOutput("AddPatientDateUI"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonAddDate", "Add")
      )
    )
  )
})

output$AddPatientDateUI <- renderUI({
  patIDs <- patient_id_list$ids
  fluidPage(fluidRow(column(
    width = 8,
    selectInput(
      inputId = "AddPatientIDDate",
      label = "Select PATIENT_ID",
      choices = c("", unique(patIDs[which(!is.na(patIDs))])),
      selected = 1
    ),
    dateInput(
      inputId = "AddPatientDate",
      label = "Date of first diagnosis",
      format = "dd.mm.yyyy"
    )
  )))
})

observeEvent(input$ModalbuttonAddDate, {
  if (input$AddPatientIDDate == "") {
    showNotification("PATIENT_ID cannot be empty.",
                     type = "error",
                     duration = NULL)
  } else if (is.null(input$AddPatientDate)) {
    showNotification("Date cannot be empty.",
                     type = "error",
                     duration = NULL)
  } else if (input$AddPatientIDDate %in% loadedData$dates_first_diagnosis$PATIENT_ID) {
    showNotification(
      "This PATIENT_ID has already a date. If you want to change the date, please use the 'Edit date' button.",
      type = "error",
      duration = NULL
    )
  } else {
    loadedData$dates_first_diagnosis <-
      rbind(
        loadedData$dates_first_diagnosis,
        list(
          PATIENT_ID = input$AddPatientIDDate,
          DATE = as.character(input$AddPatientDate)
        )
      )
    removeModal()
  }
})

# edit date
observeEvent(input$datesEdit, {
  if (is.null(input$dateTable_rows_selected)) {
    showNotification("Please select a row.",
                     type = "warning",
                     duration = NULL)
  } else {
    showModal(
      modalDialog(
        size = "m",
        title = "Edit date of first diagnosis",
        uiOutput("EditPatientDateUI"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonEditDate", "Edit")
        )
      )
    )
  }
})

output$EditPatientDateUI <- renderUI({
  patIDs <- patient_id_list$ids
  fluidPage(fluidRow(column(
    width = 8,
    selectInput(
      inputId = "EditPatientIDDate",
      label = "Select PATIENT_ID",
      choices = c("", unique(patIDs[which(!is.na(patIDs))])),
      selected = loadedData$dates_first_diagnosis[input$dateTable_rows_selected, "PATIENT_ID"]
    ),
    dateInput(
      inputId = "EditPatientDate",
      label = "Edit date of first diagnosis",
      format = "dd.mm.yyyy",
      value = loadedData$dates_first_diagnosis[input$dateTable_rows_selected, "DATE"]
    )
  )))
})

observeEvent(input$ModalbuttonEditDate, {
  if (input$AddPatientIDDate == "") {
    showNotification("PATIENT_ID cannot be empty.",
                     type = "error",
                     duration = NULL)
  } else if (is.null(input$AddPatientDate)) {
    showNotification("Date cannot be empty.",
                     type = "error",
                     duration = NULL)
  } else {
    loadedData$dates_first_diagnosis[which(loadedData$dates_first_diagnosis$PATIENT_ID ==
                                             input$EditPatientIDDate), "DATE"] <-
      as.character(input$EditPatientDate)
    removeModal()
  }
})

# delete date entry
observeEvent(input$datesDelete, {
  if (is.null(input$dateTable_rows_selected)) {
    showNotification("Please select a row.",
                     type = "warning",
                     duration = NULL)
  } else {
    showModal(
      modalDialog(
        "Do you want to delete the selected date entry?",
        title = "Delete",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonDeleteDate", "Delete")
        )
      )
    )
  }
})

observeEvent(input$ModalbuttonDeleteDate, {
  entry <- input$dateTable_rows_selected
  loadedData$dates_first_diagnosis <-
    loadedData$dates_first_diagnosis[-entry, , drop = FALSE]
  removeModal()
})

# safe dates of first diagnosis
observeEvent(input$datesSave, {
  write.table(
    loadedData$dates_first_diagnosis,
    file.path(
      "study",
      loadedData$studyID,
      "dates_first_diagnosis.txt.temp"
    ),
    append = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
  file.rename(
    file.path(
      study_dir,
      loadedData$studyID,
      "dates_first_diagnosis.txt.temp"
    ),
    file.path(study_dir, loadedData$studyID, "dates_first_diagnosis.txt")
  )
  # logging
  writeLogfile(
    outdir = study_dir,
    modified_file = file.path(loadedData$studyID, "dates_first_diagnosis.txt")
  )
  showNotification("Diagnosis dates saved successfully!",
                   type = "message",
                   duration = 10)
})

# treatment timeline ---------------------------------------------------------------
# Data table output
output$treatmentTable <- DT::renderDT({
  hidenColsTreatment <-
    which(colnames(loadedData$data_timeline_surgery) %in% c("EVENT_TYPE")) - 1
  DT::datatable(
    loadedData$data_timeline_treatment,
    selection = "single",
    rownames = F,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE,
      targets = c(hidenColsTreatment)
    )))
  )
})

# add treatmemt entry ---------------------------------------------------------------
treatment_addRow <- callModule(
  module = addRow_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis)
)
observe({
  loadedData$data_timeline_treatment <- treatment_addRow()
})

# edit treatment entry ---------------------------------------------------------------
treatment_editRow <- callModule(
  module = editRow_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
  selected_row = reactive(input$treatmentTable_rows_selected)
)
observe({
  loadedData$data_timeline_treatment <- treatment_editRow()
})

# delete treatmemt entry ---------------------------------------------------------------
treatment_delRow <- callModule(
  module = deleteRow_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  selected_row = reactive(input$treatmentTable_rows_selected)
)
observe({
  loadedData$data_timeline_treatment <- treatment_delRow()
})

# delete treatmemt column ---------------------------------------------------------------

treatment_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_treatment <- treatment_delCol()
})

# add treatmemt column ---------------------------------------------------------------

treatment_addCol <- callModule(
  module = addColumn_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment)
)
observe({
  loadedData$data_timeline_treatment <- treatment_addCol()
})

# save treatment data ---------------------------------------------------------------
callModule(
  module = saveTimeline_Server,
  id = "Treatment",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_treatment)
)
# logging
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "data_timeline_treatment.txt")
)
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "meta_timeline_treatment.txt")
)
# surgery timeline ---------------------------------------------------------------
# Data table output
output$surgeryTable <- DT::renderDT({
  hidenColsSurgery <-
    which(colnames(loadedData$data_timeline_surgery) %in% c("EVENT_TYPE", "STOP_DATE")) - 1
  DT::datatable(
    loadedData$data_timeline_surgery,
    selection = "single",
    rownames = F,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE, targets = c(hidenColsSurgery)
    )))
  )
})

# add surgery entry ---------------------------------------------------------------
surgery_addRow <- callModule(
  module = addRow_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
  mode = "timepoint"
)
observe({
  loadedData$data_timeline_surgery <- surgery_addRow()
})

# edit surgery entry ---------------------------------------------------------------
surgery_editRow <- callModule(
  module = editRow_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
  selected_row = reactive(input$surgeryTable_rows_selected),
  mode = "timepoint"
)
observe({
  loadedData$data_timeline_surgery <- surgery_editRow()
})

# delete surgery entry ---------------------------------------------------------------
surgery_delRow <- callModule(
  module = deleteRow_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  selected_row = reactive(input$surgeryTable_rows_selected)
)
observe({
  loadedData$data_timeline_surgery <- surgery_delRow()
})

# delete surgery column ---------------------------------------------------------------
surgery_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_surgery <- surgery_delCol()
})

# add surgery column ---------------------------------------------------------------
surgery_addCol <- callModule(
  module = addColumn_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery)
)
observe({
  loadedData$data_timeline_surgery <- surgery_addCol()
})

# save surgery data ---------------------------------------------------------------
callModule(
  module = saveTimeline_Server,
  id = "Surgery",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_surgery)
)
# logging
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "data_timeline_surgery.txt")
)
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "meta_timeline_surgery.txt")
)
# status timeline ---------------------------------------------------------------
# Data table output
output$statusTable <- DT::renderDT({
  hidenColsSurgery <-
    which(colnames(loadedData$data_timeline_status) %in% c("EVENT_TYPE", "STOP_DATE")) - 1
  DT::datatable(
    loadedData$data_timeline_status,
    selection = "single",
    rownames = F,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE, targets = c(hidenColsSurgery)
    )))
  )
})

# add status entry ---------------------------------------------------------------
status_addRow <- callModule(
  module = addRow_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
  mode = "timepoint"
)
observe({
  loadedData$data_timeline_status <- status_addRow()
})

# edit status entry ---------------------------------------------------------------
status_editRow <- callModule(
  module = editRow_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  patient_ids = reactive(patient_id_list$ids),
  dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
  selected_row = reactive(input$statusTable_rows_selected),
  mode = "timepoint"
)
observe({
  loadedData$data_timeline_status <- status_editRow()
})

# delete status entry ---------------------------------------------------------------
status_delRow <- callModule(
  module = deleteRow_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  selected_row = reactive(input$statusTable_rows_selected)
)
observe({
  loadedData$data_timeline_status <- status_delRow()
})

# delete status column ---------------------------------------------------------------
status_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_status <- status_delCol()
})

# add status column ---------------------------------------------------------------
status_addCol <- callModule(
  module = addColumn_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status)
)
observe({
  loadedData$data_timeline_status <- status_addCol()
})

# save status data ---------------------------------------------------------------
callModule(
  module = saveTimeline_Server,
  id = "Status",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_status)
)
# logging
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "data_timeline_status.txt")
)
writeLogfile(
  outdir = study_dir,
  modified_file = file.path(loadedData$studyID, "meta_timeline_status.txt")
)
# custom tracks ---------------------------------------------------------------
# minimal data.frame of a timeline track
min_timeline_df <- data.frame(
  PATIENT_ID = character(),
  START_DATE = numeric(),
  STOP_DATE = numeric(),
  EVENT_TYPE = character()
)

# create reactive data.frame with timeline metadata


# get custom timeline names and timeline modes from the reactive study object and fill the data.frame
# observe({
#   timeline_dfs <- names(loadedData)[grep("data_timeline_", names(loadedData))]
#   excludeTimelines <-
#     c(
#       "data_timeline_surgery",
#       "data_timeline_status",
#       "data_timeline_treatment"
#     )
#   timeline_names <- timeline_dfs[-which(timeline_dfs %in% excludeTimelines)]
#   lapply(timeline_names, function(name){
#     if(!name %in% colnames(customTimelines$timelines)){
#       if(all(is.na(loadedData[[name]][,"STOP_DATE"]))){
#         mode <- "timepoint"
#       } else {
#         mode <- "timeline"
#       }
#       customTimelines$timelines[name,] <- list(name=name, shortName=gsub("data_timeline_", "", name), mode=mode)
#     }
#   })
#   print(customTimelines$timelines)
# })

# add new custom track
observeEvent(input$addTrack, {
  # check if name for new track exists
  if (input$customTrackID == "") {
    showNotification("Provide track name first.",
                     type = "error",
                     duration = NULL)
  } else {
    # sanitize track name
    ID <- .create_name(input$customTrackID, toupper = FALSE)

    #check if track name already exists
    if (ID %in% customTimelines$timelines$shortName){
      showNotification("Name for timeline already exists",
                       type = "error",
                       duration = NULL)
    # add track to the reactive study object
    } else {
      loadedData[[paste0("data_timeline_", ID)]] <- min_timeline_df
      mode <- input$timelineMode
      customTimelines$timelines[paste0("data_timeline_", ID),] <- list(name=paste0("data_timeline_", ID), shortName=ID, mode=mode, trigger=0)
    }
  }
})

# UI for timeline drop-down widget
output$selectTrackUI <- renderUI({
  selectInput("selectTrack", width = "400px", label = NULL, choices = customTimelines$timelines$shortName)
})


observeEvent(input$editTrack, {
  customTimelines$selectedTrack <- customTimelines$timelines[customTimelines$timelines$shortName==input$selectTrack,]$name
})

# Data table output
output$Table_ct <- DT::renderDT({
  req(customTimelines$selectedTrack)

  if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode == "timeline"){
    colsToHide <- c("EVENT_TYPE")
  } else {
    colsToHide <- c("STOP_DATE","EVENT_TYPE")
  }
  # find the index of the columns to hide
  hiddenCols <-
    which(colnames(loadedData[[customTimelines$selectedTrack]]) %in% colsToHide) - 1

  DT::datatable(
    loadedData[[customTimelines$selectedTrack]],
    selection = "single",
    rownames = F,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE, targets = c(hiddenCols)
    )))
  )
})

# add custom column ---------------------------------------------------------------
observeEvent(input$AddColumn_ct,{
  req(customTimelines$selectedTrack)
  showModal(
    modalDialog(
      title = "Add new column",
      uiOutput("ct_AddCol_UI"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ct_ModalbuttonAddCol", "Add column")
      )
    )
  )
})

output$ct_AddCol_UI <- renderUI({
  fluidRow(column(
    width = 8,
    textInput(inputId="ct_colname",
              label = "Column name:",
              placeholder = "e.g. ATTRIBUTE"
    ))
  )
})

observeEvent(input$ct_ModalbuttonAddCol, {
  data <- loadedData[[customTimelines$selectedTrack]]
  if(input$ct_colname == ""){
    showNotification("Column name cannot be empty.", type="error", duration = NULL)
  } else if(toupper(input$ct_colname) %in% colnames(data)){
    showNotification("Column already exists.", type="error", duration = NULL)
  } else {
    ct_colname <- .create_name(input$ct_colname)
    data %>% dplyr::mutate(!!(ct_colname) := "") -> loadedData[[customTimelines$selectedTrack]]
    removeModal()
  }
})

# delete custom column ---------------------------------------------------------------
observeEvent(input$DeleteColumn_ct, {
  req(customTimelines$selectedTrack)
  exclude <- c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")

  showModal(
    modalDialog(
      title = "Delete column(s)",
      fluidRow(column(
        width = 8,
        selectInput(
          inputId = "ct_DelColname",
          label = "Select column(s) for deletion:",
          choices = setdiff(colnames(loadedData[[customTimelines$selectedTrack]]), exclude),
          multiple = TRUE
        )
      )),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ct_ModalbuttonDeleteCol", "Delete column(s)")
      )
    )
  )
})

observeEvent(input$ct_ModalbuttonDeleteCol, {
  loadedData[[customTimelines$selectedTrack]] <-
    loadedData[[customTimelines$selectedTrack]][, !(names(loadedData[[customTimelines$selectedTrack]]) %in% input$ct_DelColname), drop = FALSE]
  removeModal()
})

# delete custom entry ---------------------------------------------------------------

observeEvent(input$DeleteEntry_ct, {
  req(customTimelines$selectedTrack)
  if(is.null(input$Table_ct_rows_selected)){
    showNotification("Please select a row", type="warning", duration = NULL)
  } else {
    showModal(modalDialog(
      "Do you want to delete the selected entry?",
      title = "Delete",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ct_ModalbuttonDeleteEntry", "Delete")
      )
    ))
  }
})

observeEvent(input$ct_ModalbuttonDeleteEntry, {
  entry <- input$Table_ct_rows_selected

  loadedData[[customTimelines$selectedTrack]] <- loadedData[[customTimelines$selectedTrack]][-entry,,drop = FALSE]
  removeModal()
})

# add custom entry ---------------------------------------------------------------

observeEvent(input$AddEntry_ct,{
  req(customTimelines$selectedTrack)
  showModal(
    modalDialog(
      title = "New entry",
      uiOutput("ct_addEntry_UI"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ct_ModalbuttonAdd", "Add")
      )
    )
  )
})

output$ct_addEntry_UI <- renderUI({
  lapply(colnames(loadedData[[customTimelines$selectedTrack]]),
         function(colname){

           if(colname == "PATIENT_ID"){
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = "Select the Patient ID",
                 choices = unique(patient_id_list$ids[which(!is.na(patient_id_list$ids))]),
                 selected = 1
               ),
             ))
           } else if (colname == "EVENT_TYPE") {

           } else if (colname == "START_DATE") {
             fluidRow(column(
               width = 8,
               dateInput(
                 inputId=colname,
                 label = colname,
                 format = "dd.mm.yyyy"
               ),
             ))
           } else if (colname == "STOP_DATE") {
             if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode == "timeline"){
               fluidRow(column(
                 width = 8,
                 dateInput(
                   inputId=colname,
                   label = colname,
                   format = "dd.mm.yyyy"
                 ),
               ))
             }
           } else if (colname == "TREATMENT_TYPE") {
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = colname,
                 choices = c("Medical Therapy", "Radiation Therapy"),
                 selected = 1),
             ))
           } else if (colname == "SUBTYPE") {
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = colname,
                 choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"),
                 selected = 1),
             ))
           } else {
             fluidRow(column(
               width = 8,
               textInput(inputId=colname,
                         label = colname,
                         value = ""),))
           }
         })
})

# validate inputs in modalDialog and add new entry to table
observeEvent(input$ct_ModalbuttonAdd, {
  data <- loadedData[[customTimelines$selectedTrack]]
  dates <- loadedData$dates_first_diagnosis
  all_reactive_inputs <- reactiveValuesToList(input)
  addPatientValues <-  all_reactive_inputs[names(all_reactive_inputs) %in% names(data)]
  addPatientValues$EVENT_TYPE <- toupper(customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack,]$shortName)
  if(addPatientValues["PATIENT_ID"] == ""){
    showNotification("'Patient ID' is requiered.", type="error", duration = NULL)
  } else if(!addPatientValues["PATIENT_ID"] %in% dates$PATIENT_ID){
    showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type="error", duration = NULL)
  } else {
    diagnosisDate <- dates[which(dates$PATIENT_ID == addPatientValues["PATIENT_ID"]),"DATE"]
    if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode =="timeline") stopDate <- as.Date(addPatientValues[["STOP_DATE"]]) else stopDate <- NULL
    failed <- check_input_dates(
      as.Date(diagnosisDate),
      as.Date(addPatientValues[["START_DATE"]]),
      stopDate
    )
    if(failed == 1){
      showNotification("'End date' cannot be earlier as 'start date'.", type="error", duration = NULL)
    } else if(failed == 2){
      showNotification("'Start date' and 'End date' cannot be earlier as diagnosis date.", type="error", duration = NULL)
    } else {
      # add row to data_timeline_treatment
      start <- as.numeric(as.Date(addPatientValues[["START_DATE"]]) - as.Date(diagnosisDate))
      if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode =="timeline") end <- as.numeric(as.Date(addPatientValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
      addPatientValues$START_DATE <- start
      addPatientValues$STOP_DATE <- end
      loadedData[[customTimelines$selectedTrack]] <- plyr::rbind.fill(data, as.data.frame(addPatientValues))
      removeModal()
    }
  }
})

# edit custom entry ---------------------------------------------------------------
observeEvent(input$EditEntry_ct,{
  req(customTimelines$selectedTrack)
  if(is.null(input$Table_ct_rows_selected)){
    showNotification("Please select a row.", type="warning", duration = NULL)
  } else {
    showModal(
      modalDialog(
        title = "Edit entry",
        uiOutput("ct_editEntry_UI"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ct_ModalbuttonEdit", "Edit")
        )
      )
    )
  }
})

output$ct_editEntry_UI <- renderUI({
  selected_row <- input$Table_ct_rows_selected
  data <- loadedData[[customTimelines$selectedTrack]]
  patient_ids <- patient_id_list$ids
  lapply(colnames(data),
         function(colname){

           if(colname == "PATIENT_ID"){
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = "Select the Patient ID",
                 choices = unique(patient_ids[which(!is.na(patient_ids))]),
                 selected = data[selected_row, colname]
               ),
             ))
           } else if (colname == "EVENT_TYPE") {

           } else if (colname == "START_DATE") {
             fluidRow(column(
               width = 8,
               dateInput(
                 inputId = colname,
                 label = colname,
                 format = "dd.mm.yyyy"
               ),
             ))
           } else if (colname == "STOP_DATE") {
             if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode == "timeline"){
               fluidRow(column(
                 width = 8,
                 dateInput(
                   inputId = colname,
                   label = colname,
                   format = "dd.mm.yyyy"
                 ),
               ))
             }
           } else if (colname == "TREATMENT_TYPE") {
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = colname,
                 choices = c("Medical Therapy", "Radiation Therapy"),
                 selected = data[selected_row, colname]),
             ))
           } else if (colname == "SUBTYPE") {
             fluidRow(column(
               width = 8,
               selectInput(
                 inputId=colname,
                 label = colname,
                 choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"),
                 selected = data[selected_row, colname]),
             ))
           } else {
             fluidRow(column(
               width = 8,
               textInput(inputId=colname,
                         label = colname,
                         value = data[selected_row, colname]),))
           }
         })
})

# validate inputs in modalDialog and edit entry to table
observeEvent(input$ct_ModalbuttonEdit, {
  selected_row <- input$Table_ct_rows_selected
  data <- loadedData[[customTimelines$selectedTrack]]
  dates <- loadedData$dates_first_diagnosis
  all_reactive_inputs <- reactiveValuesToList(input)
  editValues <-  all_reactive_inputs[names(all_reactive_inputs) %in% names(data)]
  editValues$EVENT_TYPE <- toupper(customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack,]$shortName)
  if(editValues["PATIENT_ID"] == ""){
    showNotification("'Patient ID' is requiered.", type="error", duration = NULL)
  } else if(!editValues["PATIENT_ID"] %in% dates$PATIENT_ID){
    showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type="error", duration = NULL)
  } else {
    diagnosisDate <- dates[which(dates$PATIENT_ID == editValues["PATIENT_ID"]),"DATE"]
    if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode =="timeline") stopDate <- as.Date(editValues[["STOP_DATE"]]) else stopDate <- NULL
    failed <- check_input_dates(
      as.Date(diagnosisDate),
      as.Date(editValues[["START_DATE"]]),
      stopDate
    )
    if(failed == 1){
      showNotification("'End date' cannot be earlier as 'start date'.", type="error", duration = NULL)
    } else if(failed == 2){
      showNotification("'Start date' and 'End date' cannot be earlier as diagnosis date.", type="error", duration = NULL)
    } else {
      # edit row to data_timeline_treatment
      start <- as.numeric(as.Date(editValues[["START_DATE"]]) - as.Date(diagnosisDate))
      if(customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$mode =="timeline") end <- as.numeric(as.Date(editValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
      editValues$START_DATE <- start
      editValues$STOP_DATE <- end

      #data()[selected_row(),1] <- editValues[[1]]
      for(i in colnames(data)){
        #print(editValues[[i]])
        loadedData[[customTimelines$selectedTrack]][selected_row,i] <- editValues[i]
      }
      removeModal()
    }
  }
})

# save custom timeline table ---------------------------------------------------------------
observeEvent(input$SaveTimeline_ct, {
  req(customTimelines$selectedTrack)
  timeline_id <- customTimelines$timelines[customTimelines$timelines$name==customTimelines$selectedTrack,]$shortName
  data <- loadedData[[customTimelines$selectedTrack]]
  data_timeline <- data
  data_filename_temp <- paste0("data_timeline_", timeline_id, ".txt.temp")
  meta_filename_temp <- paste0("meta_timeline_", timeline_id, ".txt.temp")
  data_filename <- paste0("data_timeline_", timeline_id, ".txt")
  meta_filename <- paste0("meta_timeline_", timeline_id, ".txt")

  # write data
  write.table(
    data_timeline,
    file.path(study_dir, loadedData$studyID, data_filename_temp),
    append = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # write meta
  meta_timeline <- data.frame(
    V1 = c("cancer_study_identifier", "genetic_alteration_type", "datatype", "data_filename"),
    V2 = c(loadedData$studyID, "CLINICAL", "TIMELINE", data_filename)
  )

  write.table(
    meta_timeline,
    file.path(study_dir, loadedData$studyID, meta_filename_temp),
    append = FALSE,
    sep = ": ",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )

  # rename
  file.rename(
    file.path(study_dir, loadedData$studyID, data_filename_temp),
    file.path(study_dir, loadedData$studyID, data_filename)
  )
  file.rename(
    file.path(study_dir, loadedData$studyID, meta_filename_temp),
    file.path(study_dir, loadedData$studyID, meta_filename)
  )
  # logging
  writeLogfile(
    outdir = study_dir,
    modified_file = file.path(loadedData$studyID, data_filename)
  )
  writeLogfile(
    outdir = study_dir,
    modified_file = file.path(loadedData$studyID, meta_filename)
  )
  showNotification("Data saved successfully!", type="message", duration = 10)
})

