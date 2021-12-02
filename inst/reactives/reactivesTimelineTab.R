# image
output$timelineDataImg <- renderImage(
  {
    return(
      list(
        src = system.file("www", "timeline.png", package = "cbpManager"),
        contentType = "image/png",
        alt = "timeline-example",
        width = "auto"
      )
    )
  },
  deleteFile = FALSE
)

# tour  ---------------------------------------------------------------
observeEvent(input$tour_timelines, {
  tour <- read.delim(system.file("apphelp", "tour_timelines.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# dates of first diagnosis ---------------------------------------------------------------
# Data table output
output$dateTable <- DT::renderDT({
  if (!is.null(loadedData$dates_first_diagnosis)) {
    DT::datatable(
      loadedData$dates_first_diagnosis,
      escape = FALSE,
      selection = "single",
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
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
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
  req(input$AddPatientIDDate, input$AddPatientDate)
  if (input$AddPatientIDDate == "") {
    showNotification("PATIENT_ID cannot be empty.",
      type = "error",
      duration = NULL
    )
  } else if (is.null(input$AddPatientDate)) {
    showNotification("Date cannot be empty.",
      type = "error",
      duration = NULL
    )
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
    
    # change tracker
    study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
    
    removeModal()
  }
})

# edit date
observeEvent(input$datesEdit, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
  if (is.null(input$dateTable_rows_selected)) {
    showNotification("Please select a row.",
      type = "warning",
      duration = NULL
    )
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

# Validate edited values and add them to data.frame
observeEvent(input$ModalbuttonEditDate, {
  req(input$EditPatientIDDate, input$EditPatientDate)
  if (input$EditPatientIDDate == "") {
    showNotification("PATIENT_ID cannot be empty.",
      type = "error",
      duration = NULL
    )
  } else if (is.null(input$EditPatientDate)) {
    showNotification("Date cannot be empty.",
      type = "error",
      duration = NULL
    )
  } else {
    loadedData$dates_first_diagnosis[which(loadedData$dates_first_diagnosis$PATIENT_ID ==
      input$EditPatientIDDate), "DATE"] <-
      as.character(input$EditPatientDate)
    
    # change tracker
    study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
    
    removeModal()
  }
})

# delete date entry
observeEvent(input$datesDelete, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
  if (is.null(input$dateTable_rows_selected)) {
    showNotification("Please select a row.",
      type = "warning",
      duration = NULL
    )
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
  
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
  
  removeModal()
})

# safe dates of first diagnosis
observeEvent(input$datesSave, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
  }
  req(loadedData$studyID, loadedData$dates_first_diagnosis)
  write.table(
    loadedData$dates_first_diagnosis,
    file.path(
      study_dir,
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
  showNotification("Diagnosis dates saved successfully!",
    type = "message",
    duration = 10
  )
  
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("check-circle"))
  
})

# treatment timeline ---------------------------------------------------------------
# Data table output
output$treatmentTable <- DT::renderDT({
  hidenColsTreatment <-
    which(colnames(loadedData$data_timeline_treatment) %in% c("EVENT_TYPE")) - 1
  DT::datatable(
    loadedData$data_timeline_treatment,
    selection = "single",
    rownames = FALSE,
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
observeEvent(treatment_addRow(),{
  loadedData$data_timeline_treatment <- treatment_addRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(treatment_editRow(),{
  loadedData$data_timeline_treatment <- treatment_editRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete treatmemt entry ---------------------------------------------------------------
treatment_delRow <- callModule(
  module = deleteRow_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  selected_row = reactive(input$treatmentTable_rows_selected)
)
observeEvent(treatment_delRow(),{
  loadedData$data_timeline_treatment <- treatment_delRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete treatmemt column ---------------------------------------------------------------

treatment_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observeEvent(treatment_delCol(),{
  loadedData$data_timeline_treatment <- treatment_delCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# add treatmemt column ---------------------------------------------------------------

treatment_addCol <- callModule(
  module = addColumn_Server,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment)
)
observeEvent(treatment_addCol(),{
  loadedData$data_timeline_treatment <- treatment_addCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save treatment data ---------------------------------------------------------------

treatment_save <- callModule(
  module = saveTimeline_Server,
  id = "Treatment",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_treatment)
)

observeEvent(treatment_save(),{
  # change tracker
  check <- treatment_save()
  study_tracker$df[4, "Saved"] <- as.character(icon("check-circle"))
})

# surgery timeline ---------------------------------------------------------------
# Data table output
output$surgeryTable <- DT::renderDT({
  hidenColsSurgery <-
    which(colnames(loadedData$data_timeline_surgery) %in% c("EVENT_TYPE", "STOP_DATE")) - 1
  DT::datatable(
    loadedData$data_timeline_surgery,
    selection = "single",
    rownames = FALSE,
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
observeEvent(surgery_addRow(),{
  loadedData$data_timeline_surgery <- surgery_addRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(surgery_editRow(),{
  loadedData$data_timeline_surgery <- surgery_editRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete surgery entry ---------------------------------------------------------------
surgery_delRow <- callModule(
  module = deleteRow_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  selected_row = reactive(input$surgeryTable_rows_selected)
)
observeEvent(surgery_delRow(),{
  loadedData$data_timeline_surgery <- surgery_delRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete surgery column ---------------------------------------------------------------
surgery_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observeEvent(surgery_delCol(),{
  loadedData$data_timeline_surgery <- surgery_delCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# add surgery column ---------------------------------------------------------------
surgery_addCol <- callModule(
  module = addColumn_Server,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery)
)
observeEvent(surgery_addCol(),{
  loadedData$data_timeline_surgery <- surgery_addCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save surgery data ---------------------------------------------------------------
surgery_save <- callModule(
  module = saveTimeline_Server,
  id = "Surgery",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_surgery)
)

observeEvent(surgery_save(),{
  # change tracker
  check <- surgery_save()
  study_tracker$df[4, "Saved"] <- as.character(icon("check-circle"))
})

# status timeline ---------------------------------------------------------------
# Data table output
output$statusTable <- DT::renderDT({
  hidenColsSurgery <-
    which(colnames(loadedData$data_timeline_status) %in% c("EVENT_TYPE", "STOP_DATE")) - 1
  DT::datatable(
    loadedData$data_timeline_status,
    selection = "single",
    rownames = FALSE,
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
observeEvent(status_addRow(),{
  loadedData$data_timeline_status <- status_addRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
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
observeEvent(status_editRow(),{
  loadedData$data_timeline_status <- status_editRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete status entry ---------------------------------------------------------------
status_delRow <- callModule(
  module = deleteRow_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  selected_row = reactive(input$statusTable_rows_selected)
)
observeEvent(status_delRow(),{
  loadedData$data_timeline_status <- status_delRow()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# delete status column ---------------------------------------------------------------
status_delCol <- callModule(
  module = deleteColumn_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observeEvent(status_delCol(),{
  loadedData$data_timeline_status <- status_delCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# add status column ---------------------------------------------------------------
status_addCol <- callModule(
  module = addColumn_Server,
  id = "Status",
  data = reactive(loadedData$data_timeline_status)
)
observeEvent(status_addCol(),{
  loadedData$data_timeline_status <- status_addCol()
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
})

# save status data ---------------------------------------------------------------
status_save <- callModule(
  module = saveTimeline_Server,
  id = "Status",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_status)
)

observeEvent(status_save(),{
  # change tracker
  check <- status_save()
  study_tracker$df[4, "Saved"] <- as.character(icon("check-circle"))
})

# custom tracks ---------------------------------------------------------------
# minimal data.frame of a timeline track
min_timeline_df <- data.frame(
  PATIENT_ID = character(),
  START_DATE = numeric(),
  STOP_DATE = numeric(),
  EVENT_TYPE = character()
)


# add new custom track
observeEvent(input$addTrack, {
  # check if name for new track exists
  if (input$customTrackID == "") {
    showNotification("Provide track name first.",
      type = "error",
      duration = NULL
    )
  } else {
    # sanitize track name
    ID <- create_name(input$customTrackID, toupper = FALSE)

    # check if track name already exists
    if (ID %in% customTimelines$timelines$shortName) {
      showNotification("Name for timeline already exists",
        type = "error",
        duration = NULL
      )
      # add track to the reactive study object
    } else {
      loadedData[[paste0("data_timeline_", ID)]] <- min_timeline_df
      mode <- input$timelineMode
      customTimelines$timelines[paste0("data_timeline_", ID), ] <- list(name = paste0("data_timeline_", ID), shortName = ID, mode = mode, trigger = 0)
    }
  }
})

# UI for timeline drop-down widget
output$selectTrackUI <- renderUI({
  selectInput("selectTrack", width = "400px", label = NULL, choices = customTimelines$timelines$shortName)
})


observeEvent(input$editTrack, {
  if(is.null(input$selectTrack)){
    showNotification(
      "Please select a timeline track first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
  customTimelines$selectedTrack <- customTimelines$timelines[customTimelines$timelines$shortName == input$selectTrack, ]$name
})

# Data table output
output$Table_ct <- DT::renderDT({
  req(customTimelines$selectedTrack)

  if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") {
    colsToHide <- c("EVENT_TYPE")
  } else {
    colsToHide <- c("STOP_DATE", "EVENT_TYPE")
  }
  # find the index of the columns to hide
  hiddenCols <-
    which(colnames(loadedData[[customTimelines$selectedTrack]]) %in% colsToHide) - 1

  DT::datatable(
    loadedData[[customTimelines$selectedTrack]],
    selection = "single",
    rownames = FALSE,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE, targets = c(hiddenCols)
    )))
  )
})

# add custom column ---------------------------------------------------------------
observeEvent(input$AddColumn_ct, {
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
    textInput(
      inputId = "ct_colname",
      label = "Column name:",
      placeholder = "e.g. ATTRIBUTE"
    )
  ))
})

observeEvent(input$ct_ModalbuttonAddCol, {
  data <- loadedData[[customTimelines$selectedTrack]]
  if (input$ct_colname == "") {
    showNotification("Column name cannot be empty.", type = "error", duration = NULL)
  } else if (toupper(input$ct_colname) %in% colnames(data)) {
    showNotification("Column already exists.", type = "error", duration = NULL)
  } else {
    ct_colname <- create_name(input$ct_colname)
    loadedData[[customTimelines$selectedTrack]] <- data %>% dplyr::mutate(!!(ct_colname) := "")
    # change tracker
    study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
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
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
  removeModal()
})

# delete custom entry ---------------------------------------------------------------

observeEvent(input$DeleteEntry_ct, {
  req(customTimelines$selectedTrack)
  if (is.null(input$Table_ct_rows_selected)) {
    showNotification("Please select a row", type = "warning", duration = NULL)
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

  loadedData[[customTimelines$selectedTrack]] <- loadedData[[customTimelines$selectedTrack]][-entry, , drop = FALSE]
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
  removeModal()
})

# add custom entry ---------------------------------------------------------------

observeEvent(input$AddEntry_ct, {
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
  lapply(
    colnames(loadedData[[customTimelines$selectedTrack]]),
    function(colname) {
      if (colname == "PATIENT_ID") {
        fluidRow(column(
          width = 8,
          selectInput(
            inputId = colname,
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
            inputId = colname,
            label = colname,
            format = "dd.mm.yyyy"
          ),
        ))
      } else if (colname == "STOP_DATE") {
        if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") {
          fluidRow(column(
            width = 8,
            dateInput(
              inputId = colname,
              label = colname,
              format = "dd.mm.yyyy"
            ),
          ))
        }
      } else {
        fluidRow(column(
          width = 8,
          textInput(
            inputId = colname,
            label = colname,
            value = ""
          ),
        ))
      }
    }
  )
})

# validate inputs in modalDialog and add new entry to table
observeEvent(input$ct_ModalbuttonAdd, {
  data <- loadedData[[customTimelines$selectedTrack]]
  dates <- loadedData$dates_first_diagnosis
  all_reactive_inputs <- reactiveValuesToList(input)
  addPatientValues <- all_reactive_inputs[names(all_reactive_inputs) %in% names(data)]
  addPatientValues$EVENT_TYPE <- toupper(customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$shortName)
  if (addPatientValues["PATIENT_ID"] == "") {
    showNotification("'Patient ID' is requiered.", type = "error", duration = NULL)
  } else if (!addPatientValues["PATIENT_ID"] %in% dates$PATIENT_ID) {
    showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type = "error", duration = NULL)
  } else {
    diagnosisDate <- dates[which(dates$PATIENT_ID == addPatientValues["PATIENT_ID"]), "DATE"]
    if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") stopDate <- as.Date(addPatientValues[["STOP_DATE"]]) else stopDate <- NULL
    failed <- check_input_dates(
      as.Date(diagnosisDate),
      as.Date(addPatientValues[["START_DATE"]]),
      stopDate
    )
    if (failed == 1) {
      showNotification("'End date' cannot be earlier as 'start date'.", type = "error", duration = NULL)
    } else if (failed == 2) {
      showNotification("'Start date' and 'End date' cannot be earlier as diagnosis date.", type = "error", duration = NULL)
    } else {
      # add row to data_timeline_treatment
      start <- as.numeric(as.Date(addPatientValues[["START_DATE"]]) - as.Date(diagnosisDate))
      if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") end <- as.numeric(as.Date(addPatientValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
      addPatientValues$START_DATE <- start
      addPatientValues$STOP_DATE <- end
      loadedData[[customTimelines$selectedTrack]] <- plyr::rbind.fill(data, as.data.frame(addPatientValues))
      # change tracker
      study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
      removeModal()
    }
  }
})

# edit custom entry ---------------------------------------------------------------
observeEvent(input$EditEntry_ct, {
  req(customTimelines$selectedTrack)
  if (is.null(input$Table_ct_rows_selected)) {
    showNotification("Please select a row.", type = "warning", duration = NULL)
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
  lapply(
    colnames(data),
    function(colname) {
      if (colname == "PATIENT_ID") {
        fluidRow(column(
          width = 8,
          selectInput(
            inputId = colname,
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
        if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") {
          fluidRow(column(
            width = 8,
            dateInput(
              inputId = colname,
              label = colname,
              format = "dd.mm.yyyy"
            ),
          ))
        }
      } else {
        fluidRow(column(
          width = 8,
          textInput(
            inputId = colname,
            label = colname,
            value = data[selected_row, colname]
          ),
        ))
      }
    }
  )
})

# validate inputs in modalDialog and edit entry to table
observeEvent(input$ct_ModalbuttonEdit, {
  selected_row <- input$Table_ct_rows_selected
  data <- loadedData[[customTimelines$selectedTrack]]
  dates <- loadedData$dates_first_diagnosis
  all_reactive_inputs <- reactiveValuesToList(input)
  editValues <- all_reactive_inputs[names(all_reactive_inputs) %in% names(data)]
  editValues$EVENT_TYPE <- toupper(customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$shortName)
  if (editValues["PATIENT_ID"] == "") {
    showNotification("'Patient ID' is requiered.", type = "error", duration = NULL)
  } else if (!editValues["PATIENT_ID"] %in% dates$PATIENT_ID) {
    showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type = "error", duration = NULL)
  } else {
    diagnosisDate <- dates[which(dates$PATIENT_ID == editValues["PATIENT_ID"]), "DATE"]
    if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") stopDate <- as.Date(editValues[["STOP_DATE"]]) else stopDate <- NULL
    failed <- check_input_dates(
      as.Date(diagnosisDate),
      as.Date(editValues[["START_DATE"]]),
      stopDate
    )
    if (failed == 1) {
      showNotification("'End date' cannot be earlier as 'start date'.", type = "error", duration = NULL)
    } else if (failed == 2) {
      showNotification("'Start date' and 'End date' cannot be earlier as diagnosis date.", type = "error", duration = NULL)
    } else {
      # edit row to data_timeline_treatment
      start <- as.numeric(as.Date(editValues[["START_DATE"]]) - as.Date(diagnosisDate))
      if (customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$mode == "timeline") end <- as.numeric(as.Date(editValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
      editValues$START_DATE <- start
      editValues$STOP_DATE <- end

      for (i in colnames(data)) {
        loadedData[[customTimelines$selectedTrack]][selected_row, i] <- editValues[i]
      }
      # change tracker
      study_tracker$df[4, "Saved"] <- as.character(icon("exclamation-circle"))
      removeModal()
    }
  }
})

# save custom timeline table ---------------------------------------------------------------
observeEvent(input$SaveTimeline_ct, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please select and load a study in the 'Study' tab.",
      type = "error",
      duration = NULL
    )
  }
  req(customTimelines$selectedTrack, loadedData$studyID)
  timeline_id <- customTimelines$timelines[customTimelines$timelines$name == customTimelines$selectedTrack, ]$shortName
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
  
  # change tracker
  study_tracker$df[4, "Saved"] <- as.character(icon("check-circle"))
  
  showNotification("Data saved successfully!", type = "message", duration = 10)
})
