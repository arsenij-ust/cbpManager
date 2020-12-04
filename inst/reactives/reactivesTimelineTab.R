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
  module = add_rowServer,
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
  module = edit_rowServer,
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
  module = delete_rowServer,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  selected_row = reactive(input$treatmentTable_rows_selected)
)
observe({
  loadedData$data_timeline_treatment <- treatment_delRow()
})

# delete treatmemt column ---------------------------------------------------------------

treatment_delCol <- callModule(
  module = delete_columnServer,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_treatment <- treatment_delCol()
})

# add treatmemt column ---------------------------------------------------------------

treatment_addCol <- callModule(
  module = add_columnServer,
  id = "Treatment",
  data = reactive(loadedData$data_timeline_treatment)
)
observe({
  loadedData$data_timeline_treatment <- treatment_addCol()
})

# save treatment data ---------------------------------------------------------------
callModule(
  module = save_timelineServer,
  id = "Treatment",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_treatment)
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
  module = add_rowServer,
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
  module = edit_rowServer,
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
  module = delete_rowServer,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  selected_row = reactive(input$surgeryTable_rows_selected)
)
observe({
  loadedData$data_timeline_surgery <- surgery_delRow()
})

# delete surgery column ---------------------------------------------------------------
surgery_delCol <- callModule(
  module = delete_columnServer,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_surgery <- surgery_delCol()
})

# add surgery column ---------------------------------------------------------------
surgery_addCol <- callModule(
  module = add_columnServer,
  id = "Surgery",
  data = reactive(loadedData$data_timeline_surgery)
)
observe({
  loadedData$data_timeline_surgery <- surgery_addCol()
})

# save surgery data ---------------------------------------------------------------
callModule(
  module = save_timelineServer,
  id = "Surgery",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_surgery)
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
  module = add_rowServer,
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
  module = edit_rowServer,
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
  module = delete_rowServer,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  selected_row = reactive(input$statusTable_rows_selected)
)
observe({
  loadedData$data_timeline_status <- status_delRow()
})

# delete status column ---------------------------------------------------------------
status_delCol <- callModule(
  module = delete_columnServer,
  id = "Status",
  data = reactive(loadedData$data_timeline_status),
  exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
)
observe({
  loadedData$data_timeline_status <- status_delCol()
})

# add status column ---------------------------------------------------------------
status_addCol <- callModule(
  module = add_columnServer,
  id = "Status",
  data = reactive(loadedData$data_timeline_status)
)
observe({
  loadedData$data_timeline_status <- status_addCol()
})

# save status data ---------------------------------------------------------------
callModule(
  module = save_timelineServer,
  id = "Status",
  study_id = reactive(loadedData$studyID),
  data = reactive(loadedData$data_timeline_status)
)


# custom tracks ---------------------------------------------------------------
# minimal data.frame of a timeline track
min_timeline_df <- data.frame(
  PATIENT_ID = character(),
  START_DATE = numeric(),
  STOP_DATE = numeric(),
  EVENT_TYPE = character()
)

# get custom timeline names from the reactive study object
# timelineIDs <- reactive({
#   timeline_dfs <- names(loadedData)[grep("data_timeline_", names(loadedData))]
#   excludeTimelines <-
#       c(
#         "data_timeline_surgery",
#         "data_timeline_status",
#         "data_timeline_treatment"
#       )
#   timeline_dfs <- timeline_dfs[-which(timeline_dfs %in% excludeTimelines)]
#   timeline_names <- gsub("data_timeline_", "", timeline_dfs)
#   return(timeline_names)
# })

customTimelines <- reactiveValues()
# get custom timeline names from the reactive study object
observe({
  timeline_dfs <- names(loadedData)[grep("data_timeline_", names(loadedData))]
  excludeTimelines <-
    c(
      "data_timeline_surgery",
      "data_timeline_status",
      "data_timeline_treatment"
    )
  timeline_dfs <- timeline_dfs[-which(timeline_dfs %in% excludeTimelines)]
  timeline_names <- gsub("data_timeline_", "", timeline_dfs)
  customTimelines$timeline_names <- timeline_names
})

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
    if (ID %in% customTimelines$timeline_names){
      showNotification("Name for timeline already exists",
                       type = "error",
                       duration = NULL)
    # add track to the reactive study object
    } else {
      loadedData[[paste0("data_timeline_", ID)]] <- min_timeline_df
    }
  }
})

# UI for timeline drop-down widget
output$selectTrackUI <- renderUI({
  selectInput("selectTrack", width = "400px", label = NULL, choices = customTimelines$timeline_names)
})


observeEvent(input$editTrack, {
  customTimelines$selectedTrack <- input$selectTrack
})

# callModule(
#   module = dynamicTableUI,
#   id = "customTimeline"
#   # data = reactive(loadedData[[paste0("data_timeline_", selectedTrack())]]),
#   # patient_ids = reactive(patient_id_list$ids),
#   # dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
#   # mode = "timepoint"
# )


# UI modules for table modification
output$customTracksUI <- renderUI({
  req(input$editTrack)
  tagList(
    actionButton("AddCTimelineEntry", label = "Add", icon=icon("plus",lib="glyphicon")),
    # add_rowUI("customTimeline"),
    # edit_rowUI("customTimeline"),
    # delete_rowUI("customTimeline"),
    # add_columnUI("customTimeline"),
    # delete_columnUI("customTimeline"),
    # save_timelineUI("customTimeline"),
    br(),
    br(),
    DT::DTOutput("customTimeline")
  )
})

# Data table output
output$customTimeline <- DT::renderDT({
  hidenCols <-
    which(colnames(loadedData[[paste0("data_timeline_", customTimelines$selectedTrack)]]) %in% c("EVENT_TYPE")) - 1

  DT::datatable(
    loadedData[[paste0("data_timeline_", customTimelines$selectedTrack)]],
    selection = "single",
    rownames = F,
    options = list(pageLength = 25, columnDefs = list(list(
      visible = FALSE, targets = c(hidenCols)
    )))
  )
})

# add custom entry ---------------------------------------------------------------

observeEvent(input$AddCTimelineEntry,{
  showModal(
    modalDialog(
      title = "New entry",
      uiOutput("addCTimelineUI"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonAddCT", "Add")
      )
    )
  )
})

output$addCTimelineUI <- renderUI({
  data <- loadedData[[paste0("data_timeline_", customTimelines$selectedTrack)]]
  patient_ids <- patient_id_list$ids
  lapply(colnames(data),
         function(colname){

           # if(colname == "PATIENT_ID"){
           #   fluidRow(column(
           #     width = 8,
           #     selectInput(
           #       inputId=colname,
           #       label = "Select the Patient ID",
           #       choices = unique(patient_ids[which(!is.na(patient_ids))]),
           #       selected = 1
           #     ),
           #   ))
           # } else if (colname == "EVENT_TYPE") {
           #
           # } else if (colname == "START_DATE") {
           #   fluidRow(column(
           #     width = 8,
           #     dateInput(
           #       inputId=colname,
           #       label = colname,
           #       format = "dd.mm.yyyy"
           #     ),
           #   ))
           # } else if (colname == "STOP_DATE") {
           #   if(mode == "timeline"){
           #     fluidRow(column(
           #       width = 8,
           #       dateInput(
           #         inputId=colname,
           #         label = colname,
           #         format = "dd.mm.yyyy"
           #       ),
           #     ))
           #   }
           # # } else if (colname == "TREATMENT_TYPE") {
           # #   fluidRow(column(
           # #     width = 8,
           # #     selectInput(
           # #       inputId=colname,
           # #       label = colname,
           # #       choices = c("Medical Therapy", "Radiation Therapy"),
           # #       selected = 1),
           # #   ))
           # # }
           # else if (colname == "SUBTYPE") {
           #   fluidRow(column(
           #     width = 8,
           #     selectInput(
           #       inputId=ns(colname),
           #       label = colname,
           #       choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"),
           #       selected = 1),
           #   ))
           # } else {
           #   fluidRow(column(
           #     width = 8,
           #     textInput(inputId=ns(colname),
           #               label = colname,
           #               value = ""),))
           # }
           fluidRow(column(
                 width = 8,
                 textInput(inputId=colname,
                           label = colname,
                           value = ""),))
         })
})

# validate inputs in modalDialog and add new entry to table
observeEvent(input$ModalbuttonAddCT, {
  all_reactive_inputs <- reactiveValuesToList(input)
  addPatientValues <-  all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
  addPatientValues$EVENT_TYPE <- toupper(gsub("-", "", session$ns("")))
  if(addPatientValues["PATIENT_ID"] == ""){
    showNotification("'Patient ID' is requiered.", type="error", duration = NULL)
  } else if(!addPatientValues["PATIENT_ID"] %in% dates_first_diagnosis()$PATIENT_ID){
    showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type="error", duration = NULL)
  } else {
    diagnosisDate <- dates_first_diagnosis()[which(dates_first_diagnosis()$PATIENT_ID == addPatientValues["PATIENT_ID"]),"DATE"]
    if(mode=="timeline") stopDate <- as.Date(addPatientValues[["STOP_DATE"]]) else stopDate <- NULL
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
      if(mode=="timeline") end <- as.numeric(as.Date(addPatientValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
      addPatientValues$START_DATE <- start
      addPatientValues$STOP_DATE <- end
      params$df <- plyr::rbind.fill(data(), as.data.frame(addPatientValues))
      removeModal()
    }
  }
})



#
# custom_addRow <- callModule(
#   module = add_rowServer,
#   id = "customTimeline",
#   data = reactive(loadedData[[paste0("data_timeline_", selectedTrack())]]),
#   patient_ids = reactive(patient_id_list$ids),
#   dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
#   mode = "timepoint"
# )
# observe({
#   if(!is.null(custom_addRow())){
#     loadedData[[paste0("data_timeline_", selectedTrack())]] <- custom_addRow()
#   }
# })

# # edit custom entry ---------------------------------------------------------------
# status_editRow <- callModule(
#   module = edit_rowServer,
#   id = "Status",
#   data = reactive(loadedData$data_timeline_status),
#   patient_ids = reactive(patient_id_list$ids),
#   dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
#   selected_row = reactive(input$statusTable_rows_selected),
#   mode = "timepoint"
# )
# observe({
#   loadedData$data_timeline_status <- status_editRow()
# })
#
# # delete custom entry ---------------------------------------------------------------
# status_delRow <- callModule(
#   module = delete_rowServer,
#   id = "Status",
#   data = reactive(loadedData$data_timeline_status),
#   selected_row = reactive(input$statusTable_rows_selected)
# )
# observe({
#   loadedData$data_timeline_status <- status_delRow()
# })
#
# # delete custom column ---------------------------------------------------------------
# status_delCol <- callModule(
#   module = delete_columnServer,
#   id = "Status",
#   data = reactive(loadedData$data_timeline_status),
#   exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
# )
# observe({
#   loadedData$data_timeline_status <- status_delCol()
# })
#
# # add custom column ---------------------------------------------------------------
# status_addCol <- callModule(
#   module = add_columnServer,
#   id = "Status",
#   data = reactive(loadedData$data_timeline_status)
# )
# observe({
#   loadedData$data_timeline_status <- status_addCol()
# })
#
# # save custom data ---------------------------------------------------------------
# callModule(
#   module = save_timelineServer,
#   id = "Status",
#   study_id = reactive(loadedData$studyID),
#   data = reactive(loadedData$data_timeline_status)
# )
