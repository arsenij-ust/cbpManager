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
    row.names = F,
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

# min_timeline_df <- data.frame(
#   PATIENT_ID = character(),
#   START_DATE = numeric(),
#   STOP_DATE = numeric(),
#   EVENT_TYPE = character()
# )
# tracks_rc <- reactiveValues(tables = list())
#
# observeEvent(input$addTrack,{
#   if(input$customTrackID == ""){
#     showNotification("Provide track name first.", type="error", duration = NULL)
#   } else {
#     tracks_rc$tables[[input$customTrackID]] <- min_timeline_df
#     # lapply(
#     #   tracks_rc$tables, function(table){
#     #     createCustomTable(table = table, ID = names(table))
#     #   }
#     # )
#   }
# })
#
# observeEvent(input$addTrack,{
#   if(length(tracks_rc$tables) != 0){
#     purrr::iwalk(tracks_rc$tables, ~{
#       ID <- paste0(.y, "_table")
#       print(ID)
#       print(.x)
#       output[[ID]] <- renderDT({
#         hidenColsSurgery <-  which(colnames(.x) %in% c("EVENT_TYPE"))-1
#         datatable(.x,
#                   selection = "single",
#                   rownames = F,
#                   options = list(pageLength = 25, columnDefs = list(list(visible=FALSE, targets=c(hidenColsSurgery)))))
#       })
#     })
#     output$customTracksUI <- renderUI({
#       purrr::iwalk(tracks_rc$tables, ~{
#         ID <- paste0(.y, "_table")
#         print(ID)
#         DT::dataTableOutput(ID)
#       })
#     })
#   }
# })


#if(length(tracks_rc$tables) != 0){

# createCustomTable <- function(table, ID){
#   output[[paste0(ID, "_table")]] <- renderDT({
#     hidenColsSurgery <-  which(colnames(loadedData$data_timeline_status) %in% c("EVENT_TYPE"))-1
#     datatable(table,
#               selection = "single",
#               rownames = F,
#               options = list(pageLength = 25, columnDefs = list(list(visible=FALSE, targets=c(hidenColsSurgery)))))
#   })
# }

# output$customTracksUI <- renderUI({
#   req(input$addTrack)
#   purrr::iwalk(tracks_rc$tables, ~{
#     DT::dataTableOutput(paste0(names(.x), "_table"))
#   })
# })

min_timeline_df <- data.frame(
  PATIENT_ID = character(),
  START_DATE = numeric(),
  STOP_DATE = numeric(),
  EVENT_TYPE = character()
)

timelineIDs <- reactive({
  directory_files <-
    list.files(file.path(study_dir, input$cancer_study_identifier))
  timeline_files <-
    directory_files[grep("data_timeline_", directory_files)]
  excludeTimelines <-
    c(
      "data_timeline_surgery.txt",
      "data_timeline_status.txt",
      "data_timeline_treatment.txt"
    )
  timeline_files <- timeline_files[-which(excludeTimelines)]

  return(timeline_files)
})

observeEvent(input$addTrack, {
  if (input$customTrackID == "") {
    showNotification("Provide track name first.",
                     type = "error",
                     duration = NULL)
  } else {
    ID <- .create_name(input$customTrackID, toupper = FALSE)
    if (ID %in% timelineIDs)
      loadedData[[paste0("data_timeline_", ID)]] <- min_timeline_df
  }
})

test_vec <- c("data_a.txt", "data_b.txt", "data_c.txt")
lapply(test_vec, function(vec) {
  vec <- gsub("data_", "", vec)
  vec <- gsub(".txt", "", vec)
  vec
})


output$customTracksUI <- renderUI({
  tagList(
    add_rowUI("Status"),
    edit_rowUI("Status"),
    delete_rowUI("Status"),
    add_columnUI("Status"),
    delete_columnUI("Status"),
    save_timelineUI("Status"),
    br(),
    br(),
    DT::DTOutput("statusTable")
  )
})


