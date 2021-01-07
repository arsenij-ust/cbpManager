# add column ---------------------------------------------------------------
addColumn_UI <- function(id, label = "Add column") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddColumn"), label, icon=icon("plus-sign",lib="glyphicon"))

}
addColumn_Server <- function(input, output, session, data){

  observeEvent(input$AddColumn,{
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Add new column",
        #uiOutput(paste0("AddCol",id,"UI")),
        uiOutput(ns("AddCol")),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ModalbuttonAddCol"), "Add column")
        )
      )
    )
  })

  output$AddCol <- renderUI({
    ns <- session$ns
    fluidRow(column(
      width = 8,
      textInput(inputId=ns("colname"),
                label = "Column name:",
                placeholder = "e.g. ATTRIBUTE"
      ))
    )
  })

  params <- reactiveValues(df = NULL)

  observeEvent(input$ModalbuttonAddCol, {
    if(input$colname == ""){
      showNotification("Column name cannot be empty.", type="error", duration = NULL)
    } else if(toupper(input$colname) %in% colnames(data)){
      showNotification("Column already exists.", type="error", duration = NULL)
    } else {
      colname <- .create_name(input$colname)
      data() %>% dplyr::mutate(!!(colname) := "") -> params$df
      removeModal()
    }
  })

  return(reactive({params$df}))

}
# delete column ---------------------------------------------------------------
deleteColumn_UI <- function(id, label = "Delete column(s)") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("DeleteColumn"), label, icon=icon("minus-sign",lib="glyphicon"))

}
deleteColumn_Server <- function(input, output, session, data, exclude){

  observeEvent(input$DeleteColumn, {
    ns <- session$ns
    showModal(modalDialog(
      title = "Delete column(s)",
      fluidRow(column(
        width = 8,
        selectInput(inputId=ns("DelColname"),
                    label = "Select column(s) for deletion:",
                    choices = setdiff(colnames(data()), exclude),
                    multiple = TRUE
        ))
      ),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ModalbuttonDeleteCol"), "Delete column(s)")
      )
    ))
  })
  params <- reactiveValues(df = NULL)
  observeEvent(input$ModalbuttonDeleteCol, {
    params$df <- data()[,!(names(data()) %in% input$DelColname), drop = FALSE]
    #data() <- data[,!(names(data()) %in% input$DelColname), drop = FALSE]
    removeModal()
  })

  return(reactive({params$df}))

}

# delete entry ---------------------------------------------------------------
deleteRow_UI <- function(id, label = "Delete") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("DeleteEntry"), label, icon=icon("remove",lib="glyphicon"))

}
deleteRow_Server <- function(input, output, session, data, selected_row){

  observeEvent(input$DeleteEntry, {
    ns <- session$ns
    if(is.null(selected_row())){
      showNotification("Please select a row", type="warning", duration = NULL)
    } else {
      showModal(modalDialog(
        "Do you want to delete the selected entry?",
        title = "Delete",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ModalbuttonDeleteEntry"), "Delete")
        )
      ))
    }
  })
  params <- reactiveValues(df = NULL)
  observeEvent(input$ModalbuttonDeleteEntry, {
    entry <- selected_row()

    params$df <- data()[-entry,,drop = FALSE]
    removeModal()
  })
  return(reactive({params$df}))
}

# add entry ---------------------------------------------------------------
addRow_UI <- function(id, label = "Add") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddEntry"), label, icon=icon("plus",lib="glyphicon"))

}

addRow_Server <- function(input, output, session, data, patient_ids = NULL, dates_first_diagnosis = NULL, mode = c("timeline", "timepoint")){

  mode <- match.arg(mode)

  observeEvent(input$AddEntry,{
    ns <- session$ns
    showModal(
      modalDialog(
        title = "New entry",
        uiOutput(ns("addUI")),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ModalbuttonAdd"), "Add")
        )
      )
    )
  })

  output$addUI <- renderUI({
    ns <- session$ns
    lapply(colnames(data()),
           function(colname){

             if(colname == "PATIENT_ID"){
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = "Select the Patient ID",
                   choices = unique(patient_ids()[which(!is.na(patient_ids()))]),
                   selected = 1
                 ),
              ))
             } else if (colname == "EVENT_TYPE") {

             } else if (colname == "START_DATE") {
                 fluidRow(column(
                   width = 8,
                   dateInput(
                     inputId=ns(colname),
                     label = colname,
                     format = "dd.mm.yyyy"
                   ),
                 ))
             } else if (colname == "STOP_DATE") {
               if(mode == "timeline"){
                 fluidRow(column(
                   width = 8,
                   dateInput(
                     inputId=ns(colname),
                     label = colname,
                     format = "dd.mm.yyyy"
                   ),
                 ))
               }
             } else if (colname == "TREATMENT_TYPE") {
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = colname,
                   choices = c("Medical Therapy", "Radiation Therapy"),
                   selected = 1),
               ))
             } else if (colname == "SUBTYPE") {
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = colname,
                   choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"),
                   selected = 1),
               ))
             } else {
               fluidRow(column(
                 width = 8,
                 textInput(inputId=ns(colname),
                           label = colname,
                           value = ""),))
             }
           })
  })

  # validate inputs in modalDialog and add new entry to table
  params <- reactiveValues(df = NULL)
  observeEvent(input$ModalbuttonAdd, {
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
  return(reactive({params$df}))
}

# edit entry ---------------------------------------------------------------
editRow_UI <- function(id, label = "Edit") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("EditEntry"), label, icon=icon("pencil",lib="glyphicon"))

}

editRow_Server <- function(input, output, session, data, patient_ids = NULL, dates_first_diagnosis = NULL, selected_row = NULL, mode = c("timeline", "timepoint")){

  mode <- match.arg(mode)

  observeEvent(input$EditEntry,{
    ns <- session$ns
    if(is.null(selected_row())){
      showNotification("Please select a row.", type="warning", duration = NULL)
    } else {
      showModal(
        modalDialog(
          title = "Edit entry",
          uiOutput(ns("editUI")),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("ModalbuttonEdit"), "Edit")
          )
        )
      )
    }
  })

  output$editUI <- renderUI({
    ns <- session$ns
    lapply(colnames(data()),
           function(colname){

             if(colname == "PATIENT_ID"){
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = "Select the Patient ID",
                   choices = unique(patient_ids()[which(!is.na(patient_ids()))]),
                   selected = data()[selected_row(), colname]
                 ),
               ))
             } else if (colname == "EVENT_TYPE") {

             } else if (colname == "START_DATE") {
               fluidRow(column(
                 width = 8,
                 dateInput(
                   inputId=ns(colname),
                   label = colname,
                   format = "dd.mm.yyyy"
                 ),
               ))
             } else if (colname == "STOP_DATE") {
               if(mode == "timeline"){
                 fluidRow(column(
                   width = 8,
                   dateInput(
                     inputId=ns(colname),
                     label = colname,
                     format = "dd.mm.yyyy"
                   ),
                 ))
               }
             } else if (colname == "TREATMENT_TYPE") {
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = colname,
                   choices = c("Medical Therapy", "Radiation Therapy"),
                   selected = data()[selected_row(), colname]),
               ))
             } else if (colname == "SUBTYPE") {
               fluidRow(column(
                 width = 8,
                 selectInput(
                   inputId=ns(colname),
                   label = colname,
                   choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"),
                   selected = data()[selected_row(), colname]),
               ))
             } else {
               fluidRow(column(
                 width = 8,
                 textInput(inputId=ns(colname),
                           label = colname,
                           value = data()[selected_row(), colname]),))
             }
           })
  })

  # validate inputs in modalDialog and edit entry to table
  params <- reactiveValues(df = NULL)
  observeEvent(input$ModalbuttonEdit, {
    all_reactive_inputs <- reactiveValuesToList(input)
    editValues <-  all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
    editValues$EVENT_TYPE <- toupper(gsub("-", "", session$ns("")))
    if(editValues["PATIENT_ID"] == ""){
      showNotification("'Patient ID' is requiered.", type="error", duration = NULL)
    } else if(!editValues["PATIENT_ID"] %in% dates_first_diagnosis()$PATIENT_ID){
      showNotification("Please provide a valid diagnosis date for this 'Patient ID'.", type="error", duration = NULL)
    } else {
      diagnosisDate <- dates_first_diagnosis()[which(dates_first_diagnosis()$PATIENT_ID == editValues["PATIENT_ID"]),"DATE"]
      if(mode=="timeline") stopDate <- as.Date(editValues[["STOP_DATE"]]) else stopDate <- NULL
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
        if(mode=="timeline") end <- as.numeric(as.Date(editValues[["STOP_DATE"]]) - as.Date(diagnosisDate)) else end <- ""
        editValues$START_DATE <- start
        editValues$STOP_DATE <- end

        #data()[selected_row(),1] <- editValues[[1]]
        params$df <- data()
        for(i in colnames(params$df)){
          #print(editValues[[i]])
          params$df[selected_row(),i] <- editValues[i]
        }
        removeModal()
      }
    }
  })
  return(reactive({params$df}))
}

# save timeline table ---------------------------------------------------------------
saveTimeline_UI <- function(id, label = "Save") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("SaveTimeline"), label, class = "btn-success", icon=icon("saved",lib="glyphicon"))

}

saveTimeline_Server <- function(input, output, session, data, study_id){

  observeEvent(input$SaveTimeline, {
    ns <- session$ns
    timeline_id <- tolower(gsub("-", "", session$ns("")))
    data_timeline <- data()
    data_filename_temp <- paste0("data_timeline_", timeline_id, ".txt.temp")
    meta_filename_temp <- paste0("meta_timeline_", timeline_id, ".txt.temp")
    data_filename <- paste0("data_timeline_", timeline_id, ".txt")
    meta_filename <- paste0("meta_timeline_", timeline_id, ".txt")

    # write data
    write.table(
      data_timeline,
      file.path(study_dir, study_id(), data_filename_temp),
        append = FALSE,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

    # write meta
    meta_timeline <- data.frame(
      V1 = c("cancer_study_identifier", "genetic_alteration_type", "datatype", "data_filename"),
      V2 = c(study_id(), "CLINICAL", "TIMELINE", data_filename)
    )

    write.table(
      meta_timeline,
      file.path(study_dir, study_id(), meta_filename_temp),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )

    # rename
    file.rename(
      file.path(study_dir, study_id(), data_filename_temp),
      file.path(study_dir, study_id(), data_filename)
    )
    file.rename(
      file.path(study_dir, study_id(), meta_filename_temp),
      file.path(study_dir, study_id(), meta_filename)
    )
    showNotification("Data saved successfully!", type="message", duration = 10)
  })
}


