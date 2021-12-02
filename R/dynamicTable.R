# add column -------------------------------------------------------------------

#' UI elements of module for adding a column
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
addColumn_UI <- function(id, label = "Add column") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddColumn"), label, icon = icon("plus-sign", lib = "glyphicon"))
}

#' Server logic of module for adding a column
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @return reactive data.frame of modified source data
addColumn_Server <- function(input, output, session, data) {
  observeEvent(input$AddColumn, {
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Add new column",
        uiOutput(ns("AddCol")),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("ModalbuttonAddCol"), "Add column"
          )
        )
      )
    )
  })

  output$AddCol <- renderUI({
    ns <- session$ns
    fluidRow(column(
      width = 8,
      textInput(
        inputId = ns("colname"),
        label = "Column name:",
        placeholder = "e.g. ATTRIBUTE"
      )
    ))
  })
  
  params <- reactiveValues(df = NULL)
  
  observeEvent(input$ModalbuttonAddCol, {
    if (input$colname == "") {
      showNotification("Column name cannot be empty.",
                       type = "error",
                       duration = NULL
      )
    } else if (toupper(input$colname) %in% colnames(data)) {
      showNotification("Column already exists.",
                       type = "error",
                       duration = NULL
      )
    } else {
      colname <- create_name(input$colname)
      params$df <- data() %>% dplyr::mutate(!!(colname) := "")
      removeModal()
    }
  })
  
  return(reactive({
    params$df
  }))
}
# delete column ----------------------------------------------------------------

#' UI elements of module for deleting a column
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
deleteColumn_UI <- function(id, label = "Delete column(s)") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("DeleteColumn"),
    label,
    icon = icon("minus-sign", lib = "glyphicon")
  )
}

#' Server logic of module for deleting a column
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @param exclude column names that shoud be excluded from deletion
#' @return reactive data.frame of modified source data
deleteColumn_Server <-
  function(input, output, session, data, exclude) {
    observeEvent(input$DeleteColumn, {
      ns <- session$ns
      showModal(
        modalDialog(
          title = "Delete column(s)",
          fluidRow(column(
            width = 8,
            selectInput(
              inputId = ns("DelColname"),
              label = "Select column(s) for deletion:",
              choices = setdiff(colnames(data()), exclude),
              multiple = TRUE
            )
          )),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("ModalbuttonDeleteCol"), "Delete column(s)")
          )
        )
      )
    })
    params <- reactiveValues(df = NULL)
    observeEvent(input$ModalbuttonDeleteCol, {
      params$df <-
        data()[, !(names(data()) %in% input$DelColname), drop = FALSE]
      removeModal()
    })

    return(reactive({
      params$df
    }))
  }

# delete entry -----------------------------------------------------------------

#' UI elements of module for removing a row
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
deleteRow_UI <- function(id, label = "Delete") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("DeleteEntry"), label, icon = icon("remove", lib = "glyphicon"))
}

#' Server logic of module for removing a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @param selected_row Index of the selected row from the table
#' @return reactive data.frame of modified source data
deleteRow_Server <-
  function(input, output, session, data, selected_row) {
    observeEvent(input$DeleteEntry, {
      ns <- session$ns
      if (is.null(selected_row())) {
        showNotification("Please select a row",
          type = "warning",
          duration = NULL
        )
      } else {
        showModal(
          modalDialog(
            "Do you want to delete the selected entry?",
            title = "Delete",
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("ModalbuttonDeleteEntry"), "Delete"
              )
            )
          )
        )
      }
    })
    params <- reactiveValues(df = NULL)
    observeEvent(input$ModalbuttonDeleteEntry, {
      entry <- selected_row()

      params$df <- data()[-entry, , drop = FALSE]
      removeModal()
    })
    return(reactive({
      params$df
    }))
  }

# add entry --------------------------------------------------------------------

#' UI elements of module for adding a row
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
addRow_UI <- function(id, label = "Add") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddEntry"), label, icon = icon("plus", lib = "glyphicon"))
}

#' Server logic of module for adding a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @param patient_ids reactive vector of existing patient IDs
#' @param dates_first_diagnosis data.frame with dates of the first diagnosis and
#' patient IDs
#' @param mode Mode of the timeline data. Controls which columns are displayed.
#' @return reactive data.frame of modified source data
addRow_Server <-
  function(input,
           output,
           session,
           data,
           patient_ids = NULL,
           dates_first_diagnosis = NULL,
           mode = c("timeline", "timepoint")) {
    mode <- match.arg(mode)

    observeEvent(input$AddEntry, {
      ns <- session$ns
      showModal(modalDialog(
        title = "New entry",
        uiOutput(ns("addUI")),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(
            "ModalbuttonAdd"
          ), "Add")
        )
      ))
    })

    output$addUI <- renderUI({
      ns <- session$ns
      lapply(
        colnames(data()),
        function(colname) {
          if (colname == "PATIENT_ID") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
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
                inputId = ns(colname),
                label = colname,
                format = "dd.mm.yyyy"
              ),
            ))
          } else if (colname == "STOP_DATE") {
            if (mode == "timeline") {
              fluidRow(column(
                width = 8,
                dateInput(
                  inputId = ns(colname),
                  label = colname,
                  format = "dd.mm.yyyy"
                ),
              ))
            }
          } else if (colname == "TREATMENT_TYPE") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = paste(colname, "*Select multiple items for combination therapy"),
                multiple = TRUE,
                choices = c("Medical Therapy", "Radiation Therapy", "Non-surgical Local Treamtent"),
                selected = 1
              ),
            ))
          } else if (colname == "SUBTYPE") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = paste(colname, "*Select multiple items for combination therapy"),
                multiple = TRUE,
                choices = c(
                  "",
                  "Monoclonal Antibody",
                  "Antibody drug conjugate",
                  "Immunotherapy",
                  "Chemotherapy",
                  "Hormone Therapy",
                  "Targeted Therapy",
                  "WPRT",
                  "IVRT"
                ),
                selected = 1
              ),
            ))
          } else {
            fluidRow(column(
              width = 8,
              textInput(
                inputId = ns(colname),
                label = colname,
                value = ""
              ),
            ))
          }
        }
      )
    })

    # validate inputs in modalDialog and add new entry to table
    params <- reactiveValues(df = NULL)
    observeEvent(input$ModalbuttonAdd, {
      all_reactive_inputs <- reactiveValuesToList(input)
      addPatientValues <-
        all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
      addPatientValues$EVENT_TYPE <-
        toupper(gsub("-", "", session$ns("")))
      if (addPatientValues["PATIENT_ID"] == "") {
        showNotification("'Patient ID' is requiered.",
          type = "error",
          duration = NULL
        )
      } else if (!addPatientValues["PATIENT_ID"] %in% dates_first_diagnosis()$PATIENT_ID) {
        showNotification(
          "Please provide a valid diagnosis date for this 'Patient ID'.",
          type = "error",
          duration = NULL
        )
      } else {
        if ("SUBTYPE" %in% names(addPatientValues)) {
          if(is.null(addPatientValues[["SUBTYPE"]])) addPatientValues[["SUBTYPE"]] <- ""
          if(length(addPatientValues[["SUBTYPE"]])>1){
            addPatientValues[["SUBTYPE"]] <- paste(addPatientValues[["SUBTYPE"]], collapse = " + ")
          }
        }
        if ("TREATMENT_TYPE" %in% names(addPatientValues)) {
          if(is.null(addPatientValues[["TREATMENT_TYPE"]])) addPatientValues[["TREATMENT_TYPE"]] <- ""
          if(length(addPatientValues[["TREATMENT_TYPE"]])>1){
            addPatientValues[["TREATMENT_TYPE"]] <- paste(addPatientValues[["TREATMENT_TYPE"]], collapse = " + ")
          }
        }
        
        diagnosisDate <-
          dates_first_diagnosis()[which(dates_first_diagnosis()$PATIENT_ID == addPatientValues["PATIENT_ID"]), "DATE"]
        if (mode == "timeline") {
          stopDate <-
            as.Date(addPatientValues[["STOP_DATE"]])
        } else {
          stopDate <- NULL
        }
        failed <- check_input_dates(
          as.Date(diagnosisDate),
          as.Date(addPatientValues[["START_DATE"]]),
          stopDate
        )
        if (failed == 1) {
          showNotification(
            "'Stop date' cannot be earlier as 'start date'.",
            type = "error",
            duration = NULL
          )
        } else if (failed == 2) {
          showNotification(
            "'Start date' and 'Stop date' cannot be earlier as diagnosis date.",
            type = "error",
            duration = NULL
          )
        } else {
          # add row to data_timeline_treatment
          start <-
            as.numeric(
              as.Date(
                addPatientValues[["START_DATE"]]
              ) - as.Date(diagnosisDate)
            )
          if (mode == "timeline") {
            end <-
              as.numeric(
                as.Date(addPatientValues[["STOP_DATE"]]) - as.Date(diagnosisDate)
              )
          } else {
            end <- ""
          }
          addPatientValues$START_DATE <- start
          addPatientValues$STOP_DATE <- end
          params$df <-
           plyr::rbind.fill(data(), as.data.frame(addPatientValues))

          removeModal()
        }
      }
    })
    return(reactive({
      params$df
    }))
  }

# edit entry -------------------------------------------------------------------

#' UI elements of module for editing a row
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
editRow_UI <- function(id, label = "Edit") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("EditEntry"), label, icon = icon("pencil", lib = "glyphicon"))
}

#' Server logic of module for editing a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @param patient_ids reactive vector of existing patient IDs
#' @param dates_first_diagnosis data.frame with dates of the first diagnosis and patient IDs
#' @param selected_row the index of the selected row
#' @param mode Mode of the timeline data. Controls which columns are displayed.
#' @return reactive data.frame of modified source data
editRow_Server <-
  function(input,
           output,
           session,
           data,
           patient_ids = NULL,
           dates_first_diagnosis = NULL,
           selected_row = NULL,
           mode = c("timeline", "timepoint")) {
    mode <- match.arg(mode)

    observeEvent(input$EditEntry, {
      ns <- session$ns
      if (is.null(selected_row())) {
        showNotification("Please select a row.",
          type = "warning",
          duration = NULL
        )
      } else {
        showModal(modalDialog(
          title = "Edit entry",
          uiOutput(ns("editUI")),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns(
              "ModalbuttonEdit"
            ), "Edit")
          )
        ))
      }
    })

    output$editUI <- renderUI({
      ns <- session$ns
      lapply(
        colnames(data()),
        function(colname) {
          if (colname == "PATIENT_ID") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
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
                inputId = ns(colname),
                label = colname,
                format = "dd.mm.yyyy"
              ),
            ))
          } else if (colname == "STOP_DATE") {
            if (mode == "timeline") {
              fluidRow(column(
                width = 8,
                dateInput(
                  inputId = ns(colname),
                  label = colname,
                  format = "dd.mm.yyyy"
                ),
              ))
            }
          } else if (colname == "TREATMENT_TYPE") {
            treatment_value <- data()[selected_row(), colname]
            if(rlang::is_empty(treatment_value)) treatment_value <- ""
            if(grepl(" \\+ ", treatment_value)){
              treatment_value <- unlist(strsplit(treatment_value, " \\+ "))
            }
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                multiple = TRUE,
                label = paste(colname, "*Select multiple items for combination therapy"),
                choices = c("Medical Therapy", "Radiation Therapy", "Non-surgical Local Treamtent"),
                selected = treatment_value
              ),
            ))
          } else if (colname == "SUBTYPE") {
            subtype_value <- data()[selected_row(), colname]
            if(rlang::is_empty(subtype_value)) subtype_value <- ""
            if(grepl(" \\+ ", subtype_value)){
              subtype_value <- unlist(strsplit(subtype_value, " \\+ "))
            } 
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = paste(colname, "*Select multiple items for combination therapy"),
                multiple = TRUE,
                choices = c(
                  "",
                  "Monoclonal Antibody",
                  "Antibody drug conjugate",
                  "Immunotherapy",
                  "Chemotherapy",
                  "Hormone Therapy",
                  "Targeted Therapy",
                  "WPRT",
                  "IVRT"
                ),
                selected = subtype_value
              ),
            ))
          }else {
            fluidRow(column(
                  width = 8,
                  textInput(
                    inputId = ns(colname),
                    label = colname,
                    value = data()[selected_row(), colname]
                  ),
                ))
          }
        }
      )
    })

    # validate inputs in modalDialog and edit entry to table
    params <- reactiveValues(df = NULL)
    observeEvent(input$ModalbuttonEdit, {
      all_reactive_inputs <- reactiveValuesToList(input)
      editValues <-
        all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
      editValues$EVENT_TYPE <- toupper(gsub("-", "", session$ns("")))
      if ("SUBTYPE" %in% names(editValues)) {
        if(is.null(editValues[["SUBTYPE"]])) editValues[["SUBTYPE"]] <- ""
        if(length(editValues[["SUBTYPE"]])>1){
          editValues[["SUBTYPE"]] <- paste(editValues[["SUBTYPE"]], collapse = " + ")
        }
      }
      if ("TREATMENT_TYPE" %in% names(editValues)) {
        if(is.null(editValues[["TREATMENT_TYPE"]])) editValues[["TREATMENT_TYPE"]] <- ""
        if(length(editValues[["TREATMENT_TYPE"]])>1){
          editValues[["TREATMENT_TYPE"]] <- paste(editValues[["TREATMENT_TYPE"]], collapse = " + ")
        }
      }
      if (editValues["PATIENT_ID"] == "") {
        showNotification("'Patient ID' is requiered.",
          type = "error",
          duration = NULL
        )
      } else if (!editValues["PATIENT_ID"] %in% dates_first_diagnosis()$PATIENT_ID) {
        showNotification(
          "Please provide a valid diagnosis date for this 'Patient ID'.",
          type = "error",
          duration = NULL
        )
      } else {
        diagnosisDate <-
          dates_first_diagnosis()[which(dates_first_diagnosis()$PATIENT_ID == editValues["PATIENT_ID"]), "DATE"]
        if (mode == "timeline") {
          stopDate <-
            as.Date(editValues[["STOP_DATE"]])
        } else {
          stopDate <- NULL
        }
        failed <- check_input_dates(
          as.Date(diagnosisDate),
          as.Date(editValues[["START_DATE"]]),
          stopDate
        )
        if (failed == 1) {
          showNotification(
            "'End date' cannot be earlier as 'start date'.",
            type = "error",
            duration = NULL
          )
        } else if (failed == 2) {
          showNotification(
            "'Start date' and 'End date' cannot be earlier as diagnosis date.",
            type = "error",
            duration = NULL
          )
        } else {
          # edit row to data_timeline_treatment
          start <-
            as.numeric(as.Date(editValues[["START_DATE"]]) - as.Date(diagnosisDate))
          if (mode == "timeline") {
            end <-
              as.numeric(as.Date(editValues[["STOP_DATE"]]) - as.Date(diagnosisDate))
          } else {
            end <- ""
          }
          editValues$START_DATE <- start
          editValues$STOP_DATE <- end

          params$df <- data()
          for (i in colnames(params$df)) {
            params$df[selected_row(), i] <- editValues[i]
          }
          removeModal()
        }
      }
    })
    return(reactive({
      params$df
    }))
  }

# save timeline table ----------------------------------------------------------

#' UI elements of module for saving the data
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
saveTimeline_UI <- function(id, label = "Save") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(
    ns("SaveTimeline"),
    label,
    class = "btn-success",
    icon = icon("saved", lib = "glyphicon")
  )
}

#' Server logic of module for saving the source data
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data source data as data.frame
#' @param study_id the current study ID
#' @return boolean value; TRUE if function was used.
saveTimeline_Server <-
  function(input, output, session, data, study_id) {
    params <- reactiveValues(check = FALSE)
    observeEvent(input$SaveTimeline, {
      if(is.null(study_id())){
        showNotification(
          "Please select and load a study in the 'Study' tab.",
          type = "error",
          duration = NULL
        )
      }
      req(study_id(), data())
      ns <- session$ns
      timeline_id <- tolower(gsub("-", "", session$ns("")))
      data_timeline <- data()
      data_filename_temp <-
        paste0("data_timeline_", timeline_id, ".txt.temp")
      meta_filename_temp <-
        paste0("meta_timeline_", timeline_id, ".txt.temp")
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
        V1 = c(
          "cancer_study_identifier",
          "genetic_alteration_type",
          "datatype",
          "data_filename"
        ),
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
      showNotification("Data saved successfully!",
        type = "message",
        duration = 10
      )

      # logging
      if (!is.null(logDir)) {
        writeLogfile(
          outdir = logDir,
          modified_file = file.path(study_id(), data_filename)
        )
        writeLogfile(
          outdir = logDir,
          modified_file = file.path(study_id(), meta_filename)
        )
      }
      params$check <- runif(1)
    })
    
    return(reactive({
      params$check
    }))
  }
