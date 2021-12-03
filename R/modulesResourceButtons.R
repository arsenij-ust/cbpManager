#' Validate resource_definition input
#'
#'
#' @param values List of input values
#' @param resourceDf data.frame of data_resource_definition
#' @param mode The mode of the function ('add' or 'edit')
#'
#' @return boolean
validateResourceDefinition <- function(values, resourceDf, mode = "add") {
  if (rapportools::is.empty(values[["RESOURCE_ID"]])) {
    showNotification("'RESOURCE_ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (values[["RESOURCE_ID"]] %in% resourceDf$RESOURCE_ID & mode != "edit") {
    showNotification("'RESOURCE_ID' already exists.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", values[["RESOURCE_ID"]])) {
    showNotification(
      "'RESOURCE_ID' allows only numbers, letters, points, underscores and hyphens.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["DISPLAY_NAME"]])) {
    showNotification("'DISPLAY_NAME' is required.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Validate resource_sample input
#'
#'
#' @param values List of input values
#' @return boolean
validateResourceSample <- function(values) {
  if (rapportools::is.empty(values[["RESOURCE_ID"]])) {
    showNotification("'RESOURCE_ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["SAMPLE_ID"]])) {
    showNotification("'Sample ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["PATIENT_ID"]])) {
    showNotification("'Patient ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["URL"]])) {
    showNotification("'URL' is required.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (!grepl("^http.*", values[["URL"]])) {
    showNotification("'URL' should start with 'http' or 'https'.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Validate resource_patient input
#'
#'
#' @param values List of input values
#' @return boolean
validateResourcePatient <- function(values) {
  if (rapportools::is.empty(values[["RESOURCE_ID"]])) {
    showNotification("'RESOURCE_ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["PATIENT_ID"]])) {
    showNotification("'Patient ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["URL"]])) {
    showNotification("'URL' is required.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (!grepl("^http.*", values[["URL"]])) {
    showNotification("'URL' should start with 'http' or 'https'.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Validate resource_study input
#'
#'
#' @param values List of input values
#' @return boolean
validateResourceStudy <- function(values) {
  if (rapportools::is.empty(values[["RESOURCE_ID"]])) {
    showNotification("'RESOURCE_ID' is requiered.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (rapportools::is.empty(values[["URL"]])) {
    showNotification("'URL' is required.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else if (!grepl("^http.*", values[["URL"]])) {
    showNotification("'URL' should start with 'http' or 'https'.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}
# add entry --------------------------------------------------------------------

#' UI elements of Resource tab module for adding a row
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
addRowRc_UI <- function(id, label = "Add") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddEntry"), label, icon = icon("plus", lib = "glyphicon"))
}

#' Server logic of Resource tab module for adding a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data Source data as data.frame
#' @param patient_ids Reactive vector of existing patient IDs
#' @param sample_ids Reactive data.frame of existing patient IDs and sample IDs
#' @param resource_ids Reactive data.frame of data_resource_definition
#' @param resource_type The type of the resource.
#' Can be "definition", "sample", "patient", "study"
#'
#' @return reactive data.frame of modified source data
addRowRc_Server <-
  function(input,
           output,
           session,
           data,
           patient_ids = NULL,
           sample_ids = NULL,
           resource_ids = NULL,
           resource_type = c("definition", "sample", "patient", "study")) {
    resource_type <- match.arg(resource_type)

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
          if (colname == "RESOURCE_ID") {
            if (resource_type == "definition") {
              fluidRow(column(
                width = 8,
                textInput(
                  inputId = ns(colname),
                  label = "Unique RESOURCE_ID",
                  value = ""
                ),
              ))
            } else {
              subsetIDs <- resource_ids()[resource_ids()$RESOURCE_TYPE == toupper(resource_type), ]$RESOURCE_ID
              fluidRow(column(
                width = 8,
                selectInput(
                  inputId = ns(colname),
                  label = "Select RESOURCE_ID",
                  choices = c("", subsetIDs),
                  selected = 1
                ),
              ))
            }
          } else if (colname == "RESOURCE_TYPE") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select RESOURCE_ID",
                choices = c("SAMPLE", "PATIENT", "STUDY"),
                selected = 1
              ),
            ))
          } else if (colname == "OPEN_BY_DEFAULT") {
            fluidRow(column(
              width = 8,
              checkboxInput(
                inputId = ns(colname),
                label = "Define if the resource will be open in extra tab",
                value = FALSE
              ),
            ))
          } else if (colname == "PRIORITY") {
            fluidRow(column(
              width = 8,
              numericInput(
                inputId = ns(colname),
                label = colname,
                value = 1,
                min = 1,
                step = 1
              ),
            ))
          } else if (colname == "PATIENT_ID") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select Patient ID",
                choices = c("", patient_ids()),
                selected = 1
              ),
            ))
          } else if (colname == "SAMPLE_ID") {
            # to do: subset sampleIDs to selected patientID
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select Patient ID",
                choices = c("", sample_ids()$SAMPLE_ID),
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
      addResourceValues <-
        all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
      # check resource definition values
      if (resource_type == "definition") {
        if (validateResourceDefinition(
          values = addResourceValues,
          resourceDf = resource_ids()
        )) {
          params$df <-
            plyr::rbind.fill(data(), as.data.frame(addResourceValues))
          removeModal()
        }
        # check resource sample values
      } else if (resource_type == "sample") {
        if (validateResourceSample(values = addResourceValues)) {
          params$df <-
            plyr::rbind.fill(data(), as.data.frame(addResourceValues))
          removeModal()
        }
        # check resource patient values
      } else if (resource_type == "patient") {
        if (validateResourcePatient(values = addResourceValues)) {
          params$df <-
            plyr::rbind.fill(data(), as.data.frame(addResourceValues))
          removeModal()
        }
        # check resource study values
      } else if (resource_type == "study") {
        if (validateResourceStudy(values = addResourceValues)) {
          params$df <-
            plyr::rbind.fill(data(), as.data.frame(addResourceValues))
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
#' @param id Module id
#' @param label Label of the button
#' @return UI module
editRowRc_UI <- function(id, label = "Edit") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("EditEntry"), label, icon = icon("pencil", lib = "glyphicon"))
}

#' Server logic of Resource tab module for editing a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data Source data as data.frame
#' @param patient_ids Reactive vector of existing patient IDs
#' @param sample_ids Reactive data.frame of existing patient IDs and sample IDs
#' @param resource_ids Reactive data.frame of data_resource_definition
#' @param selected_row Index of the selected row
#' @param resource_type The type of the resource.
#' Can be "definition", "sample", "patient", "study"
#'
#' @return reactive data.frame of modified source data
editRowRc_Server <-
  function(input,
           output,
           session,
           data,
           patient_ids = NULL,
           sample_ids = NULL,
           resource_ids = NULL,
           selected_row = NULL,
           resource_type = c("definition", "sample", "patient", "study")) {
    resource_type <- match.arg(resource_type)

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
          if (colname == "RESOURCE_ID") {
            if (resource_type == "definition") {
              fluidRow(column(
                width = 8,
                textInput(
                  inputId = ns(colname),
                  label = "Unique RESOURCE_ID",
                  value = data()[selected_row(), colname]
                ),
              ))
            } else {
              subsetIDs <- resource_ids()[resource_ids()$RESOURCE_TYPE == toupper(resource_type), ]$RESOURCE_ID
              fluidRow(column(
                width = 8,
                selectInput(
                  inputId = ns(colname),
                  label = "Select RESOURCE_ID",
                  choices = c("", subsetIDs),
                  selected = data()[selected_row(), colname]
                ),
              ))
            }
          } else if (colname == "RESOURCE_TYPE") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select RESOURCE_ID",
                choices = c("SAMPLE", "PATIENT", "STUDY"),
                selected = data()[selected_row(), colname]
              ),
            ))
          } else if (colname == "OPEN_BY_DEFAULT") {
            fluidRow(column(
              width = 8,
              checkboxInput(
                inputId = ns(colname),
                label = "Define if the resource will be open in extra tab",
                value = FALSE
              ),
            ))
          } else if (colname == "PRIORITY") {
            fluidRow(column(
              width = 8,
              numericInput(
                inputId = ns(colname),
                label = colname,
                value = data()[selected_row(), colname],
                min = 1,
                step = 1
              ),
            ))
          } else if (colname == "PATIENT_ID") {
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select Patient ID",
                choices = c("", patient_ids()),
                selected = data()[selected_row(), colname]
              ),
            ))
          } else if (colname == "SAMPLE_ID") {
            # to do: subset sampleIDs to selected patientID
            fluidRow(column(
              width = 8,
              selectInput(
                inputId = ns(colname),
                label = "Select Patient ID",
                choices = c("", sample_ids()$SAMPLE_ID),
                selected = data()[selected_row(), colname]
              ),
            ))
          } else {
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

    # validate inputs in modalDialog and add new entry to table
    params <- reactiveValues(df = NULL)
    observeEvent(input$ModalbuttonEdit, {
      all_reactive_inputs <- reactiveValuesToList(input)
      editResourceValues <-
        all_reactive_inputs[names(all_reactive_inputs) %in% names(data())]
      # check resource definition values
      if (resource_type == "definition") {
        if (validateResourceDefinition(
          values = editResourceValues,
          resourceDf = resource_ids(),
          mode = "edit"
        )) {
          params$df <- data()
          for (i in colnames(params$df)) {
            params$df[selected_row(), i] <- editResourceValues[[i]]
          }
          removeModal()
        }
        # check resource sample values
      } else if (resource_type == "sample") {
        if (validateResourceSample(values = editResourceValues)) {
          params$df <- data()
          for (i in colnames(params$df)) {
            params$df[selected_row(), i] <- editResourceValues[i]
          }
          removeModal()
        }
        # check resource patient values
      } else if (resource_type == "patient") {
        if (validateResourcePatient(values = editResourceValues)) {
          params$df <- data()
          for (i in colnames(params$df)) {
            params$df[selected_row(), i] <- editResourceValues[i]
          }
          removeModal()
        }
        # check resource study values
      } else if (resource_type == "study") {
        if (validateResourceStudy(values = editResourceValues)) {
          params$df <- data()
          for (i in colnames(params$df)) {
            params$df[selected_row(), i] <- editResourceValues[i]
          }
          removeModal()
        }
      }
    })
    return(reactive({
      params$df
    }))
  }

# delete entry -----------------------------------------------------------------

#' UI elements of module for removing a row
#'
#' @param id Module id
#' @param label Label of the button
#' @return UI module
deleteRowRc_UI <- function(id, label = "Delete") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("DeleteEntry"), label, icon = icon("remove", lib = "glyphicon"))
}

#' Server logic of Resource tab module for deleting a row
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data Source data as data.frame
#' @param mode If 'recursive' the resources bind to the resource definition will be deleted.
#' @param sample_data Data of the resource from type 'sample'
#' @param patient_data Data of the resource from type 'patient'
#' @param study_data Data of the resource from type 'study'
#' @param selected_row Index of the selected row from the table
#'
#' @return reactive data.frame of modified source data
deleteRowRc_Server <-
  function(input, output, session, data, selected_row, mode = "default", sample_data = NULL, patient_data = NULL, study_data = NULL) {
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
            "Do you want to delete the selected entry? All resources corresponding to this 'RESOURCE_ID' will be deleted!",
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
    params <- reactiveValues(df = NULL, sample = NULL, patient = NULL, study = NULL)
    observeEvent(input$ModalbuttonDeleteEntry, {
      entry <- selected_row()
      params$sample <- sample_data()
      params$patient <- patient_data()
      params$study <- study_data()

      params$df <- data()[-entry, , drop = FALSE]
      if (mode == "recursive") {
        fromTable <- data()[entry, ]$RESOURCE_TYPE
        resourceID <- data()[entry, ]$RESOURCE_ID
        if (fromTable == "SAMPLE") {
          params$sample <- sample_data()[sample_data()$RESOURCE_ID != resourceID, ]
        } else if (fromTable == "PATIENT") {
          params$patient <- patient_data()[patient_data()$RESOURCE_ID != resourceID, ]
        } else if (fromTable == "STUDY") {
          params$study <- study_data()[study_data()$RESOURCE_ID != resourceID, ]
        }
      }
      removeModal()
    })
    return(reactive({
      params
    }))
  }


# save resource table ----------------------------------------------------------

#' UI elements of module for saving the resource data
#'
#' @param id module id
#' @param label label of the button
#' @return UI module
saveResource_UI <- function(id, label = "Save") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(
    ns("SaveResource"),
    label,
    class = "btn-success",
    icon = icon("saved", lib = "glyphicon")
  )
}

#' Server logic of module for saving the resource data
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param data Source data as data.frame
#' @param study_id The current study ID
#' @param data_filename File name of the data file
#' @param meta_filename file name of the meta file
#' @param resource_type The type of the resource.
#' Can be "definition", "sample", "patient", "study"
#'
#' @return boolean value; TRUE if function was used.
saveResource_Server <-
  function(input,
           output,
           session,
           data,
           study_id,
           data_filename,
           meta_filename,
           resource_type = c("SAMPLE", "DEFINITION", "PATIENT", "STUDY")) {
    resource_type <- match.arg(resource_type)
    params <- reactiveValues(check = FALSE)
    observeEvent(input$SaveResource, {
      if(is.null(study_id())){
        showNotification(
          "Please select and load a study in the 'Study' tab.",
          type = "error",
          duration = NULL
        )
      }
      req(data(), study_id())
      ns <- session$ns
      resource_data <- data()
      data_filename_temp <-
        paste0(data_filename, ".temp")
      meta_filename_temp <-
        paste0(meta_filename, ".temp")

      # write data
      write.table(
        resource_data,
        file.path(study_dir, study_id(), data_filename_temp),
        append = FALSE,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE,
        na = ""
      )

      # write meta
      meta_timeline <- data.frame(
        V1 = c(
          "cancer_study_identifier",
          "resource_type",
          "data_filename"
        ),
        V2 = c(study_id(), resource_type, data_filename)
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
      writeLogfile(
        outdir = study_dir,
        modified_file = file.path(study_id(), data_filename)
      )
      writeLogfile(
        outdir = study_dir,
        modified_file = file.path(study_id(), meta_filename)
      )
      params$check <- runif(1)
    })
    return(reactive({
      params$check
    }))
  }
