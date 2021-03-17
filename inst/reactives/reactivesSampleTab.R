# image
output$sampleDataImg <- renderImage(
  {
    return(
      list(
        src = system.file("www", "sample-data.PNG", package = "cbpManager"),
        contentType = "image/png",
        alt = "sample-data-image",
        width = "auto"
      )
    )
  },
  deleteFile = FALSE
)

# tour  ---------------------------------------------------------------
observeEvent(input$tour_sample, {
  tour <- read.delim(system.file("apphelp", "tour_sample.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# Data table output ---------------------------------------------------------------
output$sampleTable <- DT::renderDT({
  if (!is.null(loadedData$data_clinical_sample)) {
    DT::datatable(
      loadedData$data_clinical_sample,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    ) %>%
      DT::formatStyle(
        "PATIENT_ID",
        target = "row",
        backgroundColor = DT::styleEqual(
          c("Patient Identifier", "Patient identifier", "STRING"),
          c("lightblue", "lightblue", "lightblue")
        )
      )
  }
})

# get IDs (for other tabs)
sample_id_df <- reactiveValues(ids = NULL)
observeEvent(loadedData$data_clinical_sample, {
  sample_ids_df <- loadedData$data_clinical_sample[3:nrow(loadedData$data_clinical_sample),c("PATIENT_ID", "SAMPLE_ID")]
  sample_id_df$ids <- sample_ids_df
})

# add sample ---------------------------------------------------------------

# output reactive UIs per column
output$AddSampleUIs <- renderUI({
  lapply(
    colnames(loadedData$data_clinical_sample),
    function(colname) {
      generateUIwidgets(colname,
        mode = "add",
        tab = "Sample",
        patientIDs = patient_id_list$ids
      )
    }
  )
})

# show modalDialog for new sample
observeEvent(
  input$NewSample,
  showModal(
    modalDialog(
      size = "m",
      title = "Add sample",
      uiOutput("AddSampleUIs"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonAddSample", "Add")
      )
    )
  )
)
# validate inputs in modalDialog and add new sample to table
observeEvent(input$ModalbuttonAddSample, {
  all_reactive_inputs <- reactiveValuesToList(input)
  addSampleValues <-
    all_reactive_inputs[grep("addSampleInput_", names(all_reactive_inputs))]
  names(addSampleValues) <-
    gsub("addSampleInput_", "", names(addSampleValues))
  if (addSampleValues["PATIENT_ID"] == "") {
    showNotification("PATIENT_ID cannot be empty.",
      type = "error",
      duration = NULL
    )
  } else if (!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", addSampleValues["PATIENT_ID"])) {
    showNotification(
      "PATIENT_ID allows only numbers, letters, points, underscores and hyphens.",
      type = "error",
      duration = NULL
    )
  } else if (addSampleValues["SAMPLE_ID"] == "") {
    showNotification("SAMPLE_ID cannot be empty.",
      type = "error",
      duration = NULL
    )
  } else if (!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", addSampleValues["SAMPLE_ID"])) {
    showNotification(
      "SAMPLE_ID allows only numbers, letters, points, underscores and hyphens.",
      type = "error",
      duration = NULL
    )
  } else {
    #check if rbind would work according to number of input items
    if (all(colnames(loadedData$data_clinical_sample) %in% names(addSampleValues))) {
      loadedData$data_clinical_sample <-
        rbind(loadedData$data_clinical_sample, addSampleValues[colnames(loadedData$data_clinical_sample)])
    } else {
      print(
        "Number of input values does not match with number of columns. 
        Please contact the support."
      )
      showNotification(
        "Adding new row not possible. 
        Number of input values does not match with number of columns. 
        Please contact the support.",
        type = "error",
        duration = NULL
      )
    }
    removeModal()
  }
})

# edit sample ---------------------------------------------------------------
# output reactive UIs per column
output$EditSampleUIs <- renderUI({
  lapply(
    colnames(loadedData$data_clinical_sample),
    function(colname) {
      generateUIwidgets(
        colname,
        mode = "edit",
        tab = "Sample",
        data = loadedData$data_clinical_sample,
        selected_row = input$sampleTable_rows_selected,
        patientIDs = patient_id_list$ids
      )
    }
  )
})

# output UI edit name
output$EditNameSampleUIs <- renderUI({
  lapply(
    colnames(loadedData$data_clinical_sample),
    function(colname) {
      fluidRow(column(
        width = 8,
        textInput(
          inputId = paste0("editSampleInput_", colname),
          label = colname,
          value = loadedData$data_clinical_sample[input$sampleTable_rows_selected, colname]
        )
      ))
    }
  )
})

# output UI edit data_type
output$EditDataTypeSampleUIs <- renderUI({
  lapply(
    colnames(loadedData$data_clinical_sample),
    function(colname) {
      fluidRow(column(
        width = 8,
        selectInput(
          inputId = paste0("editSampleInput_", colname),
          label = colname,
          choices = c("STRING", "NUMBER", "BOOLEAN"),
          selected = loadedData$data_clinical_sample[input$data_clinical_sample, colname]
        )
      ))
    }
  )
})
# ModalDialog for editing a patient
observeEvent(input$EditSample,
  {
    if (is.null(input$sampleTable_rows_selected)) {
      showNotification("Please select a row", type = "warning", duration = NULL)
    } else if (input$sampleTable_rows_selected == 1) {
      showModal(modalDialog(
        title = "Edit short name of attribute",
        uiOutput("EditNameSampleUIs"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonEditSample", "Edit")
        )
      ))
    } else if (input$sampleTable_rows_selected == 2) {
      showModal(modalDialog(
        title = "Edit long name of attribute",
        uiOutput("EditNameSampleUIs"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonEditSample", "Edit")
        )
      ))
    } else if (input$sampleTable_rows_selected == 3) {
      showModal(modalDialog(
        title = "Edit data type",
        uiOutput("EditDataTypeSampleUIs"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonEditSample", "Edit")
        )
      ))
    } else {
      showModal(
        modalDialog(
          title = "Edit sample",
          uiOutput("EditSampleUIs"),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ModalbuttonEditSample", "Edit")
          )
        )
      )
    }
  },
  ignoreInit = TRUE
)

# validate edits and change values in the table
observeEvent(input$ModalbuttonEditSample, {
  all_reactive_inputs <- reactiveValuesToList(input)
  editSampleValues <-
    all_reactive_inputs[grep("editSampleInput_", names(all_reactive_inputs))]
  names(editSampleValues) <-
    gsub("editSampleInput_", "", names(editSampleValues))
  if (editSampleValues["PATIENT_ID"] == "") {
    showNotification("PATIENT_ID cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", editSampleValues["PATIENT_ID"])) {
    showNotification(
      "PATIENT_ID allows only numbers, letters, points, underscores and hyphens.",
      type = "error",
      duration = NULL
    )
  } else if (editSampleValues["SAMPLE_ID"] == "") {
    showNotification("SAMPLE_ID cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", editSampleValues["SAMPLE_ID"])) {
    showNotification(
      "SAMPLE_ID allows only numbers, letters, points, underscores and hyphens.",
      type = "error",
      duration = NULL
    )
  } else {
    for (i in colnames(loadedData$data_clinical_sample)) {
      loadedData$data_clinical_sample[input$sampleTable_rows_selected, i] <-
        editSampleValues[i]
    }
    removeModal()
  }
})

# delete sample ---------------------------------------------------------------
observeEvent(input$DeleteSample, {
  if (is.null(input$sampleTable_rows_selected)) {
    showNotification("Please select a row", type = "warning", duration = NULL)
  } else if (input$sampleTable_rows_selected == 1 |
    input$sampleTable_rows_selected == 2 |
    input$sampleTable_rows_selected == 3) {
    showNotification("Selected row cannot be deleted",
      type = "error",
      duration = NULL
    )
  } else {
    showModal(
      modalDialog(
        "Do you want to delete the selected sample entry?",
        title = "Delete",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonDeleteSample", "Delete")
        )
      )
    )
  }
})
observeEvent(input$ModalbuttonDeleteSample, {
  entry <- input$sampleTable_rows_selected

  loadedData$data_clinical_sample <-
    loadedData$data_clinical_sample[-entry, , drop = FALSE]
  removeModal()
})

# add column ---------------------------------------------------------------
# ModalDialog for adding a column
observeEvent(input$AddColumnSample,
  {
    showModal(
      modalDialog(
        title = "Add new column(s)",
        fluidRow(column(
          width = 8,
          radioButtons(
            "AddColSampleMode",
            label = "",
            choices = list(
              "Choose from pre-defined columns" = 1,
              "Add custom column" = 2
            ),
            selected = 1
          )
        )),
        uiOutput("AddColSampleUI"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonAddColSample", "Add column(s)")
        )
      )
    )
  },
  ignoreInit = TRUE
)
# output UI to select column that should be deleted
output$AddColSampleUI <- renderUI({
  if (input$AddColSampleMode == 1) {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = "SelColnameSample",
        label = "Select pre-defined column(s). (Some of them are entity specific)",
        choices = c(sampleCols$colname),
        multiple = TRUE
      )
    ))
  } else if (input$AddColSampleMode == 2) {
    fluidRow(column(
      width = 8,
      textInput(
        inputId = "colnameSample",
        label = "Column name:",
        placeholder = "e.g. ATTRIBUTE"
      ),
      textInput(
        inputId = "visShortNameSample",
        label = "Short name (visible in cBioPortal):",
        placeholder = "e.g. Attr."
      ),
      textInput(
        inputId = "visLongNameSample",
        label = "Long name (visible in cBioPortal):",
        placeholder = "e.g. Attribute of sample"
      ),
      selectInput(
        inputId = "typeofPat",
        label = "Data type:",
        choices = c("STRING", "NUMBER", "BOOLEAN"),
        selected = 1
      )
    ))
  }
})

observeEvent(input$ModalbuttonAddColSample, {
  if (input$AddColSampleMode == 1) {
    if (is.null(input$SelColnameSample)) {
      showNotification("Please select a column.",
        type = "error",
        duration = NULL
      )
    } else {
      # prevent overwriting existing columns
      colsToAdd <-
        input$SelColnameSample[!input$SelColnameSample %in% names(loadedData$data_clinical_sample)]

      loadedData$data_clinical_sample[colsToAdd] <- list("")
      for (col in colsToAdd) {
        loadedData$data_clinical_sample[1, col] <-
          sampleCols[which(sampleCols$colname == col), "shortColname"]
        loadedData$data_clinical_sample[2, col] <-
          sampleCols[which(sampleCols$colname == col), "longColname"]
        loadedData$data_clinical_sample[3, col] <-
          sampleCols[which(sampleCols$colname == col), "typeof"]
      }
      removeModal()
    }
  } else if (input$AddColSampleMode == 2) {
    if (input$colnameSample == "") {
      showNotification("Column name cannot be empty.",
        type = "error",
        duration = NULL
      )
    } else if (input$colnameSample %in% names(loadedData$data_clinical_sample)) {
      showNotification("Column name already exists.",
        type = "error",
        duration = NULL
      )
    } else if (input$visShortNameSample == "") {
      showNotification("Short name cannot be empty.",
        type = "error",
        duration = NULL
      )
    } else if (input$visLongNameSample == "") {
      showNotification("Long name cannot be empty.",
        type = "error",
        duration = NULL
      )
    } else if (toupper(input$colnameSample) %in% colnames(loadedData$data_clinical_sample)) {
      showNotification("Column already exists.",
        type = "error",
        duration = NULL
      )
    } else {
      colname <- .create_name(input$colnameSample)
      loadedData$data_clinical_sample %<>% mutate(!!(colname) := "")
      loadedData$data_clinical_sample[1, colname] <-
        input$visShortNameSample
      loadedData$data_clinical_sample[2, colname] <-
        input$visLongNameSample
      loadedData$data_clinical_sample[3, colname] <-
        input$typeofPat
      removeModal()
    }
  }
})

# delete column ---------------------------------------------------------------
observeEvent(input$DeleteColumnSample, {
  showModal(
    modalDialog(
      title = "Delete column(s)",
      fluidRow(column(
        width = 8,
        selectInput(
          inputId = "SelColnameSample",
          label = "Select column(s) for deletion:",
          choices = setdiff(
            colnames(loadedData$data_clinical_sample),
            c("SAMPLE_ID", "PATIENT_ID")
          ),
          multiple = TRUE
        )
      )),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonDeleteColSample", "Delete column(s)")
      )
    )
  )
})
observeEvent(input$ModalbuttonDeleteColSample, {
  loadedData$data_clinical_sample <-
    loadedData$data_clinical_sample[, !(names(loadedData$data_clinical_sample) %in% input$SelColnameSample), drop = FALSE]
  removeModal()
})
# save sample table ---------------------------------------------------------------
observeEvent(input$SaveDataSample,
  {
    if(is.null(loadedData$studyID)){
      showNotification(
        "Please select and load a study in the 'Study' tab.",
        type = "error",
        duration = NULL
      )
    }
    req(loadedData$studyID, loadedData$data_clinical_sample)
    # data_clinical_sample
    df <- convertDataFrame(loadedData$data_clinical_sample)

    write.table(
      df,
      file.path(
        study_dir,
        loadedData$studyID,
        "data_clinical_sample.txt.temp"
      ),
      append = FALSE,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(
        study_dir,
        loadedData$studyID,
        "data_clinical_sample.txt.temp"
      ),
      file.path(study_dir, loadedData$studyID, "data_clinical_sample.txt")
    )

    # logging
    if (!is.null(logDir)) {
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, "data_clinical_sample.txt")
      )
    }

    # meta_clinical_sample
    meta_patient_df <-
      data.frame(
        V1 = c(
          "cancer_study_identifier",
          "genetic_alteration_type",
          "datatype",
          "data_filename"
        ),
        V2 = c(
          loadedData$studyID,
          "CLINICAL",
          "SAMPLE_ATTRIBUTES",
          "data_clinical_sample.txt"
        )
      )
    write.table(
      meta_patient_df,
      file.path(
        study_dir,
        loadedData$studyID,
        "meta_clinical_sample.txt.temp"
      ),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(
        study_dir,
        loadedData$studyID,
        "meta_clinical_sample.txt.temp"
      ),
      file.path(study_dir, loadedData$studyID, "meta_clinical_sample.txt")
    )

    # logging
    if (!is.null(logDir)) {
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, "meta_clinical_sample.txt")
      )
    }

    # case lists
    case_list_dir <-
      file.path(study_dir, loadedData$studyID, "case_lists")
    ifelse(!dir.exists(case_list_dir),
      dir.create(case_list_dir),
      FALSE
    )

    cases_samples <-
      loadedData$data_clinical_sample[4:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
    cases_all_df <- data.frame(
      V1 = c(
        "cancer_study_identifier",
        "stable_id",
        "case_list_category",
        "case_list_name",
        "case_list_description",
        "case_list_ids"
      ),
      V2 = c(
        loadedData$studyID,
        paste0(loadedData$studyID, "_all"),
        "all_cases_in_study",
        "All Tumors",
        paste0(
          "All tumor samples (",
          nrow(loadedData$data_clinical_sample) - 3,
          " samples)"
        ),
        paste(cases_samples, collapse = "\t")
      )
    )
    write.table(
      cases_all_df,
      file.path(case_list_dir, "cases_all.txt.temp"),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )

    file.rename(
      file.path(case_list_dir, "cases_all.txt.temp"),
      file.path(case_list_dir, "cases_all.txt")
    )

    # logging
    if (!is.null(logDir)) {
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, "case_lists", "cases_all.txt")
      )
    }

    showNotification("Sample data saved successfully!",
      type = "message",
      duration = 10
    )
  },
  ignoreInit = TRUE
)
