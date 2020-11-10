# image
output$sampleDataImg <- renderImage({
  return(list(src = system.file("www", "sample-data.PNG", package = "cbpManager"),contentType = "image/png",alt = "sample-data-image", width="auto"))
}, deleteFile = FALSE)

#Data table output
output$sampleTable <- DT::renderDT({
  if(!is.null(loadedData$data_clinical_sample)){
    DT::datatable(loadedData$data_clinical_sample,
              selection = 'single',
              rownames = F, options = list(pageLength = 25, dom = 'ltipr', scrollX = TRUE)) %>%
      DT::formatStyle('PATIENT_ID', target = 'row', backgroundColor = DT::styleEqual(c("Patient Identifier", "Patient identifier"), c('lightblue', 'lightblue')))
  }
})

# add sample ####

# output reactive UIs per column
output$AddSampleUIs <- renderUI({
  lapply(colnames(loadedData$data_clinical_sample),
         function(colname){
           generateUIwidgets(colname, mode = "add", tab = "Sample", patientIDs = patient_id_list$ids)
         })
})

# output reactive UIs - oncotree specific
output$AddOncotreeUIs <- renderUI({
  if(input$addoncotree){
    reqColumns <- c("ONCOTREE_CODE", "CANCER_TYPE", "CANCER_TYPE_DETAILED", "TUMOR_TISSUE_SITE")
    colsToAdd <-reqColumns[!reqColumns%in%names(loadedData$data_clinical_sample)]
    fluidPage(
      fluidRow(
        p("Search the tumor type for this sample and select the row in the following table:"),
        DT::DTOutput("oncotree_tableModal"),
        lapply(colsToAdd, function(colname){generateOncotreeUIwidgets(colname, mode = "add")})
      )
    )
  }
})

observe(
  if(!is.null(input$oncotree_tableModal_row_last_clicked)){
    updateOncotreeUIwidgets(session, input$oncotree_tableModal_row_last_clicked, mode = "add")
  }
)

output$oncotree_tableModal <- DT::renderDT({
  columns_to_show <- c("code", "name", "mainType", "tissue")
  DT::datatable(oncotree[,columns_to_show], selection = 'single', rownames = FALSE, options = list(scrollX = TRUE, scrollY = TRUE, pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
})

# show modalDialog for new sample
observeEvent(input$NewSample,
             showModal(
               modalDialog(
                 size="m",
                 title = "Add sample",
                 uiOutput("AddSampleUIs"),
                 hr(),
                 h4("Add oncotree specific entries:"),
                 p("* If columns 'ONCOTREE_CODE', 'CANCER_TYPE', 'CANCER_TYPE_DETAILED' and 'TUMOR_TISSUE_SITE' does not exist yet, they will be added to the table."),
                 checkboxInput("addoncotree", label="Add oncotree", value = FALSE),
                 uiOutput("AddOncotreeUIs"),
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
  addSampleValues <- all_reactive_inputs[grep("addSampleInput_",names(all_reactive_inputs))]
  names(addSampleValues) <- gsub("addSampleInput_", "", names(addSampleValues))
  if(addSampleValues["PATIENT_ID"]==""){
    showNotification("PATIENT_ID cannot be empty.", type="error", duration = NULL)
  } else if(!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", addSampleValues["PATIENT_ID"])){
    showNotification("PATIENT_ID allows only numbers, letters, points, underscores and hyphens.", type="error", duration = NULL)
  } else if(addSampleValues["SAMPLE_ID"]==""){
    showNotification("SAMPLE_ID cannot be empty.", type="error", duration = NULL)
  } else if(!grepl("^[a-zA-Z0-9\\.\\_\\-]*$", addSampleValues["SAMPLE_ID"])){
    showNotification("SAMPLE_ID allows only numbers, letters, points, underscores and hyphens.", type="error", duration = NULL)
  } else if(input$addoncotree){
    # add missing columns
    reqColumns <- c("ONCOTREE_CODE", "CANCER_TYPE", "CANCER_TYPE_DETAILED", "TUMOR_TISSUE_SITE")
    loadedData$data_clinical_sample <- fncols(loadedData$data_clinical_sample, reqColumns)
    for(col in reqColumns){
      loadedData$data_clinical_sample[1,col] <- sampleCols[which(sampleCols$colname == col), "shortColname"]
      loadedData$data_clinical_sample[2,col] <- sampleCols[which(sampleCols$colname == col), "longColname"]
    }

    # add new row
    loadedData$data_clinical_sample <- rbind(loadedData$data_clinical_sample, addSampleValues[colnames(loadedData$data_clinical_sample)])
    removeModal()
  } else {
    loadedData$data_clinical_sample <- rbind(loadedData$data_clinical_sample, addSampleValues[colnames(loadedData$data_clinical_sample)])
    removeModal()
  }
})


# edit sample ####
# output reactive UIs per column
output$EditSampleUIs <- renderUI({
  lapply(colnames(loadedData$data_clinical_sample),
         function(colname){
           generateUIwidgets(colname, mode = "edit", tab = "Sample", data = loadedData$data_clinical_sample, selected_row = input$sampleTable_rows_selected, patientIDs = patient_id_list$ids)
         })
})

# output reactive UIs - oncotree specific
output$EditOncotreeUIs <- renderUI({
  if(input$addoncotree){
    reqColumns <- c("ONCOTREE_CODE", "CANCER_TYPE", "CANCER_TYPE_DETAILED", "TUMOR_TISSUE_SITE")
    colsToAdd <-reqColumns[!reqColumns%in%names(loadedData$data_clinical_sample)]
    fluidPage(
      fluidRow(
        p("Search the tumor type for this sample and select the row in the following table:"),
        DT::DTOutput("oncotree_tableModal"),
        lapply(colsToAdd, function(colname){generateOncotreeUIwidgets(colname, mode = "edit")})
      )
    )
  }
})

observe(
  if(!is.null(input$oncotree_tableModal_row_last_clicked)){
    updateOncotreeUIwidgets(session, input$oncotree_tableModal_row_last_clicked, mode = "edit")
  }
)

# ModalDialog for editing a patient
observeEvent(input$EditSample,{
  if(is.null(input$sampleTable_rows_selected)){
    showNotification("Please select a row", type="warning", duration = NULL)
  } else {
    showModal(
      modalDialog(
        title = "Edit sample",
        uiOutput("EditSampleUIs"),
        hr(),
        h4("Add oncotree specific entries:"),
        p("* If columns 'ONCOTREE_CODE', 'CANCER_TYPE', 'CANCER_TYPE_DETAILED' and 'TUMOR_TISSUE_SITE' does not exist yet, they will be added to the table."),
        checkboxInput("addoncotree", label="Add oncotree", value = FALSE),
        uiOutput("EditOncotreeUIs"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonEditSample", "Edit")
        )
      )
    )
  }
}, ignoreInit = T)

# validate edits and change values in the table
observeEvent(input$ModalbuttonEditSample, {
  all_reactive_inputs <- reactiveValuesToList(input)
  editSampleValues <- all_reactive_inputs[grep("editSampleInput_",names(all_reactive_inputs))]
  names(editSampleValues) <- gsub("editSampleInput_", "", names(editSampleValues))
  if(editSampleValues["SAMPLE_ID"]==""){
    showNotification("SAMPLE_ID cannot be empty.", type="error", duration = NULL)
  } else if(input$addoncotree){
    reqColumns <- c("ONCOTREE_CODE", "CANCER_TYPE", "CANCER_TYPE_DETAILED", "TUMOR_TISSUE_SITE")
    loadedData$data_clinical_sample <- fncols(loadedData$data_clinical_sample, reqColumns)
    for(col in reqColumns){
      loadedData$data_clinical_sample[1,col] <- sampleCols[which(sampleCols$colname == col), "shortColname"]
      loadedData$data_clinical_sample[2,col] <- sampleCols[which(sampleCols$colname == col), "longColname"]
    }

    for(i in colnames(loadedData$data_clinical_sample)){
      loadedData$data_clinical_sample[which(loadedData$data_clinical_sample$SAMPLE_ID == editSampleValues$SAMPLE_ID),i] <- editSampleValues[i]
    }
    removeModal()
  } else {
    for(i in colnames(loadedData$data_clinical_sample)){
      loadedData$data_clinical_sample[which(loadedData$data_clinical_sample$SAMPLE_ID == editSampleValues$SAMPLE_ID),i] <- editSampleValues[i]
    }
    removeModal()
  }
})

# delete sample ####
observeEvent(input$DeleteSample, {
  if(is.null(input$sampleTable_rows_selected)){

    showNotification("Please select a row", type="warning", duration = NULL)
  } else if(input$sampleTable_rows_selected == 1 | input$sampleTable_rows_selected == 2){
    showNotification("Selected row cannot be deleted", type="error", duration = NULL)
  } else {
    showModal(modalDialog(
      "Do you want to delete the selected sample entry?",
      title = "Delete",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonDeleteSample", "Delete")
      )
    ))
  }
})
observeEvent(input$ModalbuttonDeleteSample, {
  entry <- input$sampleTable_rows_selected

  loadedData$data_clinical_sample <- loadedData$data_clinical_sample[-entry,,drop = FALSE]
  removeModal()
})

# add column ####
# ModalDialog for adding a column
observeEvent(input$AddColumnSample,{
  showModal(
    modalDialog(
      title = "Add new column(s)",
      fluidRow(column(
        width = 8,
        radioButtons("AddColSampleMode", label = "",
                     choices = list("Choose from pre-defined columns" = 1, "Add custom column" = 2),
                     selected = 1))),
      uiOutput("AddColSampleUI"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonAddColSample", "Add column(s)")
      )
    ))
}, ignoreInit = T)
# output UI to select column that should be deleted
output$AddColSampleUI <- renderUI({
  if(input$AddColSampleMode == 1){
    fluidRow(column(
      width = 8,
      selectInput(inputId="SelColnameSample",
                  label = "Select pre-defined column(s)",
                  choices = c(sampleCols$colname),
                  multiple = TRUE
      ))
    )
  } else if (input$AddColSampleMode == 2){
    fluidRow(column(
      width = 8,
      textInput(inputId="colnameSample",
                label = "Column name:",
                placeholder = "e.g. ATTRIBUTE"
      ),
      textInput(inputId="visShortNameSample",
                label = "Short name (visible in cBioPortal):",
                placeholder = "e.g. Attr."
      ),
      textInput(inputId="visLongNameSample",
                label = "Long name (visible in cBioPortal):",
                placeholder = "e.g. Attribute of sample"
      ))
    )
  }
})

observeEvent(input$ModalbuttonAddColSample, {
  if(input$AddColSampleMode == 1){
    if(is.null(input$SelColnameSample)){
      showNotification("Please select a column.", type="error", duration = NULL)
    } else {
      # prevent overwriting existing columns
      colsToAdd <-input$SelColnameSample[!input$SelColnameSample%in%names(loadedData$data_clinical_sample)]

      loadedData$data_clinical_sample[colsToAdd] <- list("")
      for(col in colsToAdd){
        loadedData$data_clinical_sample[1,col] <- sampleCols[which(sampleCols$colname == col), "shortColname"]
        loadedData$data_clinical_sample[2,col] <- sampleCols[which(sampleCols$colname == col), "longColname"]
      }
      removeModal()
    }
  } else if (input$AddColSampleMode == 2){
    if(input$colnameSample == ""){
      showNotification("Column name cannot be empty.", type="error", duration = NULL)
    } else if(input$colnameSample %in% names(loadedData$data_clinical_sample)){
      showNotification("Column name already exists.", type="error", duration = NULL)
    } else if(input$visShortNameSample == ""){
      showNotification("Short name cannot be empty.", type="error", duration = NULL)
    } else if(input$visLongNameSample == ""){
      showNotification("Long name cannot be empty.", type="error", duration = NULL)
    } else if(toupper(input$colnameSample) %in% colnames(loadedData$data_clinical_sample)){
      showNotification("Column already exists.", type="error", duration = NULL)
    } else {
      colname <- .create_name(input$colnameSample)
      loadedData$data_clinical_sample %<>% mutate(!!(colname) := "")
      loadedData$data_clinical_sample[1,colname] <- input$visShortNameSample
      loadedData$data_clinical_sample[2,colname] <- input$visLongNameSample
      removeModal()
    }
  }
})

# delete column ####
observeEvent(input$DeleteColumnSample, {
  showModal(modalDialog(
    title = "Delete column(s)",
    fluidRow(column(
      width = 8,
      selectInput(inputId="SelColnameSample",
                  label = "Select column(s) for deletion:",
                  choices = setdiff(colnames(loadedData$data_clinical_sample), c("SAMPLE_ID", "PATIENT_ID")),
                  multiple = TRUE
      ))
    ),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ModalbuttonDeleteColSample", "Delete column(s)")
    )
  ))
})
observeEvent(input$ModalbuttonDeleteColSample, {
  loadedData$data_clinical_sample <- loadedData$data_clinical_sample[,!(names(loadedData$data_clinical_sample) %in% input$SelColnameSample), drop = FALSE]
  removeModal()
})
# save sample table ####
observeEvent(input$SaveDataSample, {
  # data_clinical_sample
  df <- convertDataFrame(loadedData$data_clinical_sample)
  df[df==""]<-NA
  write.table(df, file.path(study_dir, input$cancer_study_identifier, "data_clinical_sample.txt.temp"), append = FALSE, sep = "\t",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  file.rename(file.path(study_dir, input$cancer_study_identifier, "data_clinical_sample.txt.temp"), file.path(study_dir, input$cancer_study_identifier, "data_clinical_sample.txt"))

  # meta_clinical_sample
  meta_patient_df <- data.frame(V1 = c("cancer_study_identifier", "genetic_alteration_type", "datatype", "data_filename"),
                                V2 = c(input$cancer_study_identifier, "CLINICAL", "SAMPLE_ATTRIBUTES", "data_clinical_sample.txt")
  )
  write.table(meta_patient_df, file.path(study_dir, input$cancer_study_identifier, "meta_clinical_sample.txt.temp"), append = FALSE, sep = ": ",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  file.rename(file.path(study_dir, input$cancer_study_identifier, "meta_clinical_sample.txt.temp"), file.path(study_dir, input$cancer_study_identifier, "meta_clinical_sample.txt"))

  # case lists
  case_list_dir <- file.path(study_dir, input$cancer_study_identifier, "case_lists")
  ifelse(!dir.exists(case_list_dir), dir.create(case_list_dir), FALSE)

  cases_samples <- loadedData$data_clinical_sample[3:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
  cases_all_df <- data.frame(V1 = c("cancer_study_identifier",
                                    "stable_id",
                                    "case_list_category",
                                    "case_list_name",
                                    "case_list_description",
                                    "case_list_ids"),
                             V2 = c(input$cancer_study_identifier,
                                    paste0(input$cancer_study_identifier, "_all"),
                                    "all_cases_in_study",
                                    "All Tumors",
                                    paste0("All tumor samples (", nrow(loadedData$data_clinical_sample)-2, " samples)"),
                                    paste(cases_samples, collapse = "\t"))
  )
  write.table(cases_all_df, file.path(case_list_dir, "cases_all.txt.temp"), append = FALSE, sep = ": ",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  file.rename(file.path(case_list_dir, "cases_all.txt.temp"), file.path(case_list_dir, "cases_all.txt"))
  showNotification("Sample data saved successfully!", type="message", duration = 10)
}, ignoreInit = T)
