# image ---------------------------------------------------------------
output$MutDataImg <- renderImage(
  {
    return(
      list(
        src = system.file("www", "maf-data.PNG", package = "cbpManager"),
        contentType = "image/png",
        alt = "annotation-example",
        width = "auto"
      )
    )
  },
  deleteFile = FALSE
)

# tour  ---------------------------------------------------------------
observeEvent(input$tour_mutation, {
  tour <- read.delim(system.file("apphelp", "tour_mutation.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# upload file ---------------------------------------------------------------
upload <- reactiveValues(data = NULL)
observeEvent(input$chooseMAF, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
  if(!grepl("\\.[txt|tsv|maf|MAF|csv]", input$chooseMAF$name)){
    showNotification(
      "The file format is not supported.
      File should be '.txt', '.tsv', '.maf', '.MAF', or '.csv'.",
      type = "error",
      duration = NULL
    )
  } else {
    uploaded_data <-
      as.data.frame(vroom::vroom(input$chooseMAF$datapath, delim = "\t"))
    requiredCols <-
      c(
        "Hugo_Symbol",
        "Tumor_Sample_Barcode",
        "Variant_Classification",
        "HGVSp_Short"
      )
    if (any(!requiredCols %in% colnames(uploaded_data))) {
      showNotification(
        "One or more of the required columns are missing. Required column names are: Hugo_Symbol, Tumor_Sample_Barcode, Variant_Classification, and HGVSp_Short. Please upload file again.",
        type = "error",
        duration = NULL
      )
    } else {
      upload$data <- uploaded_data
    }
  }

})


# output for adding the uploaded file to Mutation data
output$AddUploadedMAFUI <- renderUI({
  requiredCols <-
    c(
      "Hugo_Symbol",
      "Tumor_Sample_Barcode",
      "Variant_Classification",
      "HGVSp_Short"
    )
  if (input$select_AddMAFMode == 2) {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = "SelColnameMAF",
        label = "Select columns, that should be imported to the Mutation data? 
        (The four required columns will be imported anyway.)",
        choices = setdiff(
          colnames(upload$data),
          requiredCols),
        multiple = TRUE
      )
    ))
  }
})

# show modalDialog for adding the uploaded file to Mutation data
observeEvent(
  input$AddPreview,
  {
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
        title = "Import uploaded data to Mutaion data",
        radioButtons("select_AddMAFMode", label = NULL,
                     choices = list("Add all columns" = 1, "Add selected columns (+ required columns)" = 2)),
        uiOutput("AddUploadedMAFUI"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonAddUploadedMAF", "Add")
        )
      )
    )
  }
)

# check MAF data that should be added to the Mutation data and add all or selected columns
observeEvent(input$ModalbuttonAddUploadedMAF, {
  req_cols <- c(
    "Hugo_Symbol",
    "Tumor_Sample_Barcode",
    "Variant_Classification",
    "HGVSp_Short"
  )

  if (!all(upload$data$Tumor_Sample_Barcode %in% sample_id_df$ids$SAMPLE_ID)){
    showNotification(
      "One or more of the Tumor_Sample_Barcodes in the MAF file do not exist in the Sample data (SAMPLE_ID column). Please navigate to the Sample tab and add the respective Samples.",
      type = "error",
      duration = NULL
    )
  } else if (nrow(dplyr::inner_join(upload$data[,req_cols], loadedData$data_mutations_extended[,req_cols])) >= 1){
    showNotification(
      "One or more entries of the MAF file (based on the columns Hugo_Symbol, Tumor_Sample_Barcode, Variant_Classification, and HGVSp_Short) are already present in the Mutation data.",
      type = "error",
      duration = NULL
    )
  } else if (input$select_AddMAFMode == 1){
    colOrder <- c(req_cols, setdiff(
      colnames(upload$data),
      req_cols))
   loadedData$data_mutations_extended <-
    dplyr::bind_rows(upload$data[,colOrder], loadedData$data_mutations_extended)
   
   # change tracker
   study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
   
   removeModal()
  } else if (input$select_AddMAFMode == 2){
    selCols <- c(req_cols, input$SelColnameMAF)
    loadedData$data_mutations_extended <-
      dplyr::bind_rows(upload$data[,selCols], loadedData$data_mutations_extended)
    
    # change tracker
    study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
    
    removeModal()
  }

})

# show table ---------------------------------------------------------------
output$previewMAF <- DT::renderDT({
  DT::datatable(upload$data,
    options = list(scrollX = TRUE)
  )
})

output$mafTable <- DT::renderDT({
  if (!is.null(loadedData$data_mutations_extended)) {
    DT::datatable(
      loadedData$data_mutations_extended,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  }
})

Valid_Variant_Classification <- c("Unknown", "Frame_Shift_Del", "Frame_Shift_Ins", "In_Frame_Del", "In_Frame_Ins", "Missense_Mutation", "Nonsense_Mutation", "Silent", "Splice_Site", "Translation_Start_Site", "Nonstop_Mutation", "3'UTR", "3'Flank", "5'UTR", "5'Flank", "IGR", "Intron", "RNA", "Targeted_Region", "De_novo_Start_InFrame", "De_novo_Start_OutOfFrame", "Splice_Region")

predefined_MAF_columns <- c(
"Entrez_Gene_Id", "Center", "NCBI_Build", "Chromosome", "Start_Position", "End_Position", "Strand", "Variant_Type", "Reference_Allele", "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "dbSNP_RS", "dbSNP_Val_Status", "Matched_Norm_Sample_Barcode", "Match_Norm_Seq_Allele1", "Match_Norm_Seq_Allele2", "Tumor_Validation_Allele1", "Tumor_Validation_Allele2", "Match_Norm_Validation_Allele1", "Match_Norm_Validation_Allele2", "Verification_Status", "Validation_Status", "Mutation_Status", "Sequencing_Phase", "Sequence_Source", "Validation_Method", "Score", "BAM_File", "Sequencer", "t_alt_count", "t_ref_count", "n_alt_count", "n_ref_count"
)

# add entry ---------------------------------------------------------------

# output reactive UIs per column
output$AddMAFentryUIs <- renderUI({
  lapply(
    colnames(loadedData$data_mutations_extended),
    function(colname) {
      generateUIwidgets(colname,
                        mode = "add",
                        tab = "Mutation",
                        sampleIDs = sample_id_df$ids$SAMPLE_ID,
                        patientIDs = Valid_Variant_Classification
      )
    }
  )
})

# show modalDialog for new sample
observeEvent(
  input$NewMAFentry,
  {
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
        title = "Add mutation annotation",
        uiOutput("AddMAFentryUIs"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonAddMAFentry", "Add")
        )
      )
    )
  }
)
# validate inputs in modalDialog and add new sample to table
observeEvent(input$ModalbuttonAddMAFentry, {
  req_cols <- c(
    "Hugo_Symbol",
    "Tumor_Sample_Barcode",
    "Variant_Classification",
    "HGVSp_Short"
  )
  all_reactive_inputs <- reactiveValuesToList(input)
  addMAFValues <-
    all_reactive_inputs[grep("addMutationInput_", names(all_reactive_inputs))]
  names(addMAFValues) <-
    gsub("addMutationInput_", "", names(addMAFValues))
  if (addMAFValues["Tumor_Sample_Barcode"] == "") {
    showNotification("Tumor_Sample_Barcode cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (addMAFValues["Hugo_Symbol"] == "") {
    showNotification("Hugo_Symbol cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (addMAFValues["Variant_Classification"] == "") {
    showNotification("Variant_Classification cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (addMAFValues["HGVSp_Short"] == "") {
    showNotification("HGVSp_Short cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (nrow(
    dplyr::inner_join(
      data.frame(
        Tumor_Sample_Barcode=addMAFValues["Tumor_Sample_Barcode"],
        Hugo_Symbol = addMAFValues["Hugo_Symbol"],
        Variant_Classification = addMAFValues["Variant_Classification"],
        HGVSp_Short = addMAFValues["HGVSp_Short"]
      ), 
      loadedData$data_mutations_extended[,req_cols])) >= 1){
    showNotification(
      "An entry with identical Hugo_Symbol, Tumor_Sample_Barcode, Variant_Classification, and HGVSp_Short is already present in the Mutation data.",
      type = "error",
      duration = NULL
    )
  } else {
    #check if rbind would work according to number of input items
    if (all(colnames(loadedData$data_mutations_extended) %in% names(addMAFValues))) {
      loadedData$data_mutations_extended <-
        rbind(addMAFValues[colnames(loadedData$data_mutations_extended)], loadedData$data_mutations_extended)
      
      # change tracker
      study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
      
    } else {
      message(
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

# edit entry ---------------------------------------------------------------
# output reactive UIs per column
output$EditMAFUIs <- renderUI({
  lapply(
    colnames(loadedData$data_mutations_extended),
    function(colname) {
      generateUIwidgets(
        colname,
        mode = "edit",
        tab = "Mutation",
        data = loadedData$data_mutations_extended,
        selected_row = input$mafTable_rows_selected,
        sampleIDs = sample_id_df$ids$SAMPLE_ID,
        patientIDs = Valid_Variant_Classification
      )
    }
  )
})

# ModalDialog for editing a patient
observeEvent(input$EditMAFentry,
             {
               if(is.null(loadedData$studyID)){
                 showNotification(
                   "Please load a study in the 'Study' tab first.",
                   type = "error",
                   duration = NULL
                 )
                 return(NULL)
               }
               if (is.null(input$mafTable_rows_selected)) {
                 showNotification("Please select a row", type = "warning", duration = NULL)
               } else {
                 showModal(
                   modalDialog(
                     title = "Edit mutation annotation",
                     uiOutput("EditMAFUIs"),
                     easyClose = FALSE,
                     footer = tagList(
                       modalButton("Cancel"),
                       actionButton("ModalbuttonEditMAF", "Edit")
                     )
                   )
                 )
               }
             },
             ignoreInit = TRUE
)

# validate edits and change values in the table
observeEvent(input$ModalbuttonEditMAF, {
  all_reactive_inputs <- reactiveValuesToList(input)
  editMAFValues <-
    all_reactive_inputs[grep("editMutationInput_", names(all_reactive_inputs))]
  names(editMAFValues) <-
    gsub("editMutationInput_", "", names(editMAFValues))
  if (editMAFValues["Tumor_Sample_Barcode"] == "") {
    showNotification("Tumor_Sample_Barcode cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (editMAFValues["Hugo_Symbol"] == "") {
    showNotification("Hugo_Symbol cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (editMAFValues["Variant_Classification"] == "") {
    showNotification("Variant_Classification cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else if (editMAFValues["HGVSp_Short"] == "") {
    showNotification("HGVSp_Short cannot be empty.",
                     type = "error",
                     duration = NULL
    )
  } else {
    for (i in colnames(loadedData$data_mutations_extended)) {
      loadedData$data_mutations_extended[input$mafTable_rows_selected, i] <-
        editMAFValues[i]
    }
    
    # change tracker
    study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
    
    removeModal()
  }
})

# delete entry ---------------------------------------------------------------
observeEvent(input$DeleteMAFentry, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
  if (is.null(input$mafTable_rows_selected)) {
    showNotification("Please select a row", type = "warning", duration = NULL)
  } else {
    showModal(
      modalDialog(
        "Do you want to delete the selected mutation annotation?",
        title = "Delete",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ModalbuttonDeleteMAFentry", "Delete")
        )
      )
    )
  }
})
observeEvent(input$ModalbuttonDeleteMAFentry, {
  entry <- input$mafTable_rows_selected

  loadedData$data_mutations_extended <-
    loadedData$data_mutations_extended[-entry, , drop = FALSE]
  
  # change tracker
  study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
  
  removeModal()
})

# # add column ---------------------------------------------------------------
# ModalDialog for adding a column
observeEvent(input$AddColumnMAFentry,
             {
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
                   title = "Add new column(s)",
                   fluidRow(column(
                     width = 8,
                     radioButtons(
                       "AddColMAFMode",
                       label = "",
                       choices = list(
                         "Choose from pre-defined columns" = 1,
                         "Add custom column" = 2
                       ),
                       selected = 1
                     )
                   )),
                   uiOutput("AddColMAFUI"),
                   easyClose = FALSE,
                   footer = tagList(
                     modalButton("Cancel"),
                     actionButton("ModalbuttonAddColMAF", "Add column(s)")
                   )
                 )
               )
             },
             ignoreInit = TRUE
)
# output UI to select column that should be deleted
output$AddColMAFUI <- renderUI({
  if (input$AddColMAFMode == 1) {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = "SelColnameMAF",
        label = "Select pre-defined column(s). (Some of them are entity specific)",
        choices = predefined_MAF_columns,
        multiple = TRUE
      )
    ))
  } else if (input$AddColMAFMode == 2) {
    fluidRow(column(
      width = 8,
      textInput(
        inputId = "colnameMAF",
        label = "Column name:",
        placeholder = "e.g. ATTRIBUTE"
      )))
  }
})

observeEvent(input$ModalbuttonAddColMAF, {
  if (input$AddColMAFMode == 1) {
    if (is.null(input$SelColnameMAF)) {
      showNotification("Please select a column.",
                       type = "error",
                       duration = NULL
      )
    } else {
      # prevent overwriting existing columns
      colsToAdd <-
        input$SelColnameMAF[!input$SelColnameMAF %in% names(loadedData$data_mutations_extended)]

      loadedData$data_mutations_extended[colsToAdd] <- list("")
      
      # change tracker
      study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
      
      removeModal()
    }
  } else if (input$AddColMAFMode == 2) {
    if (input$colnameMAF == "") {
      showNotification("Column name cannot be empty.",
                       type = "error",
                       duration = NULL
      )
    } else if (input$colnameMAF %in% names(loadedData$data_mutations_extended)) {
      showNotification("Column name already exists.",
                       type = "error",
                       duration = NULL
      )
    } else {
      loadedData$data_mutations_extended %<>% mutate(!!(input$colnameMAF) := "")
      
      # change tracker
      study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
      
      removeModal()
    }
  }
})

# delete column ---------------------------------------------------------------
observeEvent(input$DeleteColumnMAFentry, {
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
      title = "Delete column(s)",
      fluidRow(column(
        width = 8,
        selectInput(
          inputId = "SelColnameMAF",
          label = "Select column(s) for deletion:",
          choices = setdiff(
            colnames(loadedData$data_mutations_extended),
            c("Tumor_Sample_Barcode", "Hugo_Symbol", "Variant_Classification", "HGVSp_Short")
          ),
          multiple = TRUE
        )
      )),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalbuttonDeleteColMAF", "Delete column(s)")
      )
    )
  )
})
observeEvent(input$ModalbuttonDeleteColMAF, {
  loadedData$data_mutations_extended <-
    loadedData$data_mutations_extended[, !(names(loadedData$data_mutations_extended) %in% input$SelColnameMAF), drop = FALSE]
  
  # change tracker
  study_tracker$df[3, "Saved"] <- as.character(icon("exclamation-circle"))
  
  removeModal()
})

# save data ---------------------------------------------------------------
observeEvent(input$saveMAF, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please load a study in the 'Study' tab first.",
      type = "error",
      duration = NULL
    )
  }
  req(loadedData$studyID, loadedData$data_mutations_extended, loadedData$data_mutations_filename)
  write.table(
    loadedData$data_mutations_extended,
    file.path(study_dir, loadedData$studyID, paste0(loadedData$data_mutations_filename, ".temp")),
    append = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
  file.rename(
    file.path(study_dir, loadedData$studyID, paste0(loadedData$data_mutations_filename, ".temp")),
    file.path(study_dir, loadedData$studyID, loadedData$data_mutations_filename)
  )

  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(loadedData$studyID, loadedData$data_mutations_filename)
    )
  }

  # add cases_sequenced
  case_list_dir <-
    file.path(study_dir, loadedData$studyID, "case_lists")
  if (!dir.exists(case_list_dir)) dir.create(case_list_dir)
  cases_sequenced_df <-
    data.frame(
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
        paste0(loadedData$studyID, "_sequenced"),
        "all_cases_with_mutation_data",
        "Sequenced Tumors",
        paste0(
          "All sequenced samples (",
          length(unique(loadedData$data_mutations_extended$Tumor_Sample_Barcode)),
          " samples)"
        ),
        paste(unique(loadedData$data_mutations_extended$Tumor_Sample_Barcode), collapse = "\t")
      )
    )
  write.table(
    cases_sequenced_df,
    file.path(case_list_dir, "cases_sequenced.txt.temp"),
    append = FALSE,
    sep = ": ",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  file.rename(
    file.path(case_list_dir, "cases_sequenced.txt.temp"),
    file.path(case_list_dir, "cases_sequenced.txt")
  )

  # meta_mutations_extended
  if (!file.exists(file.path(study_dir, loadedData$studyID, "meta_mutations_extended.txt"))) {
    meta_mutations_extended_df <-
      data.frame(
        V1 = c(
          "cancer_study_identifier",
          "stable_id",
          "profile_name",
          "profile_description",
          "genetic_alteration_type",
          "datatype",
          "show_profile_in_analysis_tab",
          "data_filename"
        ),
        V2 = c(
          loadedData$studyID,
          "mutations",
          "Mutations",
          "Extended MAF",
          "MUTATION_EXTENDED",
          "MAF",
          "true",
          loadedData$data_mutations_filename
        )
      )
    write.table(
      meta_mutations_extended_df,
      file.path(study_dir, loadedData$studyID, "meta_mutations_extended.txt.temp"),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir, loadedData$studyID, "meta_mutations_extended.txt.temp"),
      file.path(study_dir, loadedData$studyID, "meta_mutations_extended.txt")
    )
  }
  
  # change tracker
  study_tracker$df[3, "Saved"] <- as.character(icon("check-circle"))

  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(loadedData$studyID, "meta_mutations_extended.txt")
    )
  }

  showNotification("Mutation data saved successfully!",
    type = "message",
    duration = 10
  )
})
