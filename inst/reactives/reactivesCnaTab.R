# image ---------------------------------------------------------------
output$CopyNumberDataImg <- renderImage(
  {
    return(
      list(
        src = system.file("www", "cna-data.PNG", package = "cbpManager"),
        contentType = "image/png",
        alt = "annotation-example",
        width = "auto"
      )
    )
  },
  deleteFile = FALSE
)

# tour  ---------------------------------------------------------------
observeEvent(input$tour_cna, {
  tour <- read.delim(system.file("apphelp", "tour_cna.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# show table ---------------------------------------------------------------
output$CNAdata <- DT::renderDT({
  if (!is.null(loadedData$studyID)) {
    DT::datatable(loadedData$data_cna,
                  options = list(scrollX = TRUE))
  }
})

# profile_name and profile_description -----------------------------------------------------------
observeEvent(input$saveMetadata, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please select and load a study in the 'Study' tab.",
      type = "error",
      duration = NULL
    )
  } else {
    if (!rapportools::is.empty(input$cna_profile_name)) {
      loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_name"),]$value <- input$cna_profile_name
    }
    if (!rapportools::is.empty(input$cna_profile_description)) {
      loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_description"),]$value <- input$cna_profile_description
    }
    
    # create meta_cna data.frame
    #meta_cna_df <- loadedData$meta_cna

    # write meta_CNA.txt
    write.table(
      loadedData$meta_cna,
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt.temp"),
      append = FALSE, 
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt.temp"),
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt")
    )
    
    # logging
    if (!is.null(logDir)) {
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, "meta_CNA.txt")
      )
    }
    
    showNotification(paste0("File meta_CNA.txt of study ", loadedData$studyID, " updated successfully!"),
                     type = "message",
                     duration = 10
    )
    removeModal()
  }
})

output$curr_profile_name <- renderText({
  req(loadedData$meta_cna)
  prof_name <- loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_name"),]$value
  if(!is.na(prof_name)){
    prof_name
  } else {
    "No profile name."
  }
})

output$curr_profile_description <- renderText({
  req(loadedData$meta_cna)
  prof_desc <- loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_description"),]$value
  if(!is.na(prof_desc)){
    prof_desc
  } else {
    "No profile description."
  }
})

# upload file ---------------------------------------------------------------------------------

# ui of modal dialog in case of negative values in Entrez_Gene_Id column
dataModal <- function(failed = FALSE) {
  modalDialog(
    title = "Warning",
    "Negative values in the Locus ID/Entrez_Gene_Id column will prevent the data from being
                uploaded to cBioPortal. Choose one of the following options.",
    radioButtons(
      "options",
      label = "Choose an option to continue:",
      choices = list(
        "Delete rows with negative values and merge data (recommended)" = 1,
        "Keep all data and merge despite negative values" = 2,
        "Cancel file upload" = 3
      ),
      selected = 1
    ),
    
    footer = tagList(actionButton("ok", "OK")
    )
  )
}

# initiate reactive objects
uploaded_data <- reactiveValues(df = NULL)
validated_data <- reactiveValues(df = NULL)

# management of file upload
observeEvent(input$chooseCNA, {
  
  output$CNAdata <- DT::renderDT({
    if (!is.null(loadedData$studyID)) {
      DT::datatable(loadedData$data_cna,
                    options = list(scrollX = TRUE))
    }
  })
  
  # check if a study is loaded
  loaded <- NULL
  if (is.null(loadedData$studyID)) {
    loaded = FALSE
    showNotification(
      "Please select and load a study in the 'Study' tab.",
      type = "error",
      duration = NULL
    )
    return(NULL)
  } 
  # else {
  #   loaded = TRUE
  # }
  
  # if a study is loaded, access respective samples and cna data
  if (loaded == TRUE) {
    uploaded_data$df <- loadedData$data_cna
    
    cases_samples <-
      loadedData$data_clinical_sample[4:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
  }
  
  # check file format before upload
  if (!grepl("\\.[txt|tsv]", input$chooseCNA$name)) {
    showNotification(
      "The file format is not supported.
      File should be '.txt', '.tsv'.",
      type = "error",
      duration = NULL
    )
  } else {
    uploaded_data$df <-
      as.data.frame(vroom::vroom(input$chooseCNA$datapath, delim = "\t"))
    
    if ("Hugo_Symbol" %in% colnames(uploaded_data$df)) {
      requiredCols <- "Hugo_Symbol" # upload of any .txt file
    } else {
      requiredCols <- "Gene Symbol" # upload of GISTIC output
    } 
    
    # check presence of required columns before renaming/deleting columns
    if (any(!requiredCols %in% colnames(uploaded_data$df))) {
      showNotification(
        "One or more of the required columns are missing.",
        type = "error",
        duration = NULL
      )
    } else {
      # adjustments when GISTIC output is uploaded
      colnames(uploaded_data$df)[which(names(uploaded_data$df) == "Gene Symbol")] <-
        "Hugo_Symbol"
      colnames(uploaded_data$df)[which(names(uploaded_data$df) == "Locus ID")] <-
        "Entrez_Gene_Id"
      uploaded_data$df$Cytoband <- NULL
    }
  }
  
  # check if all sample IDs have been added to the loaded study 
  added <- NULL
  if (loaded == TRUE) {
    if (any(!colnames(uploaded_data$df)[3:ncol(uploaded_data$df)] %in% cases_samples)) {
      added = FALSE
      showNotification(
        "Please enter all sample IDs on the study tab before proceeding.",
        type = "error",
        duration = NULL
      )
    } else {
      added = TRUE
    }
  }
  
  if (loaded == TRUE && added == TRUE) {
    validated_data$df <- loadedData$data_cna
    
    # if study is loaded and all samples are added, show data of uploaded file in table
    output$CNAdata <- DT::renderDT({
      DT::datatable(uploaded_data$df,
                    options = list(scrollX = TRUE))
    })
  }
  
    # check if there is a negative value in the column "Entrez_Gene_Id"        
    neg <- NULL
    if ("Entrez_Gene_Id" %in% colnames(uploaded_data$df)) {
      for (i in 2:nrow(uploaded_data$df)) {
        if (uploaded_data$df$Entrez_Gene_Id[i] < 0) {
          neg = TRUE
          break
        } else {
          neg = FALSE
        }
      }
    } else {
      neg = FALSE
    }
  
  # in case of a negative value show a modal dialog with three options
  if (loaded == TRUE && added == TRUE && neg == TRUE) {
    showModal(dataModal())
  } else if (loaded == TRUE && added == TRUE && neg == FALSE) {
    uploaded_data$df <- merge(uploaded_data$df, loadedData$data_cna, all = TRUE)
    validated_data$df <- uploaded_data$df
    
    # reset
    loaded <- NULL
    added <- NULL
    neg <- NULL
  }
})

# completion of the file upload depending on the option selected
observeEvent(input$ok, {
  if (input$options == "1") {
    uploaded_data$df <- uploaded_data$df[uploaded_data$df$Entrez_Gene_Id > -1,]
    neg = FALSE
    removeModal()
  } else if (input$options == "2") {
    neg = FALSE
    removeModal()
  } else if (input$options == "3") {
    showNotification(
      "The upload of a copy number data file was canceled.",
      type = "message",
      duration = NULL
    )
    output$CNAdata <- DT::renderDT({
        DT::datatable(loadedData$data_cna,
                      options = list(scrollX = TRUE))
    })
    validated_data$df <- NULL
    neg = TRUE
    removeModal()
  }
  
  if (neg == FALSE) {
    uploaded_data$df <- merge(uploaded_data$df, loadedData$data_cna, all = TRUE)
    validated_data$df <- uploaded_data$df
    
    # reset 
    loaded <- NULL
    added <- NULL
    neg <- NULL
  }
})

# save data ---------------------------------------------------------------
observeEvent(input$saveCNA, {
  if(is.null(loadedData$studyID)) {
    showNotification(
      "Please select and load a study in the 'Study' tab.",
      type = "error",
      duration = NULL
    )
  }
  
  if (!is.null(validated_data$df)) {
    loadedData$data_cna <- validated_data$df
    showNotification("CNA file submitted successfully!",
                     type = "message",
                     duration = 10
    )
    output$CNAdata <- DT::renderDT({
      if (!is.null(loadedData$studyID)) {
        DT::datatable(loadedData$data_cna,
                      options = list(scrollX = TRUE))
      }
    })
    # reset
    validated_data$df <- NULL
  } else {
    showNotification(
      "Please upload a file.",
      type = "error",
      duration = NULL
    )
  }
  
  req(loadedData$studyID, loadedData$data_cna, loadedData$data_cna_filename)
  write.table(
    loadedData$data_cna,
    file.path(study_dir, loadedData$studyID, paste0(loadedData$data_cna_filename, ".temp")),
    append = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
  file.rename(
    file.path(study_dir, loadedData$studyID, paste0(loadedData$data_cna_filename, ".temp")),
    file.path(study_dir, loadedData$studyID, loadedData$data_cna_filename)
  )
  
  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(loadedData$studyID, loadedData$data_cna_filename)
    )
  }
  
  # add cases_cna
  case_list_dir <-
    file.path(study_dir, loadedData$studyID, "case_lists")
  if (!dir.exists(case_list_dir)) dir.create(case_list_dir)
  cases_samples <-
    loadedData$data_clinical_sample[4:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
  cases_cna_df <-
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
        paste0(loadedData$studyID, "_cna"),
        "all_cases_with_cna_data",
        "Samples with CNA data",
        paste0(
          "All samples with CNA data (",
          nrow(loadedData$data_clinical_sample) - 3,
          " samples)"
        ),
        paste(cases_samples, collapse = "\t")
      )
    )
  write.table(
    cases_cna_df,
    file.path(case_list_dir, "cases_cna.txt.temp"),
    append = FALSE,
    sep = ": ",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  file.rename(
    file.path(case_list_dir, "cases_cna.txt.temp"),
    file.path(case_list_dir, "cases_cna.txt")
  )
  
  # meta_CNA
  if (file.path(study_dir, loadedData$studyID, "meta_CNA.txt")) {
    meta_cna_df <-
      data.frame(
        V1 = c(
          "cancer_study_identifier",
          "genetic_alteration_type",
          "datatype",
          "stable_id",
          "show_profile_in_analysis_tab",
          "profile_name",
          "profile_description",
          "data_filename"
        ),
        V2 = c(
          loadedData$studyID,
          "COPY_NUMBER_ALTERATION",
          "DISCRETE",
          paste0(loadedData$studyID, "_cna"),
          "true",
          loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_name"),]$value, 
          loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_description"),]$value,
          loadedData$data_cna_filename
        )
      )
    write.table(
      meta_cna_df,
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt.temp"),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt.temp"),
      file.path(study_dir, loadedData$studyID, "meta_CNA.txt")
    )
  }
  
  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(loadedData$studyID, "meta_CNA.txt")
    )
  }
})