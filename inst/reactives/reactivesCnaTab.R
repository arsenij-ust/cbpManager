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
  DT::datatable(loadedData$data_cna,
                options = list(scrollX = TRUE)
  )
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
    req(input$cna_profile_name, input$cna_profile_description)
    loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_name"),]$value <- input$cna_profile_name
    loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_description"),]$value <- input$cna_profile_description
    
    # create meta_cna data.frame
    meta_cna_df <- data.frame(
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
        "gistic",
        "true",
        loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_name"),]$value, 
        loadedData$meta_cna[which(loadedData$meta_cna$attribute=="profile_description"),]$value,
        "data_CNA.txt"
      )
    )

    # write meta_CNA.txt
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

# upload file ---------------------------------------------------------------
observeEvent(input$chooseCNA, {
  if(!grepl("\\.[txt|tsv]", input$chooseCNA$name)){
    showNotification(
      "The file format is not supported. 
      File should be '.txt', '.tsv'.",
      type = "error",
      duration = NULL
    )
  } else {
    uploaded_data <-
      as.data.frame(vroom::vroom(input$chooseCNA$datapath, delim = "\t"))
    requiredCols <-
      c(
        "Gene Symbol",
        "Locus ID"
        )
    if (any(!requiredCols %in% colnames(uploaded_data))) {
      showNotification(
        "One or more of the required columns are missing.",
        type = "error",
        duration = NULL
      )
    } else {
        colnames(uploaded_data)[which(names(uploaded_data) == "Gene Symbol")] <- "Hugo_Symbol"
        colnames(uploaded_data)[which(names(uploaded_data) == "Locus ID")] <- "Entrez_Gene_Id"
        uploaded_data$Cytoband <- NULL
        
        cases_samples <-
          loadedData$data_clinical_sample[4:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
        
        if (any(!colnames(uploaded_data)[,3:ncol(uploaded_data)] %in% cases_samples)) {
          showNotification(
            "Please enter all sample IDs on the study tab before proceeding.",
            type = "error",
            duration = NULL
          )
        } else {
          
          if (uploaded_data[uploaded_data$Entrez_Gene_Id < 0, ]){
            
            dataModal <- function(failed = FALSE) {
              modalDialog(
                title = "Warning",
                "Negative values in the Locus ID/Entrez_Gene_Id column will prevent the data from being 
                uploaded to cBioPortal. Choose one of the following options:",
                radioButtons("options", label = "Choose an option to continue:", choices = list(
                  "Delete rows with negative values and merge data (recommended)" = 1, 
                  "Keep all data and merge despite negative values" = 2,
                  "Cancel file upload" = 3), selected = 1),
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok", "OK")
                )
              )
            }
            
            showModal(dataModal())
            
            observeEvent(input$ok, {
              if (input$options == "1") {
                deleted_rows <-
                  rownames(uploaded_data[uploaded_data$Entrez_Gene_Id < 0,])
                uploaded_data[-c(deleted_rows),]
                loadedData$data_cna <-
                  dplyr::bind_rows(uploaded_data, loadedData$data_cna)
              } else if (input$options == "2") {
                loadedData$data_cna <-
                  dplyr::bind_rows(uploaded_data, loadedData$data_cna)
              } else if (input$options == "3") {
                showNotification(
                  "The upload of a copy number data file was canceled.",
                  type = "message",
                  duration = NULL
                )
                break
              }
            })
            
          } else {
            loadedData$data_cna <-
              dplyr::bind_rows(uploaded_data, loadedData$data_cna)
          }
          
        }
    }
  }
})

# save data ---------------------------------------------------------------
observeEvent(input$saveCNA, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please select and load a study in the 'Study' tab.",
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
  if (!file.exists(file.path(study_dir, loadedData$studyID, "meta_CNA.txt"))) {
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
          "gistic",
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
  
  showNotification("CNA file submitted successfully!",
                   type = "message",
                   duration = 10
  )
})