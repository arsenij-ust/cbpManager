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
      "Hugo_Symbol" || "Entrez_Gene_Id"
    if (any(!requiredCols %in% colnames(uploaded_data))) {
      showNotification(
        "One or more of the required columns are missing.",
        type = "error",
        duration = NULL
      )
    } else {
      loadedData$data_cna_extended <-
        dplyr::bind_rows(uploaded_data, loadedData$data_cna_extended)
    }
  }
  
})

# show table ---------------------------------------------------------------
output$CNAdata <- DT::renderDT({
  DT::datatable(loadedData$data_cna_extended,
                options = list(scrollX = TRUE)
  )
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
  req(loadedData$studyID, loadedData$data_cna_extended, loadedData$data_cna_filename)
  write.table(
    loadedData$data_cna_extended,
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
  
  #???????????????
  
  # add cases_sequenced
  case_list_dir <-
    file.path(study_dir, loadedData$studyID, "case_lists")
  if (!dir.exists(case_list_dir)) dir.create(case_list_dir)
  cases_samples <-
    loadedData$data_clinical_sample[4:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
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
          nrow(loadedData$data_clinical_sample) - 3,
          " samples)"
        ),
        paste(cases_samples, collapse = "\t")
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
  
  # cna_mutations_extended
  if (!file.exists(file.path(study_dir, loadedData$studyID, "meta_cna_extended.txt"))) {
    meta_cna_extended_df <-
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
          loadedData$data_cna_filename
        )
      )
    write.table(
      meta_cna_extended_df,
      file.path(study_dir, loadedData$studyID, "meta_cna_extended.txt.temp"),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir, loadedData$studyID, "meta_cna_extended.txt.temp"),
      file.path(study_dir, loadedData$studyID, "meta_cna_extended.txt")
    )
  }
  
  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(loadedData$studyID, "meta_cna_extended.txt")
    )
  }
  
  showNotification("CNA file submitted successfully!",
                   type = "message",
                   duration = 10
  )
})