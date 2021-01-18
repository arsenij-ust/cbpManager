# image ---------------------------------------------------------------
output$MutDataImg <- renderImage({
  return(
    list(
      src = system.file("www", "maf-data.PNG", package = "cbpManager"),
      contentType = "image/png",
      alt = "annotation-example",
      width = "auto"
    )
  )
}, deleteFile = FALSE)

# upload file ---------------------------------------------------------------
observeEvent(input$chooseMAF, {
  uploaded_data <-
    as.data.frame(vroom::vroom(input$chooseMAF$datapath, delim = "\t"))
  requiredCols <-
    c("Hugo_Symbol",
      "Tumor_Sample_Barcode",
      "Variant_Classification",
      "HGVSp_Short")
  if (any(!requiredCols %in% colnames(uploaded_data))) {
    showNotification(
      "One or more of the required columns are missing.",
      type = "error",
      duration = 10
    )
  } else {
    loadedData$data_mutations_extended <-
      dplyr::bind_rows(uploaded_data, loadedData$data_mutations_extended)
  }
})

# show table ---------------------------------------------------------------
output$MAFdata <- DT::renderDT({
  DT::datatable(loadedData$data_mutations_extended,
                options = list(scrollX = TRUE))
})

# save data ---------------------------------------------------------------
observeEvent(input$saveMAF, {
  #if (!is.null(input$chooseMAF)) {
    # data_mutations_extended
    write.table(
      loadedData$data_mutations_extended,
      file.path(study_dir,loadedData$studyID,paste0(loadedData$data_mutations_filename,".temp")),
      append = FALSE,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir,loadedData$studyID,paste0(loadedData$data_mutations_filename,".temp")),
      file.path(study_dir,loadedData$studyID,loadedData$data_mutations_filename)
    )

    # logging
    if(!is.null(logDir)){
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, loadedData$data_mutations_filename)
      )
    }

    # add cases_sequenced
    case_list_dir <-
      file.path(study_dir, loadedData$studyID, "case_lists")
    if(!dir.exists(case_list_dir)) dir.create(case_list_dir)
    cases_samples <-
      loadedData$data_clinical_sample[3:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
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
            nrow(loadedData$data_clinical_sample) - 2,
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
        file.path(study_dir,loadedData$studyID,"meta_mutations_extended.txt.temp"),
        append = FALSE,
        sep = ": ",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
      )
      file.rename(
        file.path(study_dir,loadedData$studyID,"meta_mutations_extended.txt.temp"),
        file.path(study_dir,loadedData$studyID,"meta_mutations_extended.txt")
      )
    }

    # logging
    if(!is.null(logDir)){
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(loadedData$studyID, "meta_mutations_extended.txt")
      )
    }

    showNotification("MAF file submitted successfully!",
                     type = "message",
                     duration = 10)
  # } else {
  #   showNotification("Please select a valid MAF file.",
  #                    type = "warning",
  #                    duration = NULL)
  # }
})
