output$MutDataImg <- renderImage({
  return(list(src = system.file("www", "maf-data.PNG", package = "cbpManager"),contentType = "image/png",alt = "annotation-example", width="auto"))
}, deleteFile = FALSE)

observeEvent(input$chooseMAF, {
  uploaded_data <- as.data.frame(vroom::vroom(input$chooseMAF$datapath, delim = "\t"))
  requiredCols <- c("Hugo_Symbol", "Tumor_Sample_Barcode", "Variant_Classification", "HGVSp_Short")
  if(any(!requiredCols %in% colnames(uploaded_data))){
    showNotification("One or more of the required columns are missing.", type="error", duration = 10)
  } else {
    loadedData$data_mutations_extended <- dplyr::bind_rows(uploaded_data, loadedData$data_mutations_extended)
  }
})

output$MAFdata <- DT::renderDT({
  loadedData$data_mutations_extended
})

observeEvent(input$saveMAF, {
  if (!is.null(input$chooseMAF)){
    # data_mutations_extended
    write.table(loadedData$data_mutations_extended, file.path(study_dir, input$cancer_study_identifier, "data_mutations_extended.txt.temp"), append = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    file.rename(file.path(study_dir, input$cancer_study_identifier, "data_mutations_extended.txt.temp"), file.path(study_dir, input$cancer_study_identifier, "data_mutations_extended.txt"))

    # add cases_sequenced
    case_list_dir <- file.path(study_dir, input$cancer_study_identifier, "case_lists")
    ifelse(!dir.exists(case_list_dir), dir.create(case_list_dir), FALSE)
    cases_samples <- loadedData$data_clinical_sample[3:nrow(loadedData$data_clinical_sample), "SAMPLE_ID"]
    cases_sequenced_df <- data.frame(V1 = c("cancer_study_identifier",
                                            "stable_id",
                                            "case_list_category",
                                            "case_list_name",
                                            "case_list_description",
                                            "case_list_ids"),
                                     V2 = c(input$cancer_study_identifier,
                                            paste0(input$cancer_study_identifier, "_sequenced"),
                                            "all_cases_with_mutation_data",
                                            "Sequenced Tumors",
                                            paste0("All sequenced samples (", nrow(loadedData$data_clinical_sample)-2, " samples)"),
                                            paste(cases_samples, collapse = "\t"))
    )
    write.table(cases_sequenced_df, file.path(case_list_dir, "cases_sequenced.txt.temp"), append = FALSE, sep = ": ",
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    file.rename(file.path(case_list_dir, "cases_sequenced.txt.temp"), file.path(case_list_dir, "cases_sequenced.txt"))

    # meta_mutations_extended
    meta_mutations_extended_df <- data.frame(V1 = c("cancer_study_identifier", "stable_id", "profile_name", "profile_description", "genetic_alteration_type", "datatype", "show_profile_in_analysis_tab", "data_filename"),
                                             V2 = c(input$cancer_study_identifier, "mutations", "Mutations", "Extended MAF", "MUTATION_EXTENDED", "MAF", "true", "data_mutations_extended.txt")
    )
    write.table(meta_mutations_extended_df, file.path(study_dir, input$cancer_study_identifier, "meta_mutations_extended.txt.temp"), append = FALSE, sep = ": ", row.names = FALSE, col.names = FALSE, quote = FALSE)
    file.rename(file.path(study_dir, input$cancer_study_identifier, "meta_mutations_extended.txt.temp"), file.path(study_dir, input$cancer_study_identifier, "meta_mutations_extended.txt"))
    showNotification("MAF file submitted successfully!", type="message", duration = 10)
  } else {
    showNotification("Please select a valid MAF file.", type="warning", duration = NULL)
  }
})
