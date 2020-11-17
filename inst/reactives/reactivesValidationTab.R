# validationTags <- eventReactive(input$runValidation, {
#   req(input$cancer_study_identifier)
#   tags <- list()
#
#   studyFiles <- list.files(file.path(study_dir, input$cancer_study_identifier))
#   studyFiles <- c(studyFiles, list.files(file.path(study_dir, input$cancer_study_identifier, "case_lists")))
#   requiredFiles <- c(
#     "data_clinical_sample.txt",
#     "data_clinical_patient.txt",
#     "data_mutations_extended.txt",
#     "meta_study.txt",
#     "meta_clinical_sample.txt",
#     "meta_clinical_patient.txt",
#     "meta_mutations_extended.txt",
#     "cases_all.txt"
#   )
#   tags <- rlist::list.append(tags, hr())
#   tags <- rlist::list.append(tags, h4("Required files:"))
#   for (f in requiredFiles){
#     if(f %in% studyFiles){
#       tags <- rlist::list.append(tags, p(paste0(f," exists"), class = "valid"))
#     } else {
#       tags <- rlist::list.append(tags, p(paste0(f," not exists"), class = "invalid"))
#     }
#   }
#   tags <- rlist::list.append(tags, h4("Optional files:"))
#
#   return(tags)
# })

# output$validateUI <- renderUI({
#   tagList(
#     validationTags()
#   )
# })

# getPage <- eventReactive(input$runValidation, {
#   req(input$cancer_study_identifier)
#
#   reticulate::use_condaenv("r-reticulate", required = TRUE)
#   py_modules <- c("requests", "PyYAML", "jinja2", "importer")
#   for (module in py_modules){
#     if(reticulate::py_module_available(module)){
#       reticulate::py_install(module, pip = TRUE)
#     }
#   }
#   validationFile <- paste0(input$cancer_study_identifier,"_validation.html")
#   validationPath <-file.path(study_dir, input$cancer_study_identifier, validationFile)
#
#   source_python(system.file("python", "validateDataWrapper.py", package = "cbpManager"))
#   executeScript(file.path(study_dir, input$cancer_study_identifier), validationPath)
#
#
#
#   ######
#
#   reticulate::use_condaenv("r-reticulate", required = TRUE)
#   source_python("inst/python/validateDataWrapper.py")
#   executeScript("test", "hallo")
#
#   source_python("inst/python/importer/validateData.py")
  #error_file=None, html_table='validation_testpatient.html', max_reported_values=3, no_portal_checks=True, portal_info_dir=None, relaxed_clinical_definitions=False, strict_maf_checks=False, study_directory='./testpatient/', url_server='http://localhost:8080', verbose=True





  ##########

  #reticulate::py_run_file(system.file("python", "importer", "validateData.py", package = "cbpManager"))

#   return(includeHTML(validationPath))
# })

output$validation<-renderUI({getPage()})
output$downloadValidation <- downloadHandler(
  filename <- function() {
    paste0(loadedData$studyID,"_validation.html")
  },

  content <- function(file) {
    validationFile <- paste0(loadedData$studyID,"_validation.html")
    validationPath <-file.path(study_dir, loadedData$studyID, validationFile)
    file.copy(validationPath, file)
  },
  contentType = NULL
)
