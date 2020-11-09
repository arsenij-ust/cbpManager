# UI of cancer type
output$ui_type_of_cancer <- renderUI({
  selectInput("type_of_cancer",
              label = "Select the cancer type",
              choices = c("mixed", oncotree$code)
  )
})

# show table of oncotree
output$oncotree_table <- DT::renderDT({
  columns_to_show <- c("code", "name", "mainType", "tissue")
  DT::datatable(oncotree[,columns_to_show], selection = 'single', rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE))
})

observe(
  if(!is.null(input$oncotree_table_row_last_clicked)){
    updateSelectInput(session, "type_of_cancer",
                      choices = c("mixed", oncotree$code),
                      selected = oncotree[input$oncotree_table_row_last_clicked,"code"])
  }
)

# load metadata of a selected study
loadedData <- reactiveValues()
loadedData$data_clinical_patient <- NULL

observeEvent(input$upload, {
  if(input$cancer_study_identifier == ""){
    showNotification("Please select a cancer study ID", type="warning", duration = NULL)
    return()
  }
  # read meta_study ####
  meta_study_path <- file.path(study_dir, input$cancer_study_identifier, "meta_study.txt")
  if (file.exists(meta_study_path)){
    meta_study <- read.table(meta_study_path, sep=":")
    loadedData$meta_study <- meta_study
  }

  # read data_clinical_patient ####
  data_clinical_patient_path <- file.path(study_dir, input$cancer_study_identifier, "data_clinical_patient.txt")
  if (file.exists(data_clinical_patient_path)){
    data_clinical_patient <- read.table(data_clinical_patient_path, sep="\t", colClasses = "character", comment.char = "")
    data_clinical_patient$V1 <- sub(pattern="^#", replacement="", x=data_clinical_patient$V1)
    colnames(data_clinical_patient) <- data_clinical_patient[5,]
    data_clinical_patient <- data_clinical_patient[-c(3,4,5),]
  } else {
    data_clinical_patient <- data.frame(PATIENT_ID = c("Patient Identifier","Patient identifier", "e.g. JohnDoe"),
                                        ATTRIBUTE = c("Name of attr.", "Longer name of attr.", "Value of attr."),
                                        stringsAsFactors = FALSE)
  }
  loadedData$data_clinical_patient <- data_clinical_patient

  updateSelectInput(session, "patientSelDelCol",
                    choices = setdiff(colnames(loadedData$data_clinical_patient), c("PATIENT_ID"))
  )

  # read data_clinical_sample ####
  data_clinical_sample_path <- file.path(study_dir, input$cancer_study_identifier, "data_clinical_sample.txt")
  if (file.exists(data_clinical_sample_path)){
    data_clinical_sample <- read.table(data_clinical_sample_path, sep="\t", colClasses = "character", comment.char = "")
    data_clinical_sample$V1 <- sub(pattern="^#", replacement="", x=data_clinical_sample$V1)
    colnames(data_clinical_sample) <- data_clinical_sample[5,]
    data_clinical_sample <- data_clinical_sample[-c(3,4,5),]
  } else {
    data_clinical_sample <- data.frame(PATIENT_ID = c("Patient Identifier","Patient identifier", "e.g. JohnDoe"),
                                       SAMPLE_ID = c("Sample Identifier","Sample identifier", "e.g. JohnDoe_1"),
                                       ATTRIBUTE = c("Name of attr.", "Longer name of attr.", "Value of attr."),
                                       stringsAsFactors = FALSE)
  }
  loadedData$data_clinical_sample <- data_clinical_sample

  # read data_mutations_extended ####
  data_mutations_extended_path <- file.path(study_dir, input$cancer_study_identifier, "data_mutations_extended.txt")
  if (file.exists(data_mutations_extended_path)){
    data_mutations_extended <- read.table(data_mutations_extended_path, sep="\t", header = TRUE)
  } else {
    data_mutations_extended <- data.frame(Hugo_Symbol=character(),
                                          Tumor_Sample_Barcode=character(),
                                          Variant_Classification=character(),
                                          HGVSp_Short=character())
  }
  loadedData$data_mutations_extended <- data_mutations_extended

  # read data_timeline_treatment ####
  data_timeline_treatment_path <- file.path(study_dir, input$cancer_study_identifier, "data_timeline_treatment.txt")
  if (file.exists(data_timeline_treatment_path)){
    data_timeline_treatment <- read.table(data_timeline_treatment_path, sep="\t", header = TRUE)
  } else {
    data_timeline_treatment <- data.frame(PATIENT_ID = character(),
                                          START_DATE = numeric(),
                                          STOP_DATE = numeric(),
                                          EVENT_TYPE = factor(levels=c("TREATMENT")),
                                          TREATMENT_TYPE = factor(levels=c("Medical Therapy", "Radiation Therapy")),
                                          SUBTYPE = character(),
                                          AGENT = character(),
                                          stringsAsFactors = FALSE)
  }
  loadedData$data_timeline_treatment <- data_timeline_treatment

  # read data_timeline_status ####
  data_timeline_status_path <- file.path(study_dir, input$cancer_study_identifier, "data_timeline_status.txt")
  if (file.exists(data_timeline_status_path)){
    data_timeline_status <- read.table(data_timeline_status_path, sep="\t", header = TRUE)
    if(any(!"SOURCE" %in% colnames(data_timeline_status))) data_timeline_status$SOURCE <- ""
  } else {
    data_timeline_status <- data.frame(PATIENT_ID = character(),
                                       START_DATE = numeric(),
                                       STOP_DATE = numeric(),
                                       EVENT_TYPE = factor(levels=c("STATUS")),
                                       STATUS = character(),
                                       SOURCE = character(),
                                       stringsAsFactors = FALSE)
  }
  loadedData$data_timeline_status <- data_timeline_status

  # read data_timeline_surgery ####
  data_timeline_surgery_path <- file.path(study_dir, input$cancer_study_identifier, "data_timeline_surgery.txt")
  if (file.exists(data_timeline_surgery_path)){
    data_timeline_surgery <- read.table(data_timeline_surgery_path, sep="\t", header = TRUE)
  } else {
    data_timeline_surgery <- data.frame(PATIENT_ID = character(),
                                        START_DATE = numeric(),
                                        STOP_DATE = numeric(),
                                        EVENT_TYPE = factor(levels=c("SURGERY")),
                                        EVENT_TYPE_DETAILED = character(),
                                        stringsAsFactors = FALSE)
  }
  loadedData$data_timeline_surgery <- data_timeline_surgery
  updateSelectInput(session, "sampleSelDelCol",
                    choices = setdiff(colnames(loadedData$data_clinical_sample), c("PATIENT_ID", "SAMPLE_ID"))
  )

  # read diagnosis_dates
  dates_first_diagnosis_path <- file.path(study_dir, input$cancer_study_identifier, "dates_first_diagnosis.txt")
  # reactive values to save one df with Patinet_IDs and date picker code and another with Patinet_IDs and user-defined dates
  if (file.exists(dates_first_diagnosis_path)){
    dates_first_diagnosis <- read.table(dates_first_diagnosis_path, sep="\t", header = TRUE)
  } else {
    dates_first_diagnosis <- data.frame(PATIENT_ID=character(), DATE=character())
  }
  loadedData$dates_first_diagnosis <- dates_first_diagnosis
  #loadedData$dates_first_diagnosis_html <- data.frame(PATIENT_ID=character(), DATE=character())
  # for (patID in loadedData$dates_first_diagnosis$PATIENT_ID){
  #   date <- loadedData$dates_first_diagnosis[[patID, "DATE"]]
  #   loadedData$dates_first_diagnosis_html[patID,] <- c(patID, paste0('<input id="calendar_',patID,'" type="text" class="form-control" value="',date,'" onchange="trackLastChange(this.id);" onkeyup="this.onchange();" onpaste="this.onchange();" oninput="this.onchange();"/>'))
  # }
  # ####
  showNotification("Study uploaded successfully!", type="message", duration = 10)
})
# show table of metadata if study loaded
output$studyTable <- DT::renderDT({
  if(!is.null(loadedData$meta_study)){
    DT::datatable(loadedData$meta_study, colnames = rep("", ncol(loadedData$meta_study)))
  }
})

# add study and generate meta_data files
cancer_study_rc <- reactive({
  list.files(study_dir)
})

output$ui_cancer_study_identifier <- renderUI({
  selectInput(
    "cancer_study_identifier",
    label = "Select ID of cancer study",
    #choices = c(studies),
    choices = c("", cancer_study_rc()),
    selected = 1
  )
})

observeEvent(input$add_study, {
  if(input$add_study_identifier == ""){
    showNotification("Please enter a cancer study ID", type="warning", duration = NULL)
    return()
  }
  # validate(
  #     need(input$add_study_identifier != "", "Please enter a cancer study ID")
  # )

  if(input$add_study_identifier %in% list.files(study_dir)){
    showModal(modalDialog(
      title = "Study ID already exists",
      "Do you want to overwrite the metadata of the existing study?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("overwrite_study", "Overwrite")
      )
    ))
  } else {

    # create study directory
    dir.create(file.path(study_dir, input$add_study_identifier))
    # meta_study
    meta_study_df <- data.frame(V1 = c("type_of_cancer", "cancer_study_identifier", "name", "description", "short_name", "add_global_case_list"),
                                V2 = c(input$type_of_cancer,input$add_study_identifier,input$add_study_name,input$description,input$add_short_name,"false")
    )
    write.table(meta_study_df, file.path(study_dir, input$add_study_identifier, "meta_study.txt.temp"), append = FALSE, sep = ": ", row.names = FALSE, col.names = FALSE, quote = FALSE)
    file.rename(file.path(study_dir, input$add_study_identifier, "meta_study.txt.temp"), file.path(study_dir, input$add_study_identifier, "meta_study.txt"))

    # update selectInput "select existing study"
    updateSelectInput(session, "cancer_study_identifier",
                      choices = c("", list.files(study_dir))
    )

    showNotification("Study added successfully!", type="message", duration = 10)
  }
})

# overwrite existing study
observeEvent(input$overwrite_study, {
  # create study directory
  dir.create(file.path(study_dir, input$add_study_identifier))
  # meta_study
  meta_study_df <- data.frame(V1 = c("type_of_cancer", "cancer_study_identifier", "name", "description", "short_name", "add_global_case_list"),
                              V2 = c(input$type_of_cancer,input$add_study_identifier,input$add_study_name,input$description,input$add_short_name,"false")
  )
  write.table(meta_study_df, file.path(study_dir, input$add_study_identifier, "meta_study.txt.temp"), append = FALSE, sep = ": ",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  file.rename(file.path(study_dir, input$add_study_identifier, "meta_study.txt.temp"), file.path(study_dir, input$add_study_identifier, "meta_study.txt"))

  # update selectInput "select existing study"
  updateSelectInput(session, "cancer_study_identifier",
                    choices = c("",list.files(study_dir))
  )
  showNotification("Study updated successfully!", type="message", duration = 10)
  removeModal()
})
