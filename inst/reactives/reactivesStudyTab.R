# image  ---------------------------------------------------------------
output$workflowImage <- renderImage(
  {
    return(
      list(
        src = system.file("www", "workflow.png", package = "cbpManager"),
        contentType = "image/png",
        alt = "workflow-cbpManager",
        width = "auto"
      )
    )
  },
  deleteFile = FALSE
)

# tour  ---------------------------------------------------------------
observeEvent(input$tour_study, {
  tour <- read.delim(system.file("apphelp", "tour_study.txt", package = "cbpManager"),
                     sep = ";", stringsAsFactors = FALSE,
                     row.names = NULL, quote = "")
  rintrojs::introjs(session, options = list(steps = tour))
})

# list files of study_dir ---------------------------------------------------------------
cancer_study_rc <- reactive({
  list.dirs(study_dir, full.names = FALSE, recursive = FALSE)
})

# dynamic cancer type selection with table ---------------------------------------------------------------
# UI of cancer type
output$ui_type_of_cancer <- renderUI({
  selectInput("type_of_cancer",
    label = "Select the cancer type (alternatively select in the table below)",
    choices = c("mixed", oncotree$code), width = "200px"
  )
})

# show table of oncotree codes
output$oncotree_table <- DT::renderDT({
  columns_to_show <- c("code", "name", "mainType", "tissue")
  DT::datatable(
    oncotree[, columns_to_show],
    selection = "single",
    rownames = FALSE,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      scrollY = TRUE
    )
  )
})

# update cancer type UI
observe(if (!is.null(input$oncotree_table_row_last_clicked)) {
  updateSelectInput(
    session,
    "type_of_cancer",
    choices = c("mixed", oncotree$code),
    selected = oncotree[input$oncotree_table_row_last_clicked, "code"]
  )
})

# add new study ---------------------------------------------------------------
observeEvent(input$add_study, {
  # check if "Add ID of cancer study" is empty
  if (input$add_study_identifier == "") {
    showNotification("Please enter a cancer study ID",
      type = "warning",
      duration = NULL
    )
    return()
  }

  # Sanitize study ID
  studyID <- create_name(input$add_study_identifier, toupper = FALSE)

  # check if provided study ID already exists
  if (studyID %in% list.dirs(study_dir, full.names = FALSE, recursive = FALSE)) {
    showModal(
      modalDialog(
        title = "Study ID already exists",
        "Do you want to overwrite the metadata of the existing study?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("overwrite_study", "Overwrite")
        )
      )
    )
  } else {
    # create study directory
    dir.create(file.path(study_dir, studyID))
    
    if(input$type_of_cancer == "") cancer_type <- "other" else cancer_type <- input$type_of_cancer
    
    # create meta_study data.frame
    meta_study_df <- data.frame(
      V1 = c(
        "type_of_cancer",
        "cancer_study_identifier",
        "name",
        "description",
        "short_name",
        "add_global_case_list",
        "reference_genome"
      ),
      V2 = c(
        cancer_type,
        studyID,
        input$add_study_name,
        input$description,
        input$add_short_name,
        "false",
        input$reference_genome
      )
    )
    if(input$citation != ""){
      meta_study_df <- rbind(meta_study_df, c("citation",input$citation))
    }
    if(input$pmid != ""){
      meta_study_df <- rbind(meta_study_df, c("pmid",input$pmid))
    }
    if(input$groups != ""){
      meta_study_df <- rbind(meta_study_df, c("groups",input$groups))
    }

    # write meta_study.txt
    write.table(
      meta_study_df,
      file.path(study_dir, studyID, "meta_study.txt.temp"),
      append = FALSE,
      sep = ": ",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    file.rename(
      file.path(study_dir, studyID, "meta_study.txt.temp"),
      file.path(study_dir, studyID, "meta_study.txt")
    )

    # logging
    if (!is.null(logDir)) {
      writeLogfile(
        outdir = logDir,
        modified_file = file.path(studyID, "meta_study.txt")
      )
    }

    # update UI "Select ID of cancer study"
    updateSelectInput(session,
      "cancer_study_identifier",
      choices = c("", list.dirs(study_dir, full.names = FALSE, recursive = FALSE))
    )

    showNotification(paste0("Study ", studyID, " added successfully!"),
      type = "message",
      duration = 10
    )
  }
})

# overwrite existing study ---------------------------------------------------------------
observeEvent(input$overwrite_study, {
  # Sanitize study ID
  studyID <- create_name(input$add_study_identifier, toupper = FALSE)
  
  if(input$type_of_cancer == "") cancer_type <- "other" else cancer_type <- input$type_of_cancer

  # create meta_study data.frame
  meta_study_df <- data.frame(
    V1 = c(
      "type_of_cancer",
      "cancer_study_identifier",
      "name",
      "description",
      "short_name",
      "add_global_case_list",
      "reference_genome"
    ),
    V2 = c(
      cancer_type,
      studyID,
      input$add_study_name,
      input$description,
      input$add_short_name,
      "false",
      input$reference_genome
    )
  )
  if(input$citation != ""){
    meta_study_df <- rbind(meta_study_df, c("citation",input$citation))
  }
  if(input$pmid != ""){
    meta_study_df <- rbind(meta_study_df, c("pmid",input$pmid))
  }
  if(input$groups != ""){
    meta_study_df <- rbind(meta_study_df, c("groups",input$groups))
  }

  # write meta_study.txt
  write.table(
    meta_study_df,
    file.path(study_dir, studyID, "meta_study.txt.temp"),
    append = FALSE,
    sep = ": ",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  file.rename(
    file.path(study_dir, studyID, "meta_study.txt.temp"),
    file.path(study_dir, studyID, "meta_study.txt")
  )

  # logging
  if (!is.null(logDir)) {
    writeLogfile(
      outdir = logDir,
      modified_file = file.path(studyID, "meta_study.txt")
    )
  }

  # update UI "Select ID of cancer study"
  updateSelectInput(session,
    "cancer_study_identifier",
    choices = c("", list.dirs(study_dir, full.names = FALSE, recursive = FALSE))
  )

  showNotification(paste0("Study ", studyID, " updated successfully!"),
    type = "message",
    duration = 10
  )
  removeModal()
})

# Select existing study ---------------------------------------------------------------
# UI of "Select ID of cancer study"
output$ui_cancer_study_identifier <- renderUI({
  selectInput(
    "cancer_study_identifier",
    label = "Select ID of cancer study",
    choices = c("", cancer_study_rc()),
    selected = 1
  )
})

# Initiate a reactive object
loadedData <- reactiveValues()
customTimelines <- reactiveValues()

# load data of selected study ---------------------------------------------------------------
observeEvent(input$upload, {
  req(input$cancer_study_identifier)
  # check if empty study ID ---------------------------------------------------------------
  if (input$cancer_study_identifier == "") {
    showNotification("Please select a cancer study ID",
      type = "warning",
      duration = NULL
    )
    return()
  }

  # fill reactive object ---------------------------------------------------------------
  loadedData$studyID <- NULL
  loadedData$data_clinical_patient <- data.frame(
    PATIENT_ID = c("Patient Identifier", "Patient identifier", "STRING", "e.g. JohnDoe"),
    ATTRIBUTE = c("Name of attr.", "Longer name of attr.", "STRING", "Value of attr."),
    stringsAsFactors = FALSE
  )
  loadedData$data_clinical_sample <- data.frame(
    PATIENT_ID = c("Patient Identifier", "Patient identifier", "STRING", "e.g. JohnDoe"),
    SAMPLE_ID = c("Sample Identifier", "Sample identifier", "STRING", "e.g. JohnDoe_1"),
    ATTRIBUTE = c("Name of attr.", "Longer name of attr.", "STRING", "Value of attr."),
    stringsAsFactors = FALSE
  )
  loadedData$data_mutations_filename <- "data_mutations_extended.txt"
  loadedData$data_mutations_extended <- data.frame(
    Hugo_Symbol = character(),
    Tumor_Sample_Barcode = character(),
    Variant_Classification = character(),
    HGVSp_Short = character()
  )
  loadedData$data_timeline_treatment <- data.frame(
    PATIENT_ID = character(),
    START_DATE = numeric(),
    STOP_DATE = numeric(),
    EVENT_TYPE = factor(levels = c("TREATMENT")),
    TREATMENT_TYPE = factor(levels = c("Medical Therapy", "Radiation Therapy")),
    SUBTYPE = character(),
    AGENT = character(),
    stringsAsFactors = FALSE
  )
  loadedData$data_timeline_status <- data.frame(
    PATIENT_ID = character(),
    START_DATE = numeric(),
    STOP_DATE = numeric(),
    EVENT_TYPE = factor(levels = c("STATUS")),
    STATUS = character(),
    SOURCE = character(),
    stringsAsFactors = FALSE
  )
  loadedData$data_timeline_surgery <- data.frame(
    PATIENT_ID = character(),
    START_DATE = numeric(),
    STOP_DATE = numeric(),
    EVENT_TYPE = factor(levels = c("SURGERY")),
    EVENT_TYPE_DETAILED = character(),
    stringsAsFactors = FALSE
  )
  loadedData$dates_first_diagnosis <-
    data.frame(PATIENT_ID = character(), DATE = character())


  customTimelines$timelines <- data.frame(
    name = character(),
    shortName = character(),
    mode = factor(levels = c("timeline", "timepoint"))
  )

  loadedData$data_resource_definition <- data.frame(
    RESOURCE_ID = character(),
    DISPLAY_NAME = character(),
    RESOURCE_TYPE = factor(levels = c("SAMPLE", "PATIENT", "STUDY")),
    DESCRIPTION = character(),
    OPEN_BY_DEFAULT = logical(),
    PRIORITY = character(),
    stringsAsFactors = FALSE
  )
  loadedData$data_resource_study <- data.frame(
    RESOURCE_ID = character(),
    URL = character(),
    stringsAsFactors = FALSE
  )
  loadedData$data_resource_patient <- data.frame(
    PATIENT_ID = character(),
    RESOURCE_ID = character(),
    URL = character(),
    stringsAsFactors = FALSE
  )
  loadedData$data_resource_sample <- data.frame(
    PATIENT_ID = character(),
    SAMPLE_ID = character(),
    RESOURCE_ID = character(),
    URL = character(),
    stringsAsFactors = FALSE
  )

  # set studyID ---------------------------------------------------------------
  loadedData$studyID <- input$cancer_study_identifier

  # read meta_study ---------------------------------------------------------------
  meta_study_file <-
    file.path(study_dir, loadedData$studyID, "meta_study.txt")
  if (file.exists(meta_study_file)) {
    meta_study <- read.table(meta_study_file, sep = ":")
    loadedData$meta_study <- meta_study
  }

  # read data_clinical_patient ---------------------------------------------------------------
  data_clinical_patient_file <-
    file.path(study_dir, loadedData$studyID, "data_clinical_patient.txt")
  if (file.exists(data_clinical_patient_file)) {
    # read file and convert to data.frame
    data_clinical_patient <- read.table(
      data_clinical_patient_file,
      sep = "\t",
      colClasses = "character",
      comment.char = ""
    ) %>%
      cBioPortalToDataFrame()
    loadedData$data_clinical_patient <- data_clinical_patient
  }

  # read data_clinical_sample ---------------------------------------------------------------
  data_clinical_sample_file <-
    file.path(study_dir, loadedData$studyID, "data_clinical_sample.txt")
  if (file.exists(data_clinical_sample_file)) {
    data_clinical_sample <- read.table(
      data_clinical_sample_file,
      sep = "\t",
      colClasses = "character",
      comment.char = ""
    ) %>%
      cBioPortalToDataFrame()
    loadedData$data_clinical_sample <- data_clinical_sample
  }

  # read data_mutations_extended ---------------------------------------------------------------
  meta_mutations_extended <-
    file.path(study_dir, loadedData$studyID, "meta_mutations_extended.txt")
  if (file.exists(meta_mutations_extended)) {
    meta_mutations <- read.table(meta_mutations_extended, sep = ":")
    loadedData$data_mutations_filename <-
      gsub(" ", "", meta_mutations[meta_mutations$V1 == "data_filename", ]$V2)
  }

  data_mutations_extended_file <-
    file.path(
      study_dir,
      loadedData$studyID,
      loadedData$data_mutations_filename
    )
  if (file.exists(data_mutations_extended_file)) {
    data_mutations_extended <-
      read.table(data_mutations_extended_file,
        sep = "\t",
        header = TRUE,
        comment.char = "#",
        stringsAsFactors = FALSE,
        quote = "",
        fill = FALSE
      )
    loadedData$data_mutations_extended <- data_mutations_extended
  }

  # read data_timeline_treatment ---------------------------------------------------------------
  data_timeline_treatment_file <-
    file.path(
      study_dir,
      loadedData$studyID,
      "data_timeline_treatment.txt"
    )
  if (file.exists(data_timeline_treatment_file)) {
    data_timeline_treatment <-
      read.table(data_timeline_treatment_file,
        sep = "\t",
        header = TRUE
      )
    loadedData$data_timeline_treatment <- data_timeline_treatment
  }

  # read data_timeline_status ---------------------------------------------------------------
  data_timeline_status_file <-
    file.path(study_dir, loadedData$studyID, "data_timeline_status.txt")
  if (file.exists(data_timeline_status_file)) {
    data_timeline_status <-
      read.table(data_timeline_status_file,
        sep = "\t",
        header = TRUE
      )
    loadedData$data_timeline_status <- data_timeline_status
  }


  # read data_timeline_surgery ---------------------------------------------------------------
  data_timeline_surgery_file <-
    file.path(study_dir, loadedData$studyID, "data_timeline_surgery.txt")
  if (file.exists(data_timeline_surgery_file)) {
    data_timeline_surgery <-
      read.table(data_timeline_surgery_file,
        sep = "\t",
        header = TRUE
      )
    loadedData$data_timeline_surgery <- data_timeline_surgery
  }

  # read custom data_timeline_ files ---------------------------------------------------------------
  all_study_files <- list.files(file.path(study_dir, loadedData$studyID))
  timeline_files <- all_study_files[grep("data_timeline_", all_study_files)]
  exclude_tls <-
    c(
      "data_timeline_surgery.txt",
      "data_timeline_status.txt",
      "data_timeline_treatment.txt"
    )
  timeline_files <- timeline_files[-which(timeline_files %in% exclude_tls)]

  for (tl_filename in timeline_files) {
    tl_file <- file.path(study_dir, loadedData$studyID, tl_filename)
    track <- read.table(tl_file, sep = "\t", header = TRUE)
    track_var <- gsub(".txt", "", tl_filename)
    loadedData[[track_var]] <- track

    if (all(is.na(loadedData[[track_var]][, "STOP_DATE"]))) {
      mode <- "timepoint"
    } else {
      mode <- "timeline"
    }
    customTimelines$timelines[track_var, ] <- list(name = track_var, shortName = gsub("data_timeline_", "", track_var), mode = mode)
  }
  customTimelines$selectedTrack <- NULL


  # read diagnosis_dates ---------------------------------------------------------------
  dates_first_diagnosis_file <-
    file.path(study_dir, loadedData$studyID, "dates_first_diagnosis.txt")
  if (file.exists(dates_first_diagnosis_file)) {
    dates_first_diagnosis <-
      read.table(dates_first_diagnosis_file,
        sep = "\t",
        header = TRUE,
        comment.char = ""
      )
    loadedData$dates_first_diagnosis <- dates_first_diagnosis
  }

  # read data_resource files ---------------------------------------------------------------
  data_resource_definition_file <-
    file.path(study_dir,
              loadedData$studyID,
              "data_resource_definition.txt")
  if (file.exists(data_resource_definition_file)) {
    data_resource_definition <-
      read.table(data_resource_definition_file,
                 sep = "\t",
                 header = TRUE)
    loadedData$data_resource_definition <- data_resource_definition
  }
  data_resource_study_file <-
    file.path(study_dir,
              loadedData$studyID,
              "data_resource_study.txt")
  if (file.exists(data_resource_study_file)) {
    data_resource_study <-
      read.table(data_resource_study_file,
                 sep = "\t",
                 header = TRUE)
    loadedData$data_resource_study <- data_resource_study
  }
  data_resource_patient_file <-
    file.path(study_dir,
              loadedData$studyID,
              "data_resource_patient.txt")
  if (file.exists(data_resource_patient_file)) {
    data_resource_patient <-
      read.table(data_resource_patient_file,
                 sep = "\t",
                 header = TRUE)
    loadedData$data_resource_patient <- data_resource_patient
  }
  data_resource_sample_file <-
    file.path(study_dir,
              loadedData$studyID,
              "data_resource_sample.txt")
  if (file.exists(data_resource_sample_file)) {
    data_resource_sample <-
      read.table(data_resource_sample_file,
                 sep = "\t",
                 header = TRUE)
    loadedData$data_resource_sample <- data_resource_sample
  }

  showNotification(paste0("Study ", loadedData$studyID, " loaded successfully! You can now proceed with the Patient tab."),
    type = "message",
    duration = 10
  )
})

# show table of metadata ---------------------------------------------------------------
output$studyTable <- DT::renderDT({
  if (!is.null(loadedData$meta_study)) {
    DT::datatable(loadedData$meta_study, colnames = rep("", ncol(loadedData$meta_study)),
                  caption = 'Meta data of loaded study:')
  }
})

# UI of "loaded study info"
output$ui_loaded_study_info <- renderUI({
  if(!is.null(loadedData$studyID)){
    column(width = 12,
      box(
        title = "Loaded study:", status = "success", solidHeader = TRUE,
        collapsible = FALSE, width=NULL,
        renderText(paste("ID:",loadedData$studyID)),
        renderText(paste("Name:",loadedData$meta_study[which(loadedData$meta_study$V1=="name"),]$V2))
      )
    )

  }
})

study_tracker <- reactiveValues()

study_tracker$df <- data.frame(
  Data=c("Patient data", "Sample data", "Mutation data", "Timeline data", "Resource data"),
  Saved=c(as.character(icon("check-circle")))
)

# UI of change tracker
output$ui_change_tracker <- DT::renderDT({
  #all_study_files <- list.files(file.path(study_dir, loadedData$studyID))
  if(!is.null(loadedData$studyID)){
      DT::datatable(
        study_tracker$df,
        escape = FALSE,
        rownames= FALSE,
        selection = "none",
        options = list(
          dom = 't',
          ordering=FALSE,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
            "$(this.api().table().body()).css({'background-color': 'white', 'color': '#517fb9'});",
            "}")
        )
      )
    
  }
})

#UI of package version
output$package_version <- renderText({paste("Version:", packageVersion("cbpManager"))})
