#' Convert the data.frame to the appropriate file format for cBioPortal
#'
#' @param df data.frame
#' @return Data.frame formated for the cBioPortal file format
#' @examples
#' cbpManager:::convertDataFrame(
#'     data.frame(
#'         ATTRIBUTE1=c("attr_1", "attribute 1", "STRING", "value_a1"), 
#'         ATTRIBUTE2=c("attr_2", "attribute 2", "STRING", "value_b1")
#'     )
#' )
#' 
convertDataFrame <- function(df) {
  df[df == ""] <- NA
  short_names <- df[1, ]
  long_names <- df[2, ]
  data_type <- df[3, ]
  priority <- rep(1, ncol(df))
  column_names <- colnames(df)
  final_df <- rbind(
    short_names,
    long_names,
    data_type,
    priority,
    column_names,
    df[4:nrow(df), ]
  )
  final_df[seq_len(4), 1] <- paste0("#", final_df[seq_len(4), 1])
  rownames(final_df) <- seq_len(nrow(final_df))

  return(final_df)
}

#' Check if input is in the appropriate date format
#'
#' @param mydate date
#' @param date.format string describig the date format
#' @return boolean
#' @examples
#' cbpManager:::IsDate("2020-02-20")
#' cbpManager:::IsDate("20.01.2020", date.format = "%d.%m.%Y")
#' 
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
    error = function(err) {
      FALSE
    }
  )
}

#' Check the input of dates
#'
#' @param diagnosisDate date of first diagnosis
#' @param startDate start date of timeline event
#' @param endDate end date of timeline event
#' @return Returns a number indicating the warning
#' @examples
#' cbpManager:::check_input_dates(
#'     diagnosisDate = "2020-01-01", 
#'     startDate = "2020-02-01", 
#'     endDate = "2020-03-01"
#' ) #returns 0
#' cbpManager:::check_input_dates(
#'     diagnosisDate = "2020-01-01", 
#'     startDate = "2019-02-01"
#' ) #returns 2
#' cbpManager:::check_input_dates(
#'     diagnosisDate = "2020-01-01", 
#'     endDate = "2019-02-01"
#' ) #returns 2
#' cbpManager:::check_input_dates(
#'     diagnosisDate = "2020-01-01", 
#'     startDate = "2020-03-01", 
#'     endDate = "2020-02-01"
#' ) #returns 1
#' 
check_input_dates <- function(diagnosisDate, startDate = NULL, endDate = NULL) {
  return_value <- 0
  if (!is.null(startDate)) {
    if (as.Date(startDate, "%Y-%m-%d") < as.Date(diagnosisDate, "%Y-%m-%d")) {
      return_value <- 2
    }
  }
  if (!is.null(endDate)) {
    if (as.Date(endDate, "%Y-%m-%d") <= as.Date(diagnosisDate, "%Y-%m-%d")) {
      return_value <- 2
    }
  }
  if (!is.null(startDate) & !is.null(endDate)) {
    if (as.Date(endDate, "%Y-%m-%d") <= as.Date(startDate, "%Y-%m-%d")) {
      return_value <- 1
    }
  }
  return(return_value)
}


#' Sanitize names
#'
#' This function takes a character string, replaces spaces by underscores and runs make.names.
#' @param x A character string.
#' @param toupper If TRUE, the name wil be upper-case; if FALSE, the name will be lower-case.
#' @return A sanitized string.
#' @examples
#' cbpManager:::create_name("Study name 1") #returns "STUDY_NAME_1"
#' cbpManager:::create_name("FANCY;name", toupper = FALSE) #returns "fancy.name"
#' 
create_name <- function(x, toupper = TRUE) {
  if (toupper) {
    x <- toupper(x)
  } else {
    x <- tolower(x)
  }
  x <- gsub(x = x, pattern = " ", replacement = "_")
  x <- make.names(x)
  return(x)
}

#' Generate UI input widget
#'
#' @param colname A character string - the name of the column, that will be the label of the input
#' @param mode "add" or "edit" - whether to use existing values or not
#' @param tab "Patient", "Sample" - The used tab; sets the html id prefix of the input
#' @param data A data.frame.
#' @param selected_row A number indicating the row number of the selected row in the data.frame.
#' @param patientIDs Vector of patient IDs used for drop down menu of the PATIENT_ID column
#' @return A sanitized string.
#' @examples 
#' cbpManager:::generateUIwidgets(colname = "attribute", mode = "add", tab = "Patient")
#' 
generateUIwidgets <- function(colname, mode = c("add", "edit"), tab = c("Patient", "Sample", "Mutation"), data = NULL, selected_row = NULL, patientIDs = NULL, sampleIDs = NULL) {
  mode <- match.arg(mode)
  tab <- match.arg(tab)

  if (mode == "add" & tab == "Patient") {
    id_prefix <- "addPatientInput_"
    textvalue <- numvalue <- ""
    selected <- 1
  } else if (mode == "add" & tab == "Sample") {
    id_prefix <- "addSampleInput_"
    numvalue <- 0
    textvalue <- numvalue <- ""
    selected <- 1
  } else if (mode == "add" & tab == "Mutation") {
    id_prefix <- "addMutationInput_"
    numvalue <- 0
    textvalue <- numvalue <- ""
    selected <- 1
  } else if (mode == "edit" & tab == "Patient") {
    id_prefix <- "editPatientInput_"
    numvalue <- selected <- textvalue <- data[selected_row, colname]
  } else if (mode == "edit" & tab == "Sample") {
    id_prefix <- "editSampleInput_"
    numvalue <- selected <- textvalue <- data[selected_row, colname]
  } else if (mode == "edit" & tab == "Mutation") {
    id_prefix <- "editMutationInput_"
    numvalue <- selected <- textvalue <- data[selected_row, colname]
  }

  if (colname == "PATIENT_ID" & tab == "Sample") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = patientIDs,
        selected = selected
      ),
    ))
  } else if (colname == "Tumor_Sample_Barcode" & tab == "Mutation") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = sampleIDs,
        selected = selected
      ),
    ))
  }  else if (colname == "Variant_Classification" & tab == "Mutation") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = patientIDs,
        selected = selected
      ),
    ))
  } else if (colname == "OS_STATUS") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("DECEASED", "LIVING"),
        selected = selected
      ),
    ))
  } else if (colname == "OS_MONTHS") {
    fluidRow(column(
      width = 8,
      numericInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        value = numvalue,
        min = 0
      ),
    ))
  } else if (colname == "DFS_STATUS") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("DiseaseFree", "Recurred/Progressed"),
        selected = selected
      ),
    ))
  } else if (colname == "DFS_MONTHS") {
    fluidRow(column(
      width = 8,
      numericInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        value = numvalue,
        min = 0
      ),
    ))
  } else if (colname == "GENDER") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("Unkown", "Male", "Female", "Diverse"),
        selected = selected
      ),
    ))
  } else if (colname == "SEX") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("Unkown", "Male", "Female", "Diverse"),
        selected = selected
      ),
    ))
  } else if (colname == "AGE") {
    fluidRow(column(
      width = 8,
      numericInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        value = numvalue,
        min = 0
      ),
    ))
  } else if (colname == "CANCER_TYPE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("other", cancer_type),
        selected = selected
      ),
    ))
  } else if (colname == "CANCER_TYPE_DETAILED") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("other", cancer_type_detailed),
        selected = selected
      ),
    ))
  } else if (colname == "TUMOR_TISSUE_SITE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("other", tumor_tissue_site),
        selected = selected
      ),
    ))
  } else if (colname == "ONCOTREE_CODE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("", oncotree_code),
        selected = selected
      ),
    ))
  } else if (colname == "SAMPLE_TYPE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("primary", "recurrence", "recurred", "progression", "progressed", "metastatic", "metastasis"),
        selected = selected
      ),
    ))
  } else if (colname == "Mutation_Status" & tab == "Mutation") {
    fluidRow(column(
      width = 8,
      selectizeInput(
        inputId = paste0(id_prefix, colname),
        label = paste0(colname, " (user text input allowed)"),
        choices = c("None", "Somatic", "Germline", "LOH", "Wildtype"),
        options = list(create = TRUE),
        selected = selected
      ),
    ))
  } else if (colname == "Verification_Status" & tab == "Mutation") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("NA", "Verified", "Unknown"),
        selected = selected
      ),
    ))
  } else if (colname == "Validation_Status" & tab == "Mutation") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        choices = c("NA", "Valid", "Invalid", "Untested", "Inconclusive", "Redacted", "Unknown"),
        selected = selected
      ),
    ))
  } else {
    fluidRow(column(
      width = 8,
      textInput(
        inputId = paste0(id_prefix, colname),
        label = colname,
        value = textvalue
      ),
    ))
  }
}

#' Create shiny UI-widget for specific columns of oncotree entries
#'
#' @param colname column name
#' @param mode determines the inputId prefix of the UI-widget
#' @param tab "Patient", "Sample" - The used tab; sets the html id prefix of the input
#' @return A oncotree specific shiny UI-widget
#' @examples 
#' oncotree <- jsonlite::fromJSON(system.file("extdata", "oncotree.json", package = "cbpManager"))
#' cancer_type <- unique(oncotree$mainType[which(!is.na(oncotree$mainType))])
#' cbpManager:::generateOncotreeUIwidgets("CANCER_TYPE", "add")
#' 
generateOncotreeUIwidgets <- function(colname, mode = c("add", "edit"), tab=c("Patient", "Sample")) {
  mode <- match.arg(mode)
  tab <- match.arg(tab)

  if (mode == "add" & tab == "Patient") {
    id_prefix <- "addPatientInput_"
  } else if (mode == "edit" & tab == "Patient") {
    id_prefix <- "editPatientInput_"
  } else if (mode == "add" & tab == "Sample") {
    id_prefix <- "addSampleInput_"
  } else if (mode == "edit" & tab == "Sample") {
    id_prefix <- "editSampleInput_"
  }

  if (colname == "CANCER_TYPE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, "CANCER_TYPE"),
        label = "CANCER_TYPE",
        choices = c("other", cancer_type),
        selected = 1
      ),
    ))
  } else if (colname == "CANCER_TYPE_DETAILED") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, "CANCER_TYPE_DETAILED"),
        label = "CANCER_TYPE_DETAILED",
        choices = c("other", cancer_type_detailed),
        selected = 1
      ),
    ))
  } else if (colname == "ONCOTREE_CODE") {
    fluidRow(column(
      width = 8,
      selectInput(
        inputId = paste0(id_prefix, "ONCOTREE_CODE"),
        label = "ONCOTREE_CODE",
        choices = c("", oncotree_code),
        selected = 1
      ),
    ))
  }
}

#' Updates UI-widgets for specific columns of oncotree entries
#'
#' @param session Shiny session
#' @param row_last_clicked the index of the row last clicked in the oncotree_table
#' @param mode determines the inputId prefix of the UI-widget
#' @param tab "Patient", "Sample" - The used tab; sets the html id pr
#' @return nothing to return
updateOncotreeUIwidgets <- function(session, row_last_clicked, mode = c("add", "edit"), tab=c("Patient", "Sample")) {
  mode <- match.arg(mode)
  tab <- match.arg(tab)

  if (mode == "add" & tab == "Patient") {
    id_prefix <- "addPatientInput_"
  } else if (mode == "edit" & tab == "Patient") {
    id_prefix <- "editPatientInput_"
  } else if (mode == "add" & tab == "Sample") {
    id_prefix <- "addSampleInput_"
  } else if (mode == "edit" & tab == "Sample") {
    id_prefix <- "editSampleInput_"
  }

  updateSelectInput(session, paste0(id_prefix, "CANCER_TYPE"),
    choices = c("other", cancer_type),
    selected = oncotree$mainType[row_last_clicked]
  )
  updateSelectInput(session, paste0(id_prefix, "CANCER_TYPE_DETAILED"),
    label = "CANCER_TYPE_DETAILED",
    choices = c("other", cancer_type_detailed),
    selected = oncotree$name[row_last_clicked]
  )
  updateSelectInput(session, paste0(id_prefix, "ONCOTREE_CODE"),
    label = "ONCOTREE_CODE",
    choices = c("", oncotree_code),
    selected = oncotree$code[row_last_clicked]
  )
}

#' Add empty column to a data.frame, if column does not exist in the data.frame
#'
#' @param data data.frame
#' @param cname column name
#' @return data.frame
#' @examples 
#' cbpManager:::fncols(data.frame(a=c(1,2,3), b=c(4,5,6)), "new")
#' 
fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]

  if (length(add) != 0) data[add] <- ""
  return(data)
}

#' Convert the cBioPortal sample- and patient-data file format into a data.frame
#'
#' This function takes a file object (from read.table), removes the # symbol,
#' sets the 5th row as the column names of the data.frame
#' and removes the rows containing the priority, data type and column name.
#' use read.table as follows: \code{read.table(file, sep='\t', colClasses = 'character', comment.char = '')}
#' @param data The data.frame of a cBioPortal sample/patient data file
#' @return data.frame
#' @examples 
#' df <- data.frame(
#'         V1=c("#attr_1", "#attribute 1", "#STRING", "#1", "ATTRIBUTE_1", "value_1"), 
#'         V2=c("attr_2", "attribute 2", "STRING", "1", "ATTRIBUTE_2", "value_2")
#'     )
#' cbpManager:::cBioPortalToDataFrame(df)
#' 
cBioPortalToDataFrame <- function(data) {
  data$V1 <- sub(pattern = "^#", replacement = "", x = data$V1)
  colnames(data) <- data[5, ]
  data <- data[-c(4, 5), ]
  rownames(data) <- seq_len(nrow(data))
  return(data)
}

#' Get Sample IDs associated with Patient IDs from the data_clinical_sample.txt file
#'
#' @param file_path A character string.
#' @param patIDs A character string.
#' @return vector with Sample IDs
#' @examples
#' cbpManager:::getSampleIDs(
#'     system.file("study/testpatient/data_clinical_sample.txt", package = "cbpManager"), 
#'     patIDs = "Testpatient")
#' 
getSampleIDs <- function(file_path, patIDs) {
  if (file.exists(file_path)) {
    # read data file
    whole_data <- read.table(file_path, sep = "\t", colClasses = "character", comment.char = "")
    whole_data <- cBioPortalToDataFrame(whole_data)
    associatedSampleIDs <- whole_data[whole_data$PATIENT_ID %in% patIDs, "SAMPLE_ID"]
    if (length(associatedSampleIDs) == 0) {
      return(NULL)
    } else {
      return(associatedSampleIDs)
    }
  } else {
    return(NULL)
  }
}

#' Import patient data into current study data.frames
#'
#' @param mode Defines the type of imported data
#' @param file_name Filename of source data
#' @param file_path Filepath with filename of source data
#' @param patIDs PATIENT_IDs of patients that should be imported
#' @param data Source data, to be subsetted according to patIDs
#' @param associatedSampleIDs The sample IDs associated to the patIDs
#' @return data.frame
#' 
importPatientData <- function(mode = c("patient", "sample", "mutations", "timelines"), file_name, file_path, patIDs, data, associatedSampleIDs = NULL) {
  if (file.exists(file_path)) {
    # read data file
    whole_data <- NULL
    if (mode == "patient" | mode == "sample") {
      whole_data <- read.table(file_path, sep = "\t", colClasses = "character", comment.char = "")
      whole_data <- cBioPortalToDataFrame(whole_data)
    } else if (mode == "mutations" | mode == "timelines") {
      whole_data <- as.data.frame(vroom::vroom(file_path, delim = "\t"))
    }

    # extract rows
    if (mode == "patient" | mode == "sample" | mode == "timelines") {
      extracted_data <- whole_data[whole_data$PATIENT_ID %in% patIDs, ]
      emptyColNames <- setdiff(colnames(extracted_data), colnames(data))
    } else if (mode == "mutations") {
      if (is.null(associatedSampleIDs)) {
        return(data)
      } else {
        extracted_data <- whole_data[whole_data$Tumor_Sample_Barcode %in% associatedSampleIDs, ]
      }
    }

    # add rows to table
    if (nrow(extracted_data) != 0) {
      data <- dplyr::bind_rows(data, extracted_data)
    }

    # add missing short- and long & data type column names
    if (mode == "patient" | mode == "sample") {
      if (length(emptyColNames) != 0) {
        for (emptyCol in emptyColNames) {
          data[1, emptyCol] <- whole_data[1, emptyCol]
          data[2, emptyCol] <- whole_data[2, emptyCol]
          data[3, emptyCol] <- whole_data[3, emptyCol]
        }
      }
    }

    showNotification(paste0("Successfully imported data from ", file_name, "!"), type = "message", duration = NULL)
    return(data)
  } else {
    showNotification(paste0("File ", file_name, " not found in study."), type = "warning", duration = NULL)
    return(data)
  }
}

#' Write a line to a logfile containing the date, time, username (from Shinyproxy), and the name of the modified file.
#'
#' @param outdir directory, where the logfile should be saved
#' @param modified_file Name of the modified file
#' @param log_filename Name of the logfile
#' @return Nothing to return
#' @examples 
#' cbpManager:::writeLogfile(tempdir(), "data_clinical_patient.txt")
#' 
writeLogfile <- function(outdir, modified_file, log_filename = "cbpManager_logfile.txt") {
  userName <- Sys.getenv("SHINYPROXY_USERNAME")
  if (userName == "") userName <- "NO_USERNAME"
  timepoint <- Sys.time()
  log <- paste0(timepoint, "   User: ", userName, " modified file '", modified_file, "'")
  write(log, file = file.path(outdir, log_filename), append = TRUE)
  invisible(file.path(outdir, log_filename))
}

#' Install conda environment with basilisk before launching the app
#'
#' @return Nothing to return
#'
#' @examples
#' \dontrun{
#' setupConda_cbpManager()
#' }
#' 
#' @export setupConda_cbpManager
setupConda_cbpManager <- function() {
  proc <- basiliskStart(env_validation)
  on.exit(basiliskStop(proc))
  checkPackages <- basiliskRun(proc, function() {
    packages_df <- basilisk::listPackages(env_validation)
    if (!all(c("Jinja2", "requests", "PyYAML") %in% packages_df$package)) {
      warning(
      "Some problems occured during the installation of the conda environment.
One or more of the necessary packages were not installed.
Please try reinstalling cbpManager and basilisk or contact the support at https://github.com/arsenij-ust/cbpManager/issues")
    }
  })
}
