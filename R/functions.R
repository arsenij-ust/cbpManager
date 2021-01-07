#' Define the datatype required in cBioPortal file format
#' NOT WORKING YET
#'
#' @param df data.frame
#' @return String according to the data type of the column
findDatatype <- function(df){
  if(!any(is.na(as.numeric(df)))){
    return("NUMBER")
  } else if(!any(is.na(as.logical(df)))) {
    return("BOOLEAN")
  } else {
    return("STRING")
  }
}

#' Convert the data.frame to the appropriate file format for cBioPortal
#'
#' @param df data.frame
#' @return Data.frame formated for the cBioPortal file format
convertDataFrame <- function(df){
  df_formated <- df[c(1,2),]
  data_type <- sapply(df[3:nrow(df),], findDatatype)
  df_formated[3,] <- data_type
  df_formated[4,] <- 1
  df_formated[,1] <- paste0("#",df_formated[,1])
  df_formated[5,] <- colnames(df)
  df_formated <- rbind(df_formated, df[3:nrow(df),])
  return(df_formated)
}

#' Check if input is in the appropriate date format
#'
#' @param mydate date
#' @param date.format string describig the date format
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

#' Check the input of dates
#'
#' @param diagnosisDate date of first diagnosis
#' @param startDate start date of timeline event
#' @param endDate end date of timeline event
#' @return Returns a number indicating the warning
check_input_dates <- function(diagnosisDate, startDate=NULL, endDate=NULL){
  if(!is.null(startDate)){
    if(as.Date(startDate, "%Y-%m-%d") < as.Date(diagnosisDate, "%Y-%m-%d")) return(2)
  }
  if(!is.null(endDate)){
    if(as.Date(endDate, "%Y-%m-%d") <= as.Date(diagnosisDate, "%Y-%m-%d")) return(2)
  }
  if(!is.null(startDate) & !is.null(endDate)){
    if(as.Date(endDate, "%Y-%m-%d") <= as.Date(startDate, "%Y-%m-%d")) return(1)
  }
  return(0)
}

#' Sanitize names
#'
#' This function takes a character string, replaces spaces by underscores and runs make.names.
#' @param x A character string.
#' @param toupper If TRUE, the name wil be upper-case; if FALSE, the name will be lower-case.
#' @return A sanitized string.
.create_name <- function(x, toupper = TRUE) {
  . = NULL # workaround for R CMD check note: no visible binding for global variable '.'
  if(toupper){
    x %>% toupper %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names %>% return()
  } else {
    x %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names %>% return()
  }

}

#' Generate UI input widget
#'
#' @param colname A character string - the name of the column, that will be the label of the input
#' @param mode "add" or "edit" - wether to use existing values or not
#' @param tab "Patient", "Sample" - The used tab; sets the html id prefix of the input
#' @param data A data.frame.
#' @param selected_row A number indicating the row number of the selected row in the data.frame.
#' @param patientIDs Vector of patient IDs used for drop down menu of the PATIENT_ID column
#' @return A sanitized string.
generateUIwidgets <- function(colname, mode = c("add", "edit"), tab = c("Patient", "Sample"), data = NULL, selected_row = NULL, patientIDs = NULL){

  mode <- match.arg(mode)
  tab <- match.arg(tab)

  if(mode == "add" & tab == "Patient"){
    id_prefix <- "addPatientInput_"
    textvalue <- numvalue <- ""
    selected <- 1
  } else if (mode == "add" & tab == "Sample"){
    id_prefix <- "addSampleInput_"
    numvalue <- 0
    textvalue <- numvalue <- ""
    selected <- 1
  } else if (mode == "edit" & tab == "Patient"){
    id_prefix <- "editPatientInput_"
    numvalue <- selected <- textvalue <- data[selected_row,colname]
  } else if (mode == "edit" & tab == "Sample"){
    id_prefix <- "editSampleInput_"
    numvalue <- selected <- textvalue <- data[selected_row,colname]
  }

  #if((colname == "PATIENT_ID" & mode == "edit")|(colname == "PATIENT_ID" & tab == "Sample")){
  if(colname == "PATIENT_ID" & tab == "Sample"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = patientIDs,
                  selected = selected),))
  } else if(colname == "OS_STATUS"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("DECEASED", "LIVING"),
                  selected = selected),))
  } else if(colname == "OS_MONTHS"){
    fluidRow(column(
      width = 8,
      numericInput(inputId=paste0(id_prefix,colname),
                   label = colname,
                   value = numvalue,
                   min = 0),))
  } else if(colname == "DFS_STATUS"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("DiseaseFree", "Recurred/Progressed"),
                  selected = selected),))
  } else if(colname == "DFS_MONTHS"){
    fluidRow(column(
      width = 8,
      numericInput(inputId=paste0(id_prefix,colname),
                   label = colname,
                   value = numvalue,
                   min = 0),))
  } else if(colname == "GENDER"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("Unkown", "Male", "Female", "Diverse"),
                  selected = selected),))
  } else if(colname == "SEX"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("Unkown", "Male", "Female", "Diverse"),
                  selected = selected),))
  } else if(colname == "AGE"){
    fluidRow(column(
      width = 8,
      numericInput(inputId=paste0(id_prefix,colname),
                   label = colname,
                   value = numvalue,
                   min = 0),))
  } else if(colname == "CANCER_TYPE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("other", cancer_type),
                  selected = selected),))
  } else if(colname == "CANCER_TYPE_DETAILED"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("other", cancer_type_detailed),
                  selected = selected),))
  } else if(colname == "TUMOR_TISSUE_SITE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("other", tumor_tissue_site),
                  selected = selected),))
  } else if(colname == "ONCOTREE_CODE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("", oncotree_code),
                  selected = selected),))
  } else if(colname == "SAMPLE_TYPE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,colname),
                  label = colname,
                  choices = c("primary", "recurrence", "recurred", "progression", "progressed", "metastatic", "metastasis"),
                  selected = selected),))
  } else {
    fluidRow(column(
      width = 8,
      textInput(inputId=paste0(id_prefix,colname),
                label = colname,
                value = textvalue),))
  }
}

#' Create shiny UI-widget for specific columns of oncotree entries
#'
#' @param colname column name
#' @param mode determines the inputId prefix of the UI-widget
#' @return A oncotree specific shiny UI-widget
generateOncotreeUIwidgets <- function(colname, mode = c("add", "edit")){
  mode <- match.arg(mode)

  if(mode == "add"){
    id_prefix <- "addSampleInput_"
  } else if (mode == "edit"){
    id_prefix <- "editSampleInput_"
  }

  if(colname == "CANCER_TYPE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,"CANCER_TYPE"),
                  label = "CANCER_TYPE",
                  choices = c("other", cancer_type),
                  selected = 1),))
  } else if(colname == "CANCER_TYPE_DETAILED"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,"CANCER_TYPE_DETAILED"),
                  label = "CANCER_TYPE_DETAILED",
                  choices = c("other", cancer_type_detailed),
                  selected = 1),))
  } else if(colname == "TUMOR_TISSUE_SITE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,"TUMOR_TISSUE_SITE"),
                  label = "TUMOR_TISSUE_SITE",
                  choices = c("other", tumor_tissue_site),
                  selected = 1),))
  } else if(colname == "ONCOTREE_CODE"){
    fluidRow(column(
      width = 8,
      selectInput(inputId=paste0(id_prefix,"ONCOTREE_CODE"),
                  label = "ONCOTREE_CODE",
                  choices = c("", oncotree_code),
                  selected = 1),
    ))
  }
}

#' Updates UI-widgets for specific columns of oncotree entries
#'
#' @param session Shiny session
#' @param row_last_clicked the index of the row last clicked in the oncotree_table
#' @param mode determines the inputId prefix of the UI-widget
#' @return nothing to return
updateOncotreeUIwidgets <- function(session, row_last_clicked, mode = c("add", "edit")){
  mode <- match.arg(mode)

  if(mode == "add"){
    id_prefix <- "addSampleInput_"
  } else if (mode == "edit"){
    id_prefix <- "editSampleInput_"
  }

  updateSelectInput(session, paste0(id_prefix,"CANCER_TYPE"),
                    choices = c("other", cancer_type),
                    selected = oncotree$mainType[row_last_clicked])
  updateSelectInput(session, paste0(id_prefix,"CANCER_TYPE_DETAILED"),
                    label = "CANCER_TYPE_DETAILED",
                    choices = c("other", cancer_type_detailed),
                    selected = oncotree$name[row_last_clicked])
  updateSelectInput(session, paste0(id_prefix,"TUMOR_TISSUE_SITE"),
                    label = "TUMOR_TISSUE_SITE",
                    choices = c("other", tumor_tissue_site),
                    selected = oncotree$tissue[row_last_clicked])
  updateSelectInput(session, paste0(id_prefix,"ONCOTREE_CODE"),
                    label = "ONCOTREE_CODE",
                    choices = c("", oncotree_code),
                    selected = oncotree$code[row_last_clicked])
}

#' Add empty column to a data.frame, if column does not exist in the data.frame
#'
#' @param data data.frame
#' @param cname column name
#' @return data.frame
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]

  if(length(add)!=0) data[add] <- ""
  return(data)
}

#' tbd
#'
#' @param colname tbd
#' @param mode tbd
#' @param data tbd
#' @param selected_row tbd
#' @param patientIDs tbd
#' @return tbd
generateTimelineUI <- function(colname, mode = c("add", "edit"), data = NULL, selected_row = NULL, patientIDs = NULL){
  mode <- match.arg(mode)


  fluidRow(column(
    width = 8,
    textInput(inputId=colname,
              label = colname,
              value = patientIDs),))
}

#' Convert the cBioPortal sample- and patient-data file format into a data.frame
#'
#' This function takes a file object (from read.table), removes the # symbol,
#' sets the 5th row as the column names of the data.frame
#' and removes the rows containing the priority, data type and column name.
#' use read.table as follows: \code{read.table(file, sep='\t', colClasses = 'character', comment.char = '')}
#' @param data The data.frame of a cBioPortal sample/patient data file
#' @return data.frame
cBioPortalToDataFrame <- function(data){
  data$V1 <- sub(pattern="^#", replacement="", x=data$V1)
  colnames(data) <- data[5,]
  data <- data[-c(3,4,5),]
  return(data)
}

#' Get Sample IDs associated with Patient IDs from the data_clinical_sample.txt file
#'
#' @param file_path A character string.
#' @param patIDs A character string.
#' @return vector with
getSampleIDs <- function(file_path, patIDs){
  if (file.exists(file_path)){
    # read data file
    whole_data <- read.table(file_path, sep="\t", colClasses = "character", comment.char = "")
    whole_data <- cBioPortalToDataFrame(whole_data)
    associatedSampleIDs <- whole_data[whole_data$PATIENT_ID %in% patIDs, "SAMPLE_ID"]
    if(length(associatedSampleIDs)==0) return(NULL) else return(associatedSampleIDs)
  } else {
    return(NULL)
  }
}

#' Import patient data into current study data.frames
#'
#' @param mode tbd
#' @param file_name tbd
#' @param file_path tbd
#' @param patIDs tbd
#' @param data tbd
#' @param associatedSampleIDs tbd
#' @return a data.frame
importPatientData <- function(mode=c("patient", "sample", "mutations", "timelines"), file_name, file_path, patIDs, data, associatedSampleIDs = NULL){
  if (file.exists(file_path)){
    # read data file
    whole_data <- NULL
    if(mode=="patient"|mode=="sample"){
      whole_data <- read.table(file_path, sep="\t", colClasses = "character", comment.char = "")
      whole_data <- cBioPortalToDataFrame(whole_data)
    } else if(mode=="mutations"|mode=="timelines"){
      whole_data <- as.data.frame(vroom::vroom(file_path, delim = "\t"))
    }

    # extract rows
    if(mode=="patient"|mode=="sample"| mode=="timelines"){
      extracted_data <- whole_data[whole_data$PATIENT_ID %in% patIDs, ]
      emptyColNames <- setdiff(colnames(extracted_data), colnames(data))
    } else if(mode=="mutations"){
      if(is.null(associatedSampleIDs)){
        return(data)
      } else {
        extracted_data <- whole_data[whole_data$Tumor_Sample_Barcode %in% associatedSampleIDs, ]
      }
    }

    # add rows to table
    if(nrow(extracted_data)!=0){
      data <- dplyr::bind_rows(data, extracted_data)
    }

    # add missing short- and long column names
    if(mode=="patient"|mode=="sample"){
      if(length(emptyColNames)!=0){
        for (emptyCol in emptyColNames){
          data[1, emptyCol] <- whole_data[1, emptyCol]
          data[2, emptyCol] <- whole_data[2, emptyCol]
        }
      }
    }

    showNotification(paste0("Successfully imported data from ",file_name, "!"), type="message", duration = NULL)
    return(data)
  } else {
    showNotification(paste0("File ",file_name," not found in study."), type="warning", duration = NULL)
    return(data)
  }
}

#' Write a line to a logfile containing the date, time, username (from Shinyproxy), and the name of the modified file.
#'
#' @param output directory, where the logfile should be saved
#' @param modified_file Name of the modified file
#' @param log_filename Name of the logfile
#' @return Nothing to return
writeLogfile <- function(outdir, modified_file, log_filename="cbpManager_logfile.txt"){
  userName <- Sys.getenv("SHINYPROXY_USERNAME")
  if(userName=="") userName <- "NO_USERNAME"
  timepoint <- Sys.time()
  log <- paste0(timepoint, "   User: ", userName, " modified file '", modified_file, "'")
  write(log, file=file.path(outdir, log_filename), append=TRUE)
}

