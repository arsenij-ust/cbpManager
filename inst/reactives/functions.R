# function for define the datatype required in cBioPortal file format
findDatatype <- function(df){
  if(!any(is.na(as.numeric(df)))){
    return("NUMBER")
  } else if(!any(is.na(as.logical(df)))) {
    return("BOOLEAN")
  } else {
    return("STRING")
  }
}
# function to convert the data.frame to the appropriate file format for cBioPortal
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
# function to check if input is in the apropriate date format
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

# function to check the input of dates
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

# patientSampleCols <- c("CANCER_TYPE", "CANCER_TYPE_DETAILED","KNOWN_MOLECULAR_CLASSIFIER","GLEASON_SCORE","HISTOLOGY","TUMOR_STAGE_2009","TUMOR_GRADE","ETS_RAF_SPINK1_STATUS","TMPRSS2_ERG_FUSION_STATUS","ERG_FUSION_ACGH","SERUM_PSA","DRIVER_MUTATIONS")

#' Sanitize component names
#'
#' This function takes a character string, replaces spaces by underscores and runs make.names.
#' @param x A character string.
#' @return A sanitized string.
.create_name <- function(x) {
  . = NULL # workaround for R CMD check note: no visible binding for global variable '.'
  x %>% toupper %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names %>% return
}

#' Generate UI input widget
#'
#' @param colname A character string - the name of the column, that will be the label of the input
#' @param mode "add" or "edit" - wether to use existing values or not
#' @param tab "Patient", "Sample" - The used tab; sets the html id prefix of the input
#' @param data A data.frame.
#' @param selected_row A number indicating the row number of the selected row in the data.frame.
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

#' tbd
#'
#' @param colname tbd
#' @param mode tbd
#' @return tbd
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

#' tbd
#'
#' @param session tbd
#' @param row_last_clicked tbd
#' @param mode tbd
#' @return tbd
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

#' tbd
#'
#' @param data tbd
#' @param cname tbd
#' @return tbd
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]

  if(length(add)!=0) data[add] <- ""
  return(data)
}

#' tbd
#'
#' @param data tbd
#' @param selected_row tbd
#' @param patIDs tbd
#' @param timeline tbd
#' @param mode tbd
#' @return tbd
timelineModal <- function(data, selected_row = NULL, patIDs, timeline = c("treatment", "surgery", "status"), mode = c("add","edit")){
  timeline <- match.arg(timeline)
  mode <- match.arg(mode)

  if(timeline == "treatment"){
    if(mode == "add"){
      selectedPatId <- selectedTreatmentType <- selectedTreatmentSubtype <- 1
      selectedTreatmentAgent <- NULL
    } else if (mode == "edit"){
      selectedPatId <- data[selected_row, "PATIENT_ID"]
      selectedTreatmentType <- data[selected_row, "TREATMENT_TYPE"]
      selectedTreatmentSubtype <- data[selected_row, "SUBTYPE"]
      selectedTreatmentAgent <- data[selected_row, "AGENT"]
    }

    fluidRow(
    column(
      width = 8,
      selectInput(
        "treatmentPatientID",
        label = "Select the Patient ID",
        choices = c("", unique(patIDs[which(!is.na(patIDs))])),
        selected = selectedPatId
      ),
      dateRangeInput("treatmentRange", "Start and End of treatment"),
      selectInput("treatmentType", "Treatment type", choices = c("Medical Therapy", "Radiation Therapy"), selected = selectedTreatmentType),
      selectInput("treatmentSubtype", "Treatment subtype", choices = c("", "Chemotherapy", "Hormone Therapy", "Targeted Therapy", "WPRT", "IVRT"), selected = selectedTreatmentSubtype),
      textInput("treatmentAgent", "Agent", placeholder = "e.g. Med_X", value = selectedTreatmentAgent)
    ))
  }

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
  #ns <- NS(id)
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
#' use read.table as follows: read.table(file, sep="\t", colClasses = "character", comment.char = "")
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
    } else if(mode=="mutations"|mode=="timelines").{
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


