# Triggers the validation by clicking the "Validate" button
validation <- eventReactive(input$runValidation, {
  if(is.null(loadedData$studyID)){
    showNotification(
      "Please select and load a study in the 'Study' tab.",
      type = "error",
      duration = NULL
    )
  }
  req(loadedData$studyID)
  validateStudy()
})

# Install and use the conda environment managed by basilisk
validateStudy <- function() {
  proc <- basiliskStart(env_validation)
  on.exit(basiliskStop(proc))

  # Execute the validateDataWrapper.py script
  validationOutput <- basiliskRun(proc, function() {
    # check if conda environment
    packages_df <- basilisk::listPackages(env_validation)
    if (!all(c("Jinja2", "requests", "PyYAML") %in% packages_df$package)) {
      return(4)
    }else{
      importerPath <- system.file("python", package = "cbpManager")
      studyDir <- file.path(study_dir, loadedData$studyID)
      outfile <-
        file.path(study_dir, loadedData$studyID, "validated_study.html")

      source_python(system.file("python", "validateDataWrapper.py", package = "cbpManager"))
      executeScript(importerPath, studyDir, outfile)
    }

  })
  validationOutput
}

# Visualize the html report and the download button
output$validateUI <- renderUI({
  exitCode <- validation()
  if (exitCode == 0) {
    message("Validation of study succeeded.")
  } else if (exitCode == 1) {
    message("Validation of study failed.")
  } else if (exitCode == 2) {
    message("Validation of study not performed as problems occurred.")
  } else if (exitCode == 3) {
    message("Validation of study succeeded with warnings.")
  } else if (exitCode == 4) {
    warning("Some problems occured during the installation of the conda environment.
One or more of the necessary packages were not installed.
Please try reinstalling cbpManager and basilisk or contact the support at https://github.com/arsenij-ust/cbpManager/issues")
    showNotification(
      "Some problems occured during the installation of the conda environment.
One or more of the necessary packages were not installed.
Please try reinstalling cbpManager and basilisk or contact the support at https://github.com/arsenij-ust/cbpManager/issues.",
      type = "error",
      duration = NULL
    )
  }

  outfile <-
    file.path(study_dir, loadedData$studyID, "validated_study_cropped.html")

  if (input$runValidation != 0 & file.exists(outfile)) {
    tagList(
      downloadButton("downloadValidation", "Download"),
      includeHTML(outfile)
    )
  }
})

# Handle download
output$downloadValidation <- downloadHandler(
  filename <- function() {
    paste("study_validation-",loadedData$studyID,"-", Sys.Date(), ".html", sep="")
  },
  content <- function(file) {
    outfile <-
      file.path(study_dir, loadedData$studyID, "validated_study.html")
    if(input$runValidation != 0 & file.exists(outfile)){
      file.copy(outfile, file)
    } else {
      return()
    }

  }
)
