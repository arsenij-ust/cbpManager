# Triggers the validation by clicking the "Validate" button
validation <- eventReactive(input$runValidation, {
  req(loadedData$studyID)
  validateStudy()
})

# Install and use the conda environment managed by basilisk
validateStudy <- function() {
  proc <- basiliskStart(env_validation)
  on.exit(basiliskStop(proc))

  # Execute the validateDataWrapper.py script
  validationOutput <- basiliskRun(proc, function() {
    importerPath <- system.file("python", package = "cbpManager")
    studyDir <- file.path(study_dir, loadedData$studyID)
    outfile <-
      file.path(study_dir, loadedData$studyID, "validated_study.html")

    source_python(system.file("python", "validateDataWrapper.py", package = "cbpManager"))
    executeScript(importerPath, studyDir, outfile)

  })
  validationOutput
}

# Visualize the html report and the download button
output$validateUI <- renderUI({
  exitCode <- validation()
  if (exitCode == 0) {
    print("Validation of study succeeded.")
  } else if (exitCode == 1) {
    print("Validation of study failed.")
  } else if (exitCode == 2) {
    print("Validation of study not performed as problems occurred.")
  } else if (exitCode == 3) {
    print("Validation of study succeeded with warnings.")
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
