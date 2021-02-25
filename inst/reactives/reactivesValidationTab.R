runValidation <- eventReactive(input$runValidation, {
  req(loadedData$studyID)
  exitCode <- validateStudy()
  if (exitCode == 0) {
    return(p("Validation of study succeeded.", class = "valid"))
  } else if (exitCode == 1) {
    return(p("Validation of study failed.", class = "invalid"))
  } else if (exitCode == 2) {
    return(p(
      "Validation of study not performed as problems occurred.",
      class = "invalid"
    ))
  } else if (exitCode == 3) {
    return(p("Validation of study succeeded with warnings.", class = "invalid"))
  }
})

validateStudy <- function() {
  proc <- basilisk::basiliskStart(env_validation)
  on.exit(basilisk::basiliskStop(proc))

  validationOutput <- basiliskRun(proc, function() {
    importerPath <- system.file("python", package = "cbpManager")
    studyDir <- file.path(study_dir, loadedData$studyID)
    outfile <-
      file.path(study_dir, loadedData$studyID, "validated_study.html")

    reticulate::source_python(system.file("python", "validateDataWrapper.py", package = "cbpManager"))
    executeScript(importerPath, studyDir, outfile)

  })
  validationOutput
}

output$validateUI <- renderUI({
  runValidation()

  outfile <-
    file.path(study_dir, loadedData$studyID, "validated_study.html")

  if (file.exists(outfile)) {
    includeHTML(outfile)
  }
})

output$downloadValidation <- downloadHandler(
  filename <- function() {
    paste("study_validation", "html", sep=".")
  },

  content <- function(file) {
    outfile <-
      file.path(study_dir, loadedData$studyID, "validated_study.html")
    if (file.exists(outfile)) {
      file.copy("study_validation.html", file)
    }
  }
)
