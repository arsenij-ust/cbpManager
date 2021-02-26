library("cbpManager")

sd <- "path/to/studyDir"
ld <- "path/to/workingDir"


test_that("Shiny app is generated", {
  app <- launchApp(returnAppObj = TRUE)

  expect_s3_class(app, "shiny.appobj")
  expect_s3_class(launchApp(studyDir = sd, logDir = ld, returnAppObj = TRUE), "shiny.appobj")

})

