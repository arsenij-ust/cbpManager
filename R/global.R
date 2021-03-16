# source files
source(system.file("tabs", "studyTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "patientTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "sampleTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "mutationsTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "timelineTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "resourceTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "validationTab.R", package = "cbpManager"), local = TRUE)

# resolve 'no visible binding NOTEs'
utils::globalVariables(
  c(
    "cancer_type",
    "cancer_type_detailed",
    "tumor_tissue_site",
    "oncotree_code",
    "oncotree",
    "study_dir",
    "cbpManager.options",
    ":=",
    "logDir"
  )
)
