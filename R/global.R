# source files
source(system.file("tabs", "studyTab.R", package = "cbpManager"), local=TRUE)
source(system.file("tabs", "patientTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "sampleTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "mutationsTab.R", package = "cbpManager"), local = TRUE)
source(system.file("reactives", "reactiveDynamicTable.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "timelineTab.R", package = "cbpManager"), local = TRUE)
source(system.file("tabs", "validationTab.R", package = "cbpManager"), local = TRUE)

# set max file size for MAF-file upload - 10 MB
options(shiny.maxRequestSize = 10 * 1024^2)

