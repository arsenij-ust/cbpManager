# dynamicTableUI <- function(id) {
#   # `NS(id)` returns a namespace function, which was save as `ns` and will
#   # invoke later.
#   ns <- NS(id)
#   tagList(
#     add_rowUI(ns("innerModule")),
#     edit_rowUI(ns("innerModule")),
#     delete_rowUI(ns("innerModule")),
#     add_columnUI(ns("innerModule")),
#     delete_columnUI(ns("innerModule")),
#     save_timelineUI(ns("innerModule")),
#     br(),
#     br(),
#     DT::DTOutput(ns("innerModule"))
#   )
# }
#
# dynamicTableServer <- function(input, output, session, data){
#   inner_editRow <- callModule(
#     module = edit_rowServer,
#     id = "innerModule",
#     data = reactive(loadedData$data_timeline_surgery),
#     patient_ids = reactive(patient_id_list$ids),
#     dates_first_diagnosis = reactive(loadedData$dates_first_diagnosis),
#     selected_row = reactive(input$surgeryTable_rows_selected),
#     mode = "timepoint"
#   )
#   observe({
#     loadedData$data_table <- inner_editRow()
#   })
#
#   # delete surgery entry ---------------------------------------------------------------
#   surgery_delRow <- callModule(
#     module = delete_rowServer,
#     id = "Surgery",
#     data = reactive(loadedData$data_timeline_surgery),
#     selected_row = reactive(input$surgeryTable_rows_selected)
#   )
#   observe({
#     loadedData$data_timeline_surgery <- surgery_delRow()
#   })
#
#   # delete surgery column ---------------------------------------------------------------
#   surgery_delCol <- callModule(
#     module = delete_columnServer,
#     id = "Surgery",
#     data = reactive(loadedData$data_timeline_surgery),
#     exclude = c("PATIENT_ID", "START_DATE", "STOP_DATE", "EVENT_TYPE")
#   )
#   observe({
#     loadedData$data_timeline_surgery <- surgery_delCol()
#   })
#
#   # add surgery column ---------------------------------------------------------------
#   surgery_addCol <- callModule(
#     module = add_columnServer,
#     id = "Surgery",
#     data = reactive(loadedData$data_timeline_surgery)
#   )
#   observe({
#     loadedData$data_timeline_surgery <- surgery_addCol()
#   })
#
#   # save surgery data ---------------------------------------------------------------
#   callModule(
#     module = save_timelineServer,
#     id = "Surgery",
#     study_id = reactive(loadedData$studyID),
#     data = reactive(loadedData$data_timeline_surgery)
#   )
# }
