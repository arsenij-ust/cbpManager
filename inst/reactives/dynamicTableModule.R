dynamicTableUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    add_rowUI(ns("customTimeline")),
    edit_rowUI(ns("customTimeline")),
    delete_rowUI(ns("customTimeline")),
    add_columnUI(ns("customTimeline")),
    delete_columnUI(ns("customTimeline")),
    save_timelineUI(ns("customTimeline")),
    br(),
    br(),
    DT::DTOutput(ns("customTable"))
  )
}

dynamicTableServer <- function(input, output, session, data){

}
