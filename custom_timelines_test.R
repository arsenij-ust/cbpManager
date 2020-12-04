library(shiny)
library(shinydashboard)
library(magrittr)

# add column ---------------------------------------------------------------
add_columnUI <- function(id, label = "Add column") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  actionButton(ns("AddColumn"), label, icon=icon("plus-sign",lib="glyphicon"))

}
add_columnServer <- function(input, output, session, data){

  params <- reactiveValues(df = NULL)

  observeEvent(input$AddColumn,{
    colname <- paste0("V.",ncol(data())+1)
    data() %>% dplyr::mutate(!!(colname) := "") -> params$df
  })

  return(reactive({params$df}))

}


ui <- fluidPage(
  fluidRow(
    width = 12,
    box(
      width = 12,
      title="Add & Edit custom timeline tracks",
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        textInput("customTrackID", label = NULL, width = "400px", placeholder = "Name of timeline track")),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        actionButton("addTrack", "Add track")),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        uiOutput("selectTrackUI")),
      div(
        style="display: inline-block;vertical-align:top; width: 200px;",
        actionButton("editTrack", "Edit track", class = "btn-success")),
      uiOutput("customTracksUI")
    )
  )
)

server <- shinyServer(function(input, output) {
  # create reactive list
  loadedData <- reactiveValues()

  # read sample data
  loadedData[["custom_1"]] <- read.table("custom_timeline_1.txt", sep = "\t", header = TRUE)
  loadedData[["custom_2"]] <- read.table("custom_timeline_2.txt", sep = "\t", header = TRUE)

  # minimal data.frame of a timeline track
  min_timeline_df <- data.frame(
    PATIENT_ID = character(),
    START_DATE = numeric(),
    STOP_DATE = numeric(),
    EVENT_TYPE = character()
  )

  # get custom timeline names from the reactive list
  data_names <- reactive({
    names(loadedData)
  })

  # add new custom timeline to reactive list
  observeEvent(input$addTrack, {
    #check if track name already exists
    ID <- input$customTrackID
    if (ID %in% data_names()){
      showNotification("Name for timeline already exists",
                       type = "error",
                       duration = NULL)
      # add track to the reactive study object
    } else {
      loadedData[[ID]] <- min_timeline_df
    }
  })

  # UI for reactive drop-down widget
  output$selectTrackUI <- renderUI({
    selectInput("selectTrack", width = "400px", label = NULL, choices = data_names())
  })

  # render table of selected data
  observeEvent(input$editTrack, {
    selectedTrack <- input$selectTrack
    print(selectedTrack)

    output$customTracksUI <- renderUI({
      tagList(
        add_columnUI("customTimeline"),
        br(),
        br(),
        DT::DTOutput("customTimeline")
      )
    })

    # Data table output
    output$customTimeline <- DT::renderDT({
      print(loadedData[[selectedTrack]])
      DT::datatable(
        loadedData[[selectedTrack]],
        selection = "single",
        rownames = F
      )
    })

    custom_addCol <- callModule(
      module = add_columnServer,
      id = "customTimeline",
      data = reactive(loadedData[[selectedTrack]])
    )
    print(custom_addCol())
    if(!is.null(custom_addCol())){
      loadedData[[selectedTrack]] <- custom_addCol()
    }

  })

})

shinyApp(ui = ui, server = server)
