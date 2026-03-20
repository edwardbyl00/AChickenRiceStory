clustering_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Clustering Module"),
    p("This is the clustering section."),
    sliderInput(
      inputId = ns("k"),
      label = "Select number of clusters",
      min = 2,
      max = 10,
      value = 3
    ),
    textOutput(ns("k_value"))
  )
}

clustering_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$k_value <- renderText({
      paste("Selected number of clusters:", input$k)
    })
    
  })
}
