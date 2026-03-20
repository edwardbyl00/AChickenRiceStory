model_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Predictive Model Module"),
    p("This is the predictive model section."),
    numericInput(
      inputId = ns("num_input"),
      label = "Enter a value",
      value = 10
    ),
    textOutput(ns("model_output"))
  )
}

model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$model_output <- renderText({
      paste("Input value is:", input$num_input)
    })
    
  })
}
