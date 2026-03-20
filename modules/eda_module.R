eda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("EDA Module"),
    p("This is the EDA section."),
    selectInput(
      inputId = ns("variable"),
      label = "Choose a variable",
      choices = c("Sales", "Profit", "Quantity")
    ),
    textOutput(ns("selected_var"))
  )
}

eda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$selected_var <- renderText({
      paste("You selected:", input$variable)
    })
    
  })
}
