library(shiny)

ui <- fluidPage(
  titlePanel("Retail Analytics App"),
  tabsetPanel(
    tabPanel("EDA"),
    tabPanel("Clustering"),
    tabPanel("Prediction")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)