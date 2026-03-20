eda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      bs4Card(
        title = "EDA Controls",
        width = 4,
        status = "primary",
        solidHeader = FALSE,
        
        uiOutput(ns("var_select_ui")),
        
        sliderInput(
          inputId = ns("bins"),
          label = "Number of bins",
          min = 5,
          max = 50,
          value = 20
        )
      ),
      
      bs4ValueBoxOutput(ns("summary_box_1"), width = 4),
      bs4ValueBoxOutput(ns("summary_box_2"), width = 4)
    ),
    
    fluidRow(
      bs4Card(
        title = "Distribution Plot",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        plotOutput(ns("hist_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "Scatter Plot",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        uiOutput(ns("scatter_x_ui")),
        uiOutput(ns("scatter_y_ui")),
        plotOutput(ns("scatter_plot"), height = "300px")
      )
    )
  )
}

eda_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    numeric_cols <- reactive({
      req(data())
      names(data())[sapply(data(), is.numeric)]
    })
    
    output$var_select_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) > 0)
      
      selectInput(
        inputId = session$ns("var"),
        label = "Select Numeric Variable",
        choices = numeric_cols(),
        selected = numeric_cols()[1]
      )
    })
    
    output$scatter_x_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) > 0)
      
      selectInput(
        inputId = session$ns("x_var"),
        label = "Scatter X Variable",
        choices = numeric_cols(),
        selected = numeric_cols()[1]
      )
    })
    
    output$scatter_y_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) > 1)
      
      selectInput(
        inputId = session$ns("y_var"),
        label = "Scatter Y Variable",
        choices = numeric_cols(),
        selected = numeric_cols()[2]
      )
    })
    
    output$summary_box_1 <- renderbs4ValueBox({
      req(data())
      bs4ValueBox(
        value = nrow(data()),
        subtitle = "Rows",
        status = "primary",
        icon = icon("table")
      )
    })
    
    output$summary_box_2 <- renderbs4ValueBox({
      req(data())
      bs4ValueBox(
        value = ncol(data()),
        subtitle = "Columns",
        status = "info",
        icon = icon("columns")
      )
    })
    
    output$hist_plot <- renderPlot({
      req(data())
      req(input$var)
      
      hist(
        data()[[input$var]],
        breaks = input$bins,
        main = paste("Distribution of", input$var),
        xlab = input$var,
        col = "lightblue",
        border = "white"
      )
    })
    
    output$scatter_plot <- renderPlot({
      req(data())
      req(input$x_var, input$y_var)
      
      plot(
        x = data()[[input$x_var]],
        y = data()[[input$y_var]],
        main = paste(input$y_var, "vs", input$x_var),
        xlab = input$x_var,
        ylab = input$y_var,
        pch = 19,
        col = "steelblue"
      )
      
      abline(
        lm(data()[[input$y_var]] ~ data()[[input$x_var]]),
        col = "red",
        lty = 2
      )
    })
    
  })
}
