clustering_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      bs4Card(
        title = "Clustering Controls",
        width = 4,
        status = "primary",
        solidHeader = FALSE,
        
        sliderInput(
          inputId = ns("k"),
          label = "Number of clusters",
          min = 2,
          max = 10,
          value = 3
        ),
        
        uiOutput(ns("cluster_vars_ui"))
      ),
      
      bs4ValueBoxOutput(ns("cluster_box_1"), width = 4),
      bs4ValueBoxOutput(ns("cluster_box_2"), width = 4)
    ),
    
    fluidRow(
      bs4Card(
        title = "Cluster Plot",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        plotOutput(ns("cluster_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "Cluster Size Summary",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        tableOutput(ns("cluster_table"))
      )
    )
  )
}

clustering_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    numeric_cols <- reactive({
      req(data())
      names(data())[sapply(data(), is.numeric)]
    })
    
    output$cluster_vars_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) >= 2)
      
      selectInput(
        inputId = session$ns("cluster_vars"),
        label = "Select Numeric Variables",
        choices = numeric_cols(),
        selected = numeric_cols()[1:min(2, length(numeric_cols()))],
        multiple = TRUE
      )
    })
    
    cluster_data <- reactive({
      req(data())
      req(input$cluster_vars)
      req(length(input$cluster_vars) >= 2)
      
      df <- data()[, input$cluster_vars, drop = FALSE]
      df <- na.omit(df)
      scale(df)
    })
    
    cluster_result <- reactive({
      req(cluster_data())
      kmeans(cluster_data(), centers = input$k)
    })
    
    output$cluster_box_1 <- renderbs4ValueBox({
      bs4ValueBox(
        value = input$k,
        subtitle = "Selected K",
        status = "primary",
        icon = icon("object-group")
      )
    })
    
    output$cluster_box_2 <- renderbs4ValueBox({
      req(cluster_data())
      bs4ValueBox(
        value = nrow(cluster_data()),
        subtitle = "Rows Used",
        status = "info",
        icon = icon("database")
      )
    })
    
    output$cluster_plot <- renderPlot({
      req(cluster_data())
      req(cluster_result())
      
      plot_data <- as.data.frame(cluster_data())
      
      plot(
        x = plot_data[[1]],
        y = plot_data[[2]],
        col = cluster_result()$cluster,
        pch = 19,
        main = paste("Cluster Plot with K =", input$k),
        xlab = names(plot_data)[1],
        ylab = names(plot_data)[2]
      )
      
      points(
        cluster_result()$centers[, 1],
        cluster_result()$centers[, 2],
        pch = 8,
        cex = 2,
        lwd = 2,
        col = 1:input$k
      )
    })
    
    output$cluster_table <- renderTable({
      req(cluster_result())
      as.data.frame(table(cluster_result()$cluster))
    })
    
  })
}
