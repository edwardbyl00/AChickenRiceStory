library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(ggplot2)

ui <- dashboardPage(
  dark = FALSE,
  fullscreen = TRUE,
  
  header = dashboardHeader(
    title = "AChickenRiceStory"
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    title = "Dataset Overview",
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("Dataset Overview", tabName = "overview", icon = icon("database")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Decision Tree Model", tabName = "tree", icon = icon("play-circle")),
      menuItem("Model Experimentation", tabName = "experiment", icon = icon("check-square")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("sliders-h"))
    )
  ),
  
  body = dashboardBody(
    fluidRow(
      column(
        width = 12,
        h2("Select Regression Model")
      )
    
    ),
    
    tabItems(
      tabItem(
        tabName = "experiment",
        
        fluidRow(
          column(
            width = 4,
            bs4Card(
              width = 12,
              title = "Select Model Parameters",
              plotlyOutput("feature_plot_bottom"),
              
              selectInput(
                "model_type",
                "Select Model Parameters",
                choices = c("Random Forest", "Linear Regression", "XGBoost")
              ),
              
              sliderInput("n_trees", "Number of Trees", min = 10, max = 500, value = 100),
              sliderInput("min_samples", "Minimum Samples per Node", min = 1, max = 20, value = 5),
              sliderInput("max_depth", "Maximum Depth", min = 1, max = 20, value = 6),
              sliderInput("n_predictors", "Number of Predictors", min = 1, max = 20, value = 5),
              
              actionButton("run_model", "Run Experiment", class = "btn-primary")
            )
          ),
          
          column(
            width = 8,
            fluidRow(
              valueBoxOutput("r2_box", width = 4),
              valueBoxOutput("mse_box", width = 4),
              valueBoxOutput("rmse_box", width = 4)
            ),
            
            fluidRow(
              column(
                width = 7,
                box(
                  width = 12,
                  title = "Evaluation Metrics",
                  DTOutput("metrics_table")
                )
              ),
              column(
                width = 5,
                box(
                  width = 12,
                  title = "Feature Importance",
                  plotlyOutput("feature_plot_top", height = "250px")
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              title = "Feature Importance",
              plotlyOutput("feature_plot_bottom", height = "300px")
            )
          ),
          
          column(
            width = 4,
            box(
              width = 12,
              title = "Correlation Heatmap",
              plotlyOutput("heatmap_plot", height = "300px")
            )
          ),
          
          column(
            width = 4,
            box(
              width = 12,
              title = "Predicted vs Actual CLV",
              plotlyOutput("scatter_plot", height = "300px")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$r2_box <- renderValueBox({
    valueBox(
      value = "0.97",
      subtitle = "R²",
      icon = icon("chart-line"),
      status = "primary"
    )
  })
  
  output$mse_box <- renderValueBox({
    valueBox(
      value = "5,700,000,000",
      subtitle = "MSE",
      icon = icon("calculator"),
      status = "primary"
    )
  })
  
  output$rmse_box <- renderValueBox({
    valueBox(
      value = "75,412",
      subtitle = "RMSE",
      icon = icon("square-root-alt"),
      status = "primary"
    )
  })
  
  output$metrics_table <- renderDT({
    datatable(
      data.frame(
        Predicted = c(20000, 22000, 20500),
        Actual = c(21000, 21500, 19800),
        Residual = c(-1000, 500, 700)
      ),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$feature_plot_top <- renderPlotly({
    df <- data.frame(
      feature = c("Total", "Tenure", "Time", "Income", "Migrations"),
      importance = c(95, 85, 70, 62, 58)
    )
    
    p <- ggplot(df, aes(x = reorder(feature, importance), y = importance)) +
      geom_col(fill = "#5B8FF9") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$feature_plot_bottom <- renderPlotly({
    df <- data.frame(
      feature = c("Transactions", "Monthly Tenure", "Income", "Customer Tenure", "Reserve"),
      importance = c(100, 88, 76, 55, 43)
    )
    
    p <- ggplot(df, aes(x = reorder(feature, importance), y = importance)) +
      geom_col(fill = "#5B8FF9") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$heatmap_plot <- renderPlotly({
    mat <- matrix(runif(100), nrow = 10)
    plot_ly(
      x = 1:10,
      y = 1:10,
      z = mat,
      type = "heatmap"
    )
  })
  
  output$scatter_plot <- renderPlotly({
    df <- data.frame(
      actual = rnorm(200, mean = 100000, sd = 30000),
      predicted = rnorm(200, mean = 100000, sd = 25000)
    )
    
    p <- ggplot(df, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6, color = "#3B82F6") +
      theme_minimal() +
      labs(x = "Actual Customer Lifetime Value", y = "Predicted Value")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)