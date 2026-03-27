# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,bs4Dash,DT,readr,dplyr,tidyverse,ggplot2,
               plotly,lubridate,bslib,scales)

# set up modules
source("modules/eda_module.R")
source("modules/clustering_module.R")
source("modules/model_module.R")


ui <- dashboardPage(
  dark = FALSE,
  fullscreen = FALSE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "A Chicken Rice Story",
      color = "teal"
    ),
    skin = "light"
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    
#     Left Sidebar
    sidebarMenu(
      id = "tabs",
      menuItem("Dataset Overview", tabName = "overview", icon = icon("database")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Clustering", tabName = "clustering", icon = icon("object-group")),
      menuItem("Model Experimentation", tabName = "model", icon = icon("flask")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("balance-scale"))
    )
  ),
  
  body = dashboardBody(
    tags$head(
#       add softer cards and rounder
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6fb !important;
        }
        .card {
          border-radius: 12px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        }
        .small-box {
          border-radius: 12px;
        }
        .main-sidebar {
          background-color: #f8f9fc !important;
        }
        .btn-primary {
          background-color: #5B8FF9;
          border-color: #5B8FF9;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          bs4Card(
            title = "Upload Dataset",
            width = 4,
            status = "teal",
            solidHeader = FALSE,
            
            fileInput(
              inputId = "file_upload",
              label = "Upload CSV file",
              accept = c(".csv")
            ),
            
            checkboxInput("header", "Header", value = TRUE),
            selectInput("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
            
            br(),
            verbatimTextOutput("file_status")
          ),
          
          bs4ValueBoxOutput("n_rows", width = 2),
          bs4ValueBoxOutput("n_cols", width = 2),
          bs4ValueBoxOutput("file_name_box", width = 4)
        ),
        
        fluidRow(
          bs4Card(
            title = "Dataset Preview",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            DTOutput("data_preview")
          )
        )
      ),
      
      tabItem(
        tabName = "eda",
        eda_ui("eda_1")
      ),
      
      tabItem(
        tabName = "clustering",
        clustering_ui("cluster_1")
      ),
      
      tabItem(
        tabName = "model",
        model_ui("model_1")
      ),
      
      tabItem(
        tabName = "comparison",
        fluidRow(
          bs4Card(
            title = "Model Comparison",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            p("This section can be added later.")
          )
        )
      )
    )
  )
)

##### Server
server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file_upload)
    
    read.csv(
      file = input$file_upload$datapath,
      header = input$header,
      sep = input$sep
    )
  })
  
  output$file_status <- renderText({
    req(input$file_upload)
    paste("Uploaded file:", input$file_upload$name)
  })
  
  output$n_rows <- renderbs4ValueBox({
    req(uploaded_data())
    bs4ValueBox(
      value = nrow(uploaded_data()),
      subtitle = "Rows",
      color = "teal",
      icon = icon("table")
    )
  })
  
  output$n_cols <- renderbs4ValueBox({
    req(uploaded_data())
    bs4ValueBox(
      value = ncol(uploaded_data()),
      subtitle = "Columns",
      color = "teal",
      icon = icon("columns")
    )
  })
  
  output$file_name_box <- renderbs4ValueBox({
    req(input$file_upload)
    bs4ValueBox(
      value = input$file_upload$name,
      subtitle = "File Name",
      color = "teal",
      icon = icon("file-csv")
    )
  })
  
  output$data_preview <- renderDT({
    req(uploaded_data())
    datatable(
      head(uploaded_data(), 20),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  eda_server("eda_1", data = uploaded_data)
  clustering_server("cluster_1", data = uploaded_data)
  model_server("model_1", data = uploaded_data)
}

shinyApp(ui, server)