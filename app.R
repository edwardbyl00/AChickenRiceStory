# Increase load filesize
options(shiny.maxRequestSize = 100*1024^2)

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, bs4Dash, DT, readr, dplyr, tidyverse, ggplot2,
               plotly, lubridate, bslib, scales,
               rpart, xgboost, randomForest)

# set up modules
source("modules/eda_module.R")
source("modules/clustering_module.R")
source("modules/Prediction_model_module.R")
source("modules/model_comparison.R")


ui <- dashboardPage(
  dark = FALSE,
  fullscreen = FALSE,
  controlbar = NULL,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$div(
        tags$img(src = "ChickinSights_logo.png", height = "30px"),
        tags$span(" ChickInsights", style = "margin-left:8px; font-weight:600;")
      ),
      color = "teal"
    ),
    skin = "light"
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "lightblue",
    elevation = 3,
    
    #     Left Sidebar
    sidebarMenu(
      id = "tabs",
      menuItem("Data Overview", tabName = "overview", icon = icon("database")),
      menuItem("Data Cleaning", tabName = "cleaning", icon = icon("broom")),
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
          background-color: #20c997 !important;
          border-color: #20c997 !important;
          color: white !important;
        }
        .btn-primary:hover {
          background-color: #17a589 !important;
          border-color: #17a589 !important;
          color: white !important;
        }

        /* clean top navbar */
        .main-header {
          border-bottom: 1px solid #e5e7eb !important;
          box-shadow: none !important;
        }
        .main-header .navbar {
          background-color: #ffffff !important;
          box-shadow: none !important;
          min-height: 60px;
        }

        /* hide stubborn top right controls */
        .main-header .navbar-custom-menu,
        .main-header .navbar-right,
        .main-header .dark-mode-switch,
        .main-header [data-widget='fullscreen'],
        .main-header [data-widget='control-sidebar'] {
          display: none !important;
        }

        /* keep left sidebar toggle only */
        .main-header .nav-link {
          color: #4b5563 !important;
          font-size: 18px;
        }

        /* cleaner page spacing */
        .content-wrapper > .content {
          padding-top: 18px;
        }
      ")),
      
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const selectors = [
            '.main-header .navbar-custom-menu',
            '.main-header .navbar-right',
            '.main-header .dark-mode-switch',
            '.main-header [data-widget=\"fullscreen\"]',
            '.main-header [data-widget=\"control-sidebar\"]'
          ];
          
          selectors.forEach(function(sel) {
            document.querySelectorAll(sel).forEach(function(el) {
              el.remove();
            });
          });
          
          const headerLists = document.querySelectorAll('.main-header .navbar > ul');
          if (headerLists.length > 1) {
            for (let i = 1; i < headerLists.length; i++) {
              headerLists[i].remove();
            }
          }
        });
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "overview",
        
        fluidRow(
          bs4ValueBoxOutput("n_rows", width = 4),
          bs4ValueBoxOutput("n_cols", width = 4),
          bs4ValueBoxOutput("file_name_box", width = 4)
        ),
        
        fluidRow(
          bs4Card(
            title = "Upload Dataset",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            
            fileInput(
              inputId = "file_upload",
              label = "Upload CSV file",
              accept = c(".csv")
            ),
            
            checkboxInput("header", "Header", value = TRUE),
            selectInput(
              "sep",
              "Separator",
              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
              selected = ","
            ),
            
            br(),
            
            selectInput(
              inputId = "saved_file",
              label = "Or select a saved dataset",
              choices = NULL
            ),
            
            actionButton("load_saved", "Load Selected Dataset", class = "btn btn-primary"),
            
            br(), br(),
            verbatimTextOutput("file_status"),
            
            br(), br(),
            actionButton("clear_uploads", "Clear Saved Files", class = "btn btn-danger")
          )
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
      
      # Data cleaning tab
      tabItem(
        tabName = "cleaning",
        
        fluidRow(
          bs4ValueBoxOutput("n_numeric", width = 3),
          bs4ValueBoxOutput("n_character", width = 3),
          bs4ValueBoxOutput("n_missing", width = 3),
          bs4ValueBoxOutput("n_complete", width = 3)
        ),
        
        fluidRow(
          bs4Card(
            title = "Missing Data Warning",
            width = 12,
            status = "warning",
            solidHeader = FALSE,
            htmlOutput("missing_warning")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Convert Column Type",
            width = 6,
            status = "teal",
            solidHeader = FALSE,
            
            tags$div(
              style = "background-color:#eef5ff; padding:10px; border-radius:8px; margin-bottom:10px;",
              # Description for data types
              HTML("
                <b>Column Type Guide</b><br>
                • <b>Numeric</b>: Used for calculations (e.g., sales, price)<br>
                • <b>Character</b>: Free text (e.g., names, IDs)<br>
                • <b>Factor</b>: Categories with limited groups (e.g. gender, product type)<br>
                • <b>Date</b>: Time-based values (e.g., 2023-01-01)
              ")
            ),
            
            uiOutput("convert_column_ui"),
            selectInput(
              "target_type",
              "Convert To",
              choices = c("numeric", "character", "factor", "Date")
            ),
            actionButton("apply_conversion", "Apply Conversion", class = "btn btn-primary"),
            
            br(), br(),
            
            fileInput(
              "type_mapping_file",
              "Upload Column Type Mapping CSV",
              accept = c(".csv")
            ),
            helpText("Fill in 'target_type' only. Leave blank if no change is needed."),
            
            downloadButton("download_type_mapping_template", "Download Mapping Template"),
            
            br(), br(),
            
            actionButton("apply_mapping_conversion", "Apply Mapping File", class = "btn btn-primary")
          ),
          
          bs4Card(
            title = "Data Cleaning Controls",
            width = 6,
            status = "danger",
            solidHeader = FALSE,
            
            uiOutput("drop_columns_ui"),
            actionButton("drop_columns_btn", "Drop Selected Columns", class = "btn btn-danger"),
            
            br(), br(),
            
            checkboxInput("remove_na_rows", "Remove Rows with ANY Missing Values", value = FALSE),
            
            sliderInput(
              "na_threshold",
              "Remove Rows with > % Missing",
              min = 0,
              max = 100,
              value = 50
            ),
            
            actionButton("apply_cleaning", "Apply Row Cleaning", class = "btn btn-warning"),
            br(), br(),
            downloadButton("download_cleaned", "Download Current Dataset")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Dataset Structure",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            DTOutput("data_structure")
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
        comparison_ui("comparison_1")
      )
    )
  )
)

##### Server
server <- function(input, output, session) {
  
  # create upload storage
  dir.create("data/uploads", showWarnings = FALSE, recursive = TRUE)
  
  # reactive dataset
  active_data <- reactiveVal(NULL)
  
  # save models result
  saved_models <- reactiveValues(results = list())
  
  # helper: safer date conversion
  safe_convert_date <- function(x) {
    x_chr <- as.character(x)
    
    out <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
    
    still_na <- is.na(out) & !is.na(x_chr) & x_chr != ""
    if (any(still_na)) {
      out[still_na] <- suppressWarnings(as.Date(x_chr[still_na], format = "%d/%m/%Y"))
    }
    
    still_na <- is.na(out) & !is.na(x_chr) & x_chr != ""
    if (any(still_na)) {
      out[still_na] <- suppressWarnings(as.Date(x_chr[still_na], format = "%m/%d/%Y"))
    }
    
    still_na <- is.na(out) & !is.na(x_chr) & x_chr != ""
    if (any(still_na)) {
      out[still_na] <- suppressWarnings(as.Date(x_chr[still_na], format = "%d-%m-%Y"))
    }
    
    out
  }
  
  # save uploaded file
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    files <- list.files("data/uploads", pattern = "\\.csv$")
    
    if (length(files) >= 5) {
      showNotification(
        "Maximum of 5 uploaded files reached. Please delete existing files before uploading new ones.",
        type = "error"
      )
      return(NULL)
    }
    
    saved_name <- paste0(
      tools::file_path_sans_ext(input$file_upload$name),
      "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
    
    file.copy(
      from = input$file_upload$datapath,
      to = file.path("data/uploads", saved_name),
      overwrite = TRUE
    )
  })
  
  # update dropdown list (SEPARATE observer)
  observe({
    files <- list.files("data/uploads", pattern = "\\.csv$")
    updateSelectInput(session, "saved_file", choices = files)
  })
  
  # Update column choice in upload data
  observe({
    req(active_data())
    updateSelectInput(
      session,
      "dist_column",
      choices = names(active_data())
    )
  })
  
  # load uploaded file into active_data
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    df <- read.csv(
      file = input$file_upload$datapath,
      header = input$header,
      sep = input$sep
    )
    
    active_data(df)
  })
  
  # load selected saved file (SEPARATE observer)
  observeEvent(input$load_saved, {
    req(input$saved_file)
    
    file_path <- file.path("data/uploads", input$saved_file)
    df <- read.csv(file_path)
    
    active_data(df)
  })
  
  #   clear loaded dataset
  observeEvent(input$clear_uploads, {
    files <- list.files("data/uploads", full.names = TRUE)
    
    if (length(files) > 0) {
      file.remove(files)
    }
    
    updateSelectInput(session, "saved_file", choices = character(0))
    
    showNotification("All uploaded files removed.", type = "message")
  })
  
  # file status
  output$file_status <- renderText({
    if (!is.null(input$file_upload)) {
      paste("Uploaded file:", input$file_upload$name)
    } else if (!is.null(input$saved_file) && input$saved_file != "") {
      paste("Loaded saved file:", input$saved_file)
    } else {
      "No dataset loaded yet."
    }
  })
  
  
  # VALUE BOXES (FIXED)
  output$n_rows <- renderbs4ValueBox({
    req(active_data())
    bs4ValueBox(
      value = nrow(active_data()),
      subtitle = "Rows",
      color = "teal",
      icon = icon("table")
    )
  })
  
  output$n_cols <- renderbs4ValueBox({
    req(active_data())
    bs4ValueBox(
      value = ncol(active_data()),
      subtitle = "Columns",
      color = "teal",
      icon = icon("columns")
    )
  })
  
  output$file_name_box <- renderbs4ValueBox({
    file_name <- if (!is.null(input$file_upload)) {
      input$file_upload$name
    } else if (!is.null(input$saved_file) && input$saved_file != "") {
      input$saved_file
    } else {
      "No file loaded"
    }
    
    bs4ValueBox(
      value = file_name,
      subtitle = "File Name",
      color = "teal",
      icon = icon("file-csv")
    )
  })
  
  output$n_numeric <- renderbs4ValueBox({
    req(active_data())
    df <- active_data()
    bs4ValueBox(
      value = sum(sapply(df, is.numeric)),
      subtitle = "Numeric Columns",
      color = "teal",
      icon = icon("hashtag")
    )
  })
  
  output$n_character <- renderbs4ValueBox({
    req(active_data())
    df <- active_data()
    bs4ValueBox(
      value = sum(sapply(df, function(x) is.character(x) || is.factor(x))),
      subtitle = "Text Columns",
      color = "teal",
      icon = icon("font")
    )
  })
  
  # missing value
  output$n_missing <- renderbs4ValueBox({
    req(active_data())
    df <- active_data()
    bs4ValueBox(
      value = sum(is.na(df)),
      subtitle = "Missing Cells",
      color = "teal",
      icon = icon("exclamation-circle")
    )
  })
  
  # character
  output$n_complete <- renderbs4ValueBox({
    req(active_data())
    df <- active_data()
    bs4ValueBox(
      value = sum(complete.cases(df)),
      subtitle = "Complete Rows",
      color = "teal",
      icon = icon("check-circle")
    )
  })
  
  # missing data warning
  output$missing_warning <- renderUI({
    req(active_data())
    df <- active_data()
    
    total_missing <- sum(is.na(df))
    missing_pct <- round((total_missing / (nrow(df) * ncol(df))) * 100, 2)
    
    if (total_missing == 0) {
      tags$div(
        style = "color: green; font-weight: 600;",
        HTML("<i class='fas fa-check-circle'></i> No missing values detected in the dataset.")
      )
    } else {
      tags$div(
        style = "color: #856404; font-weight: 600;",
        HTML(paste0(
          "<i class='fas fa-exclamation-triangle'></i> Warning: ",
          total_missing,
          " missing cells detected (",
          missing_pct,
          "% of the dataset)."
        ))
      )
    }
  })
  
  #   select column
  output$convert_column_ui <- renderUI({
    req(active_data())
    
    selectInput(
      "convert_column",
      "Select Column(s)",
      choices = names(active_data()),
      multiple = TRUE
    )
  })
  
  # drop column selector
  output$drop_columns_ui <- renderUI({
    req(active_data())
    
    selectInput(
      "columns_to_drop",
      "Select Column(s) to Drop",
      choices = names(active_data()),
      multiple = TRUE
    )
  })
  
  #   conversion
  observeEvent(input$apply_conversion, {
    req(active_data())
    req(input$convert_column, input$target_type)
    
    df <- active_data()
    cols <- input$convert_column
    converted <- c()
    failed <- c()
    
    for (col in cols) {
      result <- tryCatch({
        if (input$target_type == "numeric") {
          df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
        } else if (input$target_type == "character") {
          df[[col]] <- as.character(df[[col]])
        } else if (input$target_type == "factor") {
          df[[col]] <- as.factor(df[[col]])
        } else if (input$target_type == "Date") {
          df[[col]] <- safe_convert_date(df[[col]])
        }
        TRUE
      }, error = function(e) {
        FALSE
      })
      
      if (result) {
        converted <- c(converted, col)
      } else {
        failed <- c(failed, col)
      }
    }
    
    active_data(df)
    
    if (length(converted) > 0) {
      showNotification(
        paste("Converted:", paste(converted, collapse = ", ")),
        type = "message"
      )
    }
    
    if (length(failed) > 0) {
      showNotification(
        paste("Failed:", paste(failed, collapse = ", ")),
        type = "error"
      )
    }
  })
  
  #   download mapping template
  output$download_type_mapping_template <- downloadHandler(
    filename = function() {
      "column_type_mapping_template.csv"
    },
    content = function(file) {
      req(active_data())
      
      df <- active_data()
      
      template <- data.frame(
        column_name = names(df),
        current_type = tolower(sapply(df, function(x) class(x)[1])),
        target_type = ""
      )
      
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  #   upload mapped list for conversion
  observeEvent(input$apply_mapping_conversion, {
    req(active_data())
    req(input$type_mapping_file)
    
    df <- active_data()
    
    mapping_df <- read.csv(input$type_mapping_file$datapath, stringsAsFactors = FALSE)
    
    # check required columns
    required_cols <- c("column_name", "target_type")
    if (!all(required_cols %in% names(mapping_df))) {
      showNotification(
        "Mapping file must contain columns: column_name and target_type",
        type = "error"
      )
      return(NULL)
    }
    
    valid_types <- c("numeric", "character", "factor", "date")
    
    converted <- c()
    failed <- c()
    skipped <- c()
    
    for (i in seq_len(nrow(mapping_df))) {
      col_name <- trimws(as.character(mapping_df$column_name[i]))
      target_type <- trimws(as.character(mapping_df$target_type[i]))
      target_type_lower <- tolower(target_type)
      
      # skip empty rows
      if (is.na(target_type) || target_type == "") {
        next
      }
      
      if (!(col_name %in% names(df))) {
        skipped <- c(skipped, col_name)
        next
      }
      
      if (!(target_type_lower %in% valid_types)) {
        failed <- c(failed, paste0(col_name, " (invalid type: ", target_type, ")"))
        next
      }
      
      result <- tryCatch({
        if (target_type_lower == "numeric") {
          df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
        } else if (target_type_lower == "character") {
          df[[col_name]] <- as.character(df[[col_name]])
        } else if (target_type_lower == "factor") {
          df[[col_name]] <- as.factor(df[[col_name]])
        } else if (target_type_lower == "date") {
          df[[col_name]] <- safe_convert_date(df[[col_name]])
        }
        TRUE
      }, error = function(e) {
        FALSE
      })
      
      if (result) {
        converted <- c(converted, col_name)
      } else {
        failed <- c(failed, col_name)
      }
    }
    
    active_data(df)
    
    if (length(converted) > 0) {
      showNotification(
        paste("Converted from mapping file:", paste(converted, collapse = ", ")),
        type = "message"
      )
    }
    
    if (length(skipped) > 0) {
      showNotification(
        paste("Skipped column(s) not found:", paste(skipped, collapse = ", ")),
        type = "warning"
      )
    }
    
    if (length(failed) > 0) {
      showNotification(
        paste("Failed conversion:", paste(failed, collapse = ", ")),
        type = "error"
      )
    }
  })
  
  #   drop selected columns
  observeEvent(input$drop_columns_btn, {
    req(active_data())
    req(input$columns_to_drop)
    
    df <- active_data()
    
    df <- df[, !(names(df) %in% input$columns_to_drop), drop = FALSE]
    
    active_data(df)
    
    showNotification(
      paste("Dropped columns:", paste(input$columns_to_drop, collapse = ", ")),
      type = "message"
    )
  })
  
  #   remove missing value rows
  observeEvent(input$apply_cleaning, {
    req(active_data())
    
    df <- active_data()
    
    if (isTRUE(input$remove_na_rows)) {
      df <- df[complete.cases(df), , drop = FALSE]
    } else {
      row_missing_pct <- rowMeans(is.na(df)) * 100
      df <- df[row_missing_pct <= input$na_threshold, , drop = FALSE]
    }
    
    active_data(df)
    
    showNotification(
      "Row cleaning applied successfully.",
      type = "message"
    )
  })
  
  # data structure
  output$data_structure <- renderDT({
    req(active_data())
    
    df <- active_data()
    
    structure_df <- data.frame(
      Column = names(df),
      Data_Type = sapply(df, function(x) class(x)[1]),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
      Example = sapply(df, function(x) {
        vals <- x[!is.na(x)]
        if (length(vals) == 0) return(NA)
        as.character(vals[1])
      })
    )
    
    datatable(structure_df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  #   download converted data
  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste0("cleaned_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(active_data())
      write.csv(active_data(), file, row.names = FALSE)
    }
  )
  
  # DATA PREVIEW (FIXED)
  output$data_preview <- renderDT({
    req(active_data())
    datatable(active_data(), options = list(scrollX = TRUE)) %>%
      formatStyle(
        columns = names(active_data()),
        backgroundColor = styleEqual(NA, "#ffe6e6")
      )
  })
  
  # MODULES (FIXED)
  eda_server("eda_1", data = active_data)
  clustering_server("cluster_1", data = active_data)
  model_server("model_1", data = active_data, saved_models = saved_models)
  comparison_server("comparison_1", data = active_data, saved_models = saved_models)
}

shinyApp(ui, server)