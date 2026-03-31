eda_ui <- function(id) {
  ns <- NS(id)

  
  tagList(
    tags$style(HTML("
      .eda-title {
        font-size: 28px;
        font-weight: 700;
        margin-bottom: 10px;
      }
      .eda-desc {
        font-size: 16px;
        line-height: 1.8;
        margin-bottom: 18px;
      }
      .eda-side-card {
        background: #ffffff;
        border: 1px solid #d9d9d9;
        border-radius: 14px;
        padding: 16px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }
      .eda-summary-box {
        background-color: #f3f4f6;
        border-left: 5px solid #5DADE2;
        padding: 16px;
        border-radius: 10px;
        margin-top: 18px;
        font-size: 15px;
        line-height: 1.7;
      }
      .nav-tabs .nav-link {
        color: #5a5a5a;
      }
      .nav-tabs .nav-link.active {
        font-weight: 600;
        color: #000000;
      }
    ")),
    
    div(class = "eda-title", "Exploratory Data Analysis"),
    
    div(
      class = "eda-desc",
      "This is the first part of the R-Shiny application where users can perform Exploratory Data Analysis on the currently loaded and cleaned dataset. Users may select any variable and the application will automatically generate the appropriate visualisation."
    ),
    
    tabsetPanel(
      id = ns("eda_tabs"),
      
      tabPanel(
        "Univariate",
        br(),
        fluidRow(
          column(
            width = 4,
            div(
              class = "eda-side-card",
              uiOutput(ns("variable_ui")),
              uiOutput(ns("log_scale_ui"))
            )
          ),
          
          column(
            width = 8,
            h2(strong("Univariate Analysis")),
            plotlyOutput(ns("uni_plot"), height = "500px"),
            div(class = "eda-summary-box", htmlOutput(ns("interpretation_text")))
          )
        )
      ),
      
      tabPanel(
        "Bivariate",
        br(),
        fluidRow(
          column(
            width = 4,
            div(
              class = "eda-side-card",
              uiOutput(ns("scatter_x_ui")),
              uiOutput(ns("scatter_y_ui"))
            )
          ),
          column(
            width = 8,
            h2(strong("Bivariate Analysis")),
            plotlyOutput(ns("scatter_plot"), height = "500px")
          )
        )
      )
    )
  )
}

eda_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    teal_col <- "#20B2AA"
    
    pretty_label <- function(x) {
      x <- gsub("_", " ", x)
      tools::toTitleCase(x)
    }
    
    detect_var_type <- function(x, var_name = NULL) {
      if (inherits(x, "Date")) return("date")
      
      if (!is.null(var_name)) {
        id_pattern <- "(^id$|_id$|id_|customer_id|account_id|transaction_id|member_id|user_id)"
        if (grepl(id_pattern, tolower(var_name))) {
          return("identifier")
        }
      }
      
      if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
      
      if (is.numeric(x) || is.integer(x)) {
        unique_ratio <- dplyr::n_distinct(x, na.rm = TRUE) / sum(!is.na(x))
        
        if (unique_ratio > 0.9) {
          return("identifier")
        }
        
        return("numeric")
      }
      
      return("other")
    }
    
    is_skewed_numeric <- function(x) {
      x <- x[is.finite(x)]
      if (length(x) < 10) return(FALSE)
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      med <- median(x, na.rm = TRUE)
      iqr_val <- q3 - q1
      if (iqr_val == 0) return(FALSE)
      max(x, na.rm = TRUE) > med + 3 * iqr_val
    }
    
#     Helper to detect ID columns
    is_identifier <- function(x, name) {
      name <- tolower(name)
      
      # name-based rule
      if (grepl("(^id$|_id$|id_|customer_id|account_id|transaction_id|member_id|user_id)", name)) {
        return(TRUE)
      }
      
      # high uniqueness numeric columns are likely IDs
      if (is.numeric(x) || is.integer(x)) {
        non_missing_n <- sum(!is.na(x))
        if (non_missing_n == 0) return(FALSE)
        
        unique_ratio <- dplyr::n_distinct(x, na.rm = TRUE) / non_missing_n
        if (unique_ratio > 0.9) return(TRUE)
      }
      
      FALSE
    }
    
    detect_var_type <- function(x, var_name = NULL) {
      if (inherits(x, "Date")) return("date")
      if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
      if (is.numeric(x) || is.integer(x)) return("numeric")
      return("other")
    }
    
    
    current_data <- reactive({
      req(data())
      data()
    })
    
    numeric_cols <- reactive({
      req(current_data())
      
      df <- current_data()
      
      names(df)[sapply(names(df), function(nm) {
        !is_identifier(df[[nm]], nm) && detect_var_type(df[[nm]], nm) == "numeric"
      })]
    })
    
    output$variable_ui <- renderUI({
      df <- current_data()
      
      non_id_names <- names(df)[
        !mapply(is_identifier, df, names(df))
      ]
      
      if (length(non_id_names) == 0) {
        return(
          helpText("No suitable analysis variables available after excluding identifier columns.")
        )
      }
      
      numeric_vars <- non_id_names[sapply(non_id_names, function(nm) {
        detect_var_type(df[[nm]], nm) == "numeric"
      })]
      
      categorical_vars <- non_id_names[sapply(non_id_names, function(nm) {
        detect_var_type(df[[nm]], nm) == "categorical"
      })]
      
      date_vars <- non_id_names[sapply(non_id_names, function(nm) {
        detect_var_type(df[[nm]], nm) == "date"
      })]
      
      grouped_choices <- list()
      
      if (length(numeric_vars) > 0) {
        grouped_choices[["Numeric Variables"]] <- setNames(numeric_vars, pretty_label(numeric_vars))
      }
      
      if (length(categorical_vars) > 0) {
        grouped_choices[["Categorical Variables"]] <- setNames(categorical_vars, pretty_label(categorical_vars))
      }
      
      if (length(date_vars) > 0) {
        grouped_choices[["Date Variables"]] <- setNames(date_vars, pretty_label(date_vars))
      }
      
      first_choice <- c(numeric_vars, categorical_vars, date_vars)[1]
      
      selectInput(
        inputId = session$ns("selected_var"),
        label = strong("Variables"),
        choices = grouped_choices,
        selected = first_choice
      )
    })
    
    output$log_scale_ui <- renderUI({
      req(input$selected_var)
      df <- current_data()
      x <- df[[input$selected_var]]
      
      if (detect_var_type(x) == "numeric" && is_skewed_numeric(x)) {
        checkboxInput(
          inputId = session$ns("use_log_scale"),
          label = "Use log scale for skewed data",
          value = FALSE
        )
      }
    })
    
    output$uni_plot <- renderPlotly({
      req(input$selected_var)
      
      df <- current_data()
      req(input$selected_var %in% names(df))
      
      var_name <- input$selected_var
      x <- df[[var_name]]
      var_type <- detect_var_type(x, var_name)
      label <- pretty_label(var_name)
      use_log <- isTRUE(input$use_log_scale)
      
      if (var_type == "numeric") {
        plot_df <- df |>
          dplyr::filter(!is.na(.data[[var_name]]))
        
        if (use_log) {
          plot_df <- plot_df |>
            dplyr::filter(.data[[var_name]] > 0)
          
          p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[var_name]])) +
            ggplot2::geom_histogram(
              bins = 30,
              fill = teal_col,
              color = "white",
              alpha = 0.9
            ) +
            ggplot2::scale_x_log10(labels = scales::comma) +
            ggplot2::labs(
              title = paste("Distribution of", label),
              x = paste(label, "(log scale)"),
              y = "Count"
            ) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", size = 16),
              panel.grid.minor = ggplot2::element_blank()
            )
        } else {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[var_name]])) +
            ggplot2::geom_histogram(
              bins = 30,
              fill = teal_col,
              color = "white",
              alpha = 0.9
            ) +
            ggplot2::labs(
              title = paste("Distribution of", label),
              x = label,
              y = "Count"
            ) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", size = 16),
              panel.grid.minor = ggplot2::element_blank()
            ) +
            ggplot2::scale_x_continuous(labels = scales::comma)
        }
        
        plotly::ggplotly(p)
        
      } else if (var_type == "categorical") {
        plot_df <- df |>
          dplyr::mutate(temp_var = as.character(.data[[var_name]])) |>
          dplyr::mutate(temp_var = ifelse(is.na(temp_var), "Missing", temp_var)) |>
          dplyr::count(temp_var, name = "count") |>
          dplyr::arrange(desc(count))
        
        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(x = reorder(temp_var, count), y = count, fill = temp_var)
        ) +
          ggplot2::geom_col(show.legend = FALSE, alpha = 0.95) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = paste("Frequency of", label),
            x = label,
            y = "Count"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 16),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::scale_y_continuous(labels = scales::comma)
        
        plotly::ggplotly(p)
        
      } else if (var_type == "date") {
        plot_df <- df |>
          dplyr::filter(!is.na(.data[[var_name]])) |>
          dplyr::count(.data[[var_name]], name = "count")
        
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[var_name]], y = count)) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 2) +
          ggplot2::labs(
            title = paste("Date Distribution of", label),
            x = label,
            y = "Count"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 16),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::scale_y_continuous(labels = scales::comma)
        
        plotly::ggplotly(p)
        
      } else if (var_type == "identifier") {
        p <- ggplot2::ggplot() +
          ggplot2::annotate(
            "text",
            x = 1,
            y = 1,
            label = "This variable looks like an identifier, so a distribution plot is not meaningful.",
            size = 5
          ) +
          ggplot2::theme_void()
        
        plotly::ggplotly(p)
      }
    })
    
    output$interpretation_text <- renderUI({
      req(input$selected_var)
      
      df <- current_data()
      var_name <- input$selected_var
      x <- df[[var_name]]
      var_type <- detect_var_type(x, var_name)
      label <- pretty_label(var_name)
      use_log <- isTRUE(input$use_log_scale)
      
      if (var_type == "identifier") {
        return(HTML(paste0(
          "<b>", label, "</b> appears to be an identifier field. ",
          "Identifier variables usually have many unique values, so univariate distribution plots are not very meaningful."
        )))
      }
      
      if (var_type == "numeric") {
        miss <- sum(is.na(x))
        med <- round(stats::median(x, na.rm = TRUE), 2)
        mean_val <- round(mean(x, na.rm = TRUE), 2)
        min_val <- round(min(x, na.rm = TRUE), 2)
        max_val <- round(max(x, na.rm = TRUE), 2)
        skew_flag <- is_skewed_numeric(x)
        
        txt <- paste0(
          "<b>", label, "</b> is a numeric variable. ",
          "The values range from ", scales::comma(min_val), " to ", scales::comma(max_val),
          ", with a median of ", scales::comma(med),
          " and mean of ", scales::comma(mean_val), ". "
        )
        
        if (skew_flag) {
          txt <- paste0(txt, "The distribution appears right-skewed, meaning a small number of large values may stretch the scale. ")
        } else {
          txt <- paste0(txt, "The distribution appears relatively balanced without extreme stretching. ")
        }
        
        if (use_log) {
          txt <- paste0(txt, "A log scale is applied to improve readability for large-value differences. ")
        }
        
        txt <- paste0(txt, "Missing values: ", miss, ".")
        return(HTML(txt))
      }
      
      if (var_type == "categorical") {
        top_cat <- df |>
          dplyr::mutate(temp_var = as.character(.data[[var_name]])) |>
          dplyr::mutate(temp_var = ifelse(is.na(temp_var), "Missing", temp_var)) |>
          dplyr::count(temp_var, sort = TRUE, name = "count") |>
          dplyr::slice(1)
        
        n_cat <- dplyr::n_distinct(x, na.rm = TRUE)
        miss <- sum(is.na(x))
        
        return(HTML(paste0(
          "<b>", label, "</b> is a categorical variable with ",
          n_cat, " distinct categories. ",
          "The most common category is '<b>", top_cat$temp_var,
          "</b>' with ", scales::comma(top_cat$count),
          " observations. Missing values: ", miss, "."
        )))
      }
      
      if (var_type == "date") {
        earliest <- min(x, na.rm = TRUE)
        latest <- max(x, na.rm = TRUE)
        miss <- sum(is.na(x))
        
        return(HTML(paste0(
          "<b>", label, "</b> is a date variable spanning from ",
          earliest, " to ", latest,
          ". The plot shows how observations are distributed across time. ",
          "Missing values: ", miss, "."
        )))
      }
      
      HTML("Interpretation is not available for this variable.")
    })
    
    output$scatter_x_ui <- renderUI({
      req(length(numeric_cols()) >= 1)
      
      selectInput(
        inputId = session$ns("x_var"),
        label = strong("X Variable"),
        choices = setNames(numeric_cols(), pretty_label(numeric_cols())),
        selected = numeric_cols()[1]
      )
    })
    
    output$scatter_y_ui <- renderUI({
      req(length(numeric_cols()) >= 2)
      
      selectInput(
        inputId = session$ns("y_var"),
        label = strong("Y Variable"),
        choices = setNames(numeric_cols(), pretty_label(numeric_cols())),
        selected = numeric_cols()[2]
      )
    })
    
    output$scatter_plot <- renderPlotly({
      req(input$x_var, input$y_var)
      
      df <- current_data()
      
      plot_df <- df |>
        dplyr::filter(!is.na(.data[[input$x_var]]), !is.na(.data[[input$y_var]]))
      
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data[[input$x_var]], y = .data[[input$y_var]])
      ) +
        ggplot2::geom_point(color = "#2E86C1", alpha = 0.7, size = 2.5) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#E74C3C") +
        ggplot2::labs(
          title = paste(pretty_label(input$y_var), "vs", pretty_label(input$x_var)),
          x = pretty_label(input$x_var),
          y = pretty_label(input$y_var)
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      plotly::ggplotly(p)
    })
  })
}