pacman::p_load(
  shiny, bs4Dash, DT, readr, dplyr, tidyverse, ggplot2,
  plotly, lubridate, bslib, scales,
  caret, rpart, ranger, recipes, parsnip, workflows, xgboost, randomForest
)

cleared_flag <- reactiveVal(FALSE)

model_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      bs4Card(
        title = "Select Regression Model",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        
        selectInput(
          inputId = ns("model_type"),
          label = "Select Model",
          choices = c("Linear Regression", "Regression Tree", "Random Forest", "XGBoost")
        ),
        
        uiOutput(ns("target_ui")),
        uiOutput(ns("predictors_ui")),
        
        fluidRow(
          column(
            width = 6,
            actionButton(ns("select_all_predictors"), "Select All", class = "btn-secondary btn-sm")
          ),
          column(
            width = 6,
            actionButton(ns("clear_predictors"), "Clear All", class = "btn-secondary btn-sm")
          )
        ),
        br(),
        
        uiOutput(ns("cluster_note_ui")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Regression Tree'", ns("model_type")),
          sliderInput(ns("minsplit"), "Minimum Split", min = 10, max = 100, value = 50),
          sliderInput(ns("cp"), "Complexity Parameter", min = 0.001, max = 0.05, value = 0.01, step = 0.001),
          sliderInput(ns("tree_maxdepth"), "Maximum Depth", min = 2, max = 20, value = 4)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Random Forest'", ns("model_type")),
          sliderInput(ns("n_trees"), "Number of Trees", min = 50, max = 500, value = 100),
          sliderInput(ns("mtry"), "mtry", min = 1, max = 20, value = 3),
          sliderInput(ns("min_node_size"), "Minimum Node Size", min = 1, max = 20, value = 5)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'XGBoost'", ns("model_type")),
          sliderInput(ns("xgb_trees"), "Number of Trees", min = 100, max = 1000, value = 500, step = 50),
          sliderInput(ns("xgb_depth"), "Tree Depth", min = 2, max = 10, value = 6),
          sliderInput(ns("xgb_lr"), "Learning Rate", min = 0.01, max = 0.3, value = 0.05, step = 0.01),
          sliderInput(ns("xgb_min_n"), "Minimum Node Size", min = 1, max = 20, value = 5),
          sliderInput(ns("xgb_sample"), "Sample Size", min = 0.5, max = 1, value = 0.8, step = 0.1)
        ),
        
        actionButton(ns("run_model"), "Run Experiment", class = "btn-primary"),
        br(), br(),
        actionButton(ns("save_model"), "Save Model to Comparison", class = "btn-success")
      ),
      
      column(
        width = 8,
        
        fluidRow(
          bs4ValueBoxOutput(ns("r2_box"), width = 4),
          bs4ValueBoxOutput(ns("mse_box"), width = 4),
          bs4ValueBoxOutput(ns("rmse_box"), width = 4)
        ),
        
        fluidRow(
          bs4Card(
            title = "Evaluation Metrics",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            DT::DTOutput(ns("metrics_table"))
          )
        )
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "Feature Importance",
        width = 6,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("feature_plot_bottom"), height = "300px")
      ),
      
      bs4Card(
        title = "Actual vs Predicted",
        width = 6,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("scatter_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "Model Summary",
        width = 12,
        status = "teal",
        solidHeader = FALSE,
        
        # button at top
        div(
          style = "margin-bottom: 10px;",
          downloadButton(ns("download_coefficients"), "Download Coefficients")
        ),
        
        # model summary below
        verbatimTextOutput(ns("model_summary"))
      )
      
    )
  )
}

model_server <- function(id, data, saved_models) {
  moduleServer(id, function(input, output, session) {
    
    # numeric columns from uploaded CSV
    # target should remain numeric because this is a regression task
    numeric_cols <- reactive({
      req(data())
      names(data())[sapply(data(), is.numeric)]
    })
    
    # predictor columns can include numeric, factor, and character
    # this allows the cluster column to be used once it is written back by the clustering module
    predictor_cols <- reactive({
      req(data())
      names(data())[sapply(data(), function(x) {
        is.numeric(x) || is.factor(x) || is.character(x)
      })]
    })
    
    # show a note when cluster column is available for modelling
    output$cluster_note_ui <- renderUI({
      req(data())
      
      if ("cluster" %in% names(data())) {
        tags$div(
          style = "
            background-color: #e8f7f2;
            border-left: 4px solid #20c997;
            padding: 10px 12px;
            border-radius: 6px;
            margin-bottom: 12px;
            font-size: 14px;
          ",
          HTML(
            "<b>Cluster detected.</b><br>
             The cluster column is available and will be included as a predictor by default."
          )
        )
      } else {
        tags$div(
          style = "
            background-color: #fff3cd;
            border-left: 4px solid #f0ad4e;
            padding: 10px 12px;
            border-radius: 6px;
            margin-bottom: 12px;
            font-size: 14px;
          ",
          HTML(
            "<b>No cluster column detected yet.</b><br>
             Run clustering first if you want to use customer segments in the prediction model."
          )
        )
      }
    })
    
    # target selector UI
    output$target_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) >= 2)
      
      selectInput(
        inputId = session$ns("target"),
        label = "Select Target Variable",
        choices = numeric_cols(),
        selected = numeric_cols()[1]
      )
    })
    
    # predictor selector UI
    output$predictors_ui <- renderUI({
      req(data())
      req(input$target)
      
      available_predictors <- setdiff(predictor_cols(), input$target)
      
      # preserve existing selection if user already picked
      selected_vars <- isolate(input$predictors)
      
      if (cleared_flag()) {
        selected_vars <- character(0)
      } else if (is.null(selected_vars) || length(selected_vars) == 0) {
        if ("cluster" %in% available_predictors) {
          selected_vars <- c("cluster", setdiff(available_predictors, "cluster"))
        } else {
          selected_vars <- available_predictors
        }
      }
      
      selectInput(
        inputId = session$ns("predictors"),
        label = "Select Predictor Variables",
        choices = available_predictors,
        selected = selected_vars,
        multiple = TRUE
      )
    })
    
    # select all predictor variables
    observeEvent(input$select_all_predictors, {
      req(data())
      req(input$target)
      
      available_predictors <- setdiff(predictor_cols(), input$target)
      
      if ("cluster" %in% available_predictors) {
        selected_vars <- c("cluster", setdiff(available_predictors, "cluster"))
      } else {
        selected_vars <- available_predictors
      }
      
      updateSelectInput(
        session = session,
        inputId = "predictors",
        selected = selected_vars
      )
    })
    
    # clear all predictor variables
    observeEvent(input$clear_predictors, {
      cleared_flag(TRUE)
      
      updateSelectInput(
        session = session,
        inputId = "predictors",
        selected = character(0)
      )
    })
    
    # modelling dataset
    model_data <- reactive({
      req(data())
      req(input$target, input$predictors)
      req(length(input$predictors) >= 1)
      
      selected_cols <- unique(c(input$target, input$predictors))
      df <- data()[, selected_cols, drop = FALSE]
      
      # convert character predictors to factor
      # this helps lm, rpart and randomForest handle categorical predictors properly
      for (col in names(df)) {
        if (col != input$target && is.character(df[[col]])) {
          df[[col]] <- as.factor(df[[col]])
        }
      }
      
      df <- na.omit(df)
      
      validate(
        need(nrow(df) > 5, "Not enough complete rows to train the model.")
      )
      
      df
    })
    
    # fit selected model when user clicks Run Experiment
    fitted_model <- eventReactive(input$run_model, {
      req(model_data())
      
      predictors <- setdiff(input$predictors, input$target)
      
      validate(
        need(length(predictors) >= 1, "Please select at least 1 predictor.")
      )
      
      df <- model_data()
      formula_text <- paste(input$target, "~", paste(predictors, collapse = " + "))
      model_formula <- as.formula(formula_text)
      
      model <- NULL
      preds <- NULL
      importance_df <- NULL
      model_summary_text <- NULL
      
      # linear regression
      if (input$model_type == "Linear Regression") {
        model <- lm(model_formula, data = df)
        preds <- predict(model, newdata = df)
        
        coef_vals <- coef(model)
        coef_vals <- coef_vals[names(coef_vals) != "(Intercept)"]
        
        importance_df <- data.frame(
          feature = names(coef_vals),
          importance = abs(as.numeric(coef_vals))
        ) %>%
          arrange(desc(importance))
        
        model_summary_text <- capture.output(summary(model))
      }
      
      # regression tree
      else if (input$model_type == "Regression Tree") {
        model <- rpart::rpart(
          formula = model_formula,
          data = df,
          method = "anova",
          control = rpart::rpart.control(
            minsplit = input$minsplit,
            cp = input$cp,
            maxdepth = input$tree_maxdepth
          )
        )
        
        preds <- predict(model, newdata = df)
        
        imp_vals <- model$variable.importance
        if (is.null(imp_vals)) {
          imp_vals <- setNames(rep(0, length(predictors)), predictors)
        }
        
        importance_df <- data.frame(
          feature = names(imp_vals),
          importance = as.numeric(imp_vals)
        ) %>%
          arrange(desc(importance))
        
        model_summary_text <- capture.output(summary(model))
      }
      
      # random forest
      else if (input$model_type == "Random Forest") {
        model <- randomForest::randomForest(
          formula = model_formula,
          data = df,
          ntree = input$n_trees,
          mtry = min(input$mtry, length(predictors)),
          nodesize = input$min_node_size,
          importance = TRUE
        )
        
        preds <- predict(model, newdata = df)
        
        imp_mat <- randomForest::importance(model)
        
        if (is.matrix(imp_mat)) {
          importance_df <- data.frame(
            feature = rownames(imp_mat),
            importance = imp_mat[, 1]
          ) %>%
            arrange(desc(importance))
        } else {
          importance_df <- data.frame(
            feature = names(imp_mat),
            importance = as.numeric(imp_mat)
          ) %>%
            arrange(desc(importance))
        }
        
        model_summary_text <- capture.output(print(model))
      }
      
      # xgboost
      else if (input$model_type == "XGBoost") {
        x_df <- df[, predictors, drop = FALSE]
        
        # one hot encode non numeric predictors
        # this is required because xgboost needs numeric matrix input
        x_mat <- model.matrix(~ . - 1, data = x_df)
        y_vec <- df[[input$target]]
        
        model <- xgboost::xgboost(
          data = x_mat,
          label = y_vec,
          nrounds = input$xgb_trees,
          max_depth = input$xgb_depth,
          eta = input$xgb_lr,
          min_child_weight = input$xgb_min_n,
          subsample = input$xgb_sample,
          objective = "reg:squarederror",
          verbose = 0
        )
        
        preds <- predict(model, newdata = x_mat)
        
        imp_tbl <- xgboost::xgb.importance(
          feature_names = colnames(x_mat),
          model = model
        )
        
        if (is.null(imp_tbl) || nrow(imp_tbl) == 0) {
          importance_df <- data.frame(
            feature = colnames(x_mat),
            importance = 0
          )
        } else {
          importance_df <- data.frame(
            feature = imp_tbl$Feature,
            importance = imp_tbl$Gain
          ) %>%
            arrange(desc(importance))
        }
        
        model_summary_text <- capture.output(print(model))
      }
      
      # evaluation metrics
      actual <- df[[input$target]]
      mse <- mean((actual - preds)^2)
      rmse <- sqrt(mse)
      r2 <- 1 - sum((actual - preds)^2) / sum((actual - mean(actual))^2)
      
      list(
        model = model,
        model_type = input$model_type,
        target = input$target,
        predictors = predictors,
        predictions = preds,
        actual = actual,
        mse = mse,
        rmse = rmse,
        r2 = r2,
        importance_df = importance_df,
        model_summary_text = model_summary_text
      )
    })
    
    # save model result to comparison tab
    observeEvent(input$save_model, {
      req(fitted_model())
      
      model_name <- paste0(
        fitted_model()$model_type, "_",
        fitted_model()$target, "_",
        format(Sys.time(), "%H%M%S")
      )
      
      saved_models$results[[model_name]] <- list(
        model_type = fitted_model()$model_type,
        target = fitted_model()$target,
        r2 = fitted_model()$r2,
        mse = fitted_model()$mse,
        rmse = fitted_model()$rmse,
        actual = fitted_model()$actual,
        predictions = fitted_model()$predictions
      )
      
      showNotification(paste("Saved model:", model_name), type = "message")
    })
    
    # r squared value box
    output$r2_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = round(fitted_model()$r2, 3),
        subtitle = HTML("R<sup>2</sup>"),
        color = "primary",
        icon = icon("chart-line")
      )
    })
    
    # mse value box
    output$mse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = format(round(fitted_model()$mse, 2), big.mark = ","),
        subtitle = "MSE",
        color = "info",
        icon = icon("calculator")
      )
    })
    
    # rmse value box
    output$rmse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = format(round(fitted_model()$rmse, 2), big.mark = ","),
        subtitle = "RMSE",
        color = "warning",
        icon = icon("square-root-alt")
      )
    })
    
    # prediction table
    output$metrics_table <- DT::renderDT({
      req(fitted_model())
      
      results_df <- data.frame(
        Actual = fitted_model()$actual,
        Predicted = fitted_model()$predictions,
        Residual = fitted_model()$actual - fitted_model()$predictions
      )
      
      DT::datatable(
        head(results_df, 20),
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
    
    # full feature importance plot
    output$feature_plot_bottom <- renderPlot({
      req(fitted_model())
      imp <- fitted_model()$importance_df
      req(!is.null(imp))
      req(nrow(imp) > 0)
      
      par(mar = c(5, 8, 3, 2))
      barplot(
        rev(imp$importance),
        names.arg = rev(imp$feature),
        horiz = TRUE,
        las = 1,
        col = "steelblue",
        main = "Feature Importance"
      )
    })
    
    # download coefficients for linear regression
    # for tree based models, export feature importance instead
    output$download_coefficients <- downloadHandler(
      filename = function() {
        paste0("model_coefficients_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(fitted_model())
        
        model_obj <- fitted_model()$model
        
        # linear regression has actual coefficients
        if ("lm" %in% class(model_obj)) {
          coef_vals <- coef(model_obj)
          
          coef_df <- data.frame(
            Feature = names(coef_vals),
            Coefficient = as.numeric(coef_vals)
          )
          
          write.csv(coef_df, file, row.names = FALSE)
          
        } else {
          # non linear models do not have standard coefficients
          # export feature importance instead
          imp_df <- fitted_model()$importance_df
          
          validate(
            need(!is.null(imp_df) && nrow(imp_df) > 0,
                 "No coefficients or feature importance available for download.")
          )
          
          write.csv(imp_df, file, row.names = FALSE)
        }
      }
    )
    
    # actual vs predicted scatter plot
    output$scatter_plot <- renderPlot({
      req(fitted_model())
      
      plot(
        x = fitted_model()$actual,
        y = fitted_model()$predictions,
        pch = 19,
        col = "steelblue",
        main = "Actual vs Predicted",
        xlab = "Actual",
        ylab = "Predicted"
      )
      
      abline(0, 1, col = "red", lty = 2)
    })
    
    # model summary text
    output$model_summary <- renderPrint({
      req(fitted_model())
      cat(paste(fitted_model()$model_summary_text, collapse = "\n"))
    })
    
  })
}