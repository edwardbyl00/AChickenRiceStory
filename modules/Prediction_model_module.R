pacman::p_load(
  shiny, bs4Dash, DT, dplyr, rpart, xgboost, randomForest
)

model_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "Select Regression Model",
          width = 12,
          status = "teal",
          solidHeader = FALSE,
          
          selectInput(
            inputId = ns("model_type"),
            label = "Select Model",
            choices = c("Linear Regression", "Regression Tree", "Random Forest", "XGBoost")
          ),
          
          uiOutput(ns("target_ui")),
          uiOutput(ns("predictors_ui")),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Regression Tree'", ns("model_type")),
            sliderInput(ns("minsplit"), "Minimum Split", min = 10, max = 100, value = 50),
            sliderInput(ns("cp"), "Complexity Parameter", min = 0.001, max = 0.05, value = 0.01, step = 0.001),
            sliderInput(ns("tree_maxdepth"), "Maximum Depth", min = 2, max = 20, value = 4)
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Random Forest'", ns("model_type")),
            sliderInput(ns("n_trees"), "Number of Trees", min = 50, max = 500, value = 100),
            sliderInput(ns("mtry"), "mtry", min = 1, max = 10, value = 3),
            sliderInput(ns("min_node_size"), "Minimum Node Size", min = 1, max = 20, value = 5)
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'XGBoost'", ns("model_type")),
            sliderInput(ns("xgb_trees"), "Number of Boosting Rounds", min = 100, max = 1000, value = 500, step = 50),
            sliderInput(ns("xgb_depth"), "Tree Depth", min = 2, max = 10, value = 6),
            sliderInput(ns("xgb_lr"), "Learning Rate", min = 0.01, max = 0.3, value = 0.05, step = 0.01),
            sliderInput(ns("xgb_min_n"), "Minimum Node Size", min = 1, max = 20, value = 5),
            sliderInput(ns("xgb_sample"), "Sample Size", min = 0.5, max = 1, value = 0.8, step = 0.1)
          ),
          
          actionButton(ns("run_model"), "Run Experiment", class = "btn-primary"),
          br(), br(),
          actionButton(ns("save_model"), "Save Current Model", class = "btn-success")
        )
      ),
      
      column(
        width = 8,
        fluidRow(
          column(
            width = 4,
            bs4ValueBoxOutput(ns("r2_box"), width = 12)
          ),
          column(
            width = 4,
            bs4ValueBoxOutput(ns("mse_box"), width = 12)
          ),
          column(
            width = 4,
            bs4ValueBoxOutput(ns("rmse_box"), width = 12)
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Card(
              title = "Evaluation Metrics",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              DTOutput(ns("metrics_table"))
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "Feature Importance",
          width = 12,
          status = "primary",
          solidHeader = FALSE,
          plotOutput(ns("feature_plot_bottom"), height = "280px")
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "Actual vs Predicted",
          width = 12,
          status = "teal",
          solidHeader = FALSE,
          plotOutput(ns("scatter_plot"), height = "280px")
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "Model Summary",
          width = 12,
          status = "teal",
          solidHeader = FALSE,
          verbatimTextOutput(ns("model_summary"))
        )
      )
    )
  )
    
  
}

model_server <- function(id, data, saved_models) {
  moduleServer(id, function(input, output, session) {
    
    numeric_cols <- reactive({
      req(data())
      cols <- names(data())[sapply(data(), is.numeric)]
      cols[!grepl("(^id$|_id$)", cols, ignore.case = TRUE)]
    })
    
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
    
    output$predictors_ui <- renderUI({
      req(data(), input$target)
      req(length(numeric_cols()) >= 2)
      
      predictor_choices <- setdiff(numeric_cols(), input$target)
      
      selectizeInput(
        inputId = session$ns("predictors"),
        label = "Select Predictor Variables",
        choices = predictor_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select predictor variables",
          plugins = list("remove_button"),
          maxItems = NULL,
          closeAfterSelect = FALSE
        )
      )
    })
    
    model_data <- reactive({
      req(data())
      req(input$target, input$predictors)
      req(length(input$predictors) >= 1)
      
      selected_cols <- unique(c(input$target, input$predictors))
      df <- data()[, selected_cols, drop = FALSE]
      na.omit(df)
    })
    
    fitted_model <- eventReactive(input$run_model, {
      req(model_data())
      
      predictors <- setdiff(input$predictors, input$target)
      req(length(predictors) >= 1)
      
      formula_text <- paste(input$target, "~", paste(predictors, collapse = " + "))
      model_formula <- as.formula(formula_text)
      
      set.seed(123)
      df <- model_data()
      train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
      
      train_data <- df[train_idx, , drop = FALSE]
      test_data  <- df[-train_idx, , drop = FALSE]
      
      validate(
        need(nrow(test_data) > 1, "Test set is too small for evaluation.")
      )
      
      model <- NULL
      preds <- NULL
      importance_df <- data.frame(
        feature = character(0),
        importance = numeric(0)
      )
      
      if (input$model_type == "Linear Regression") {
        
        model <- lm(model_formula, data = train_data)
        preds <- predict(model, newdata = test_data)
        
        coefs <- coef(model)
        coefs <- coefs[names(coefs) != "(Intercept)"]
        
        if (length(coefs) > 0) {
          importance_df <- data.frame(
            feature = names(coefs),
            importance = abs(as.numeric(coefs))
          )
        }
        
      } else if (input$model_type == "Regression Tree") {
        
        model <- rpart::rpart(
          formula = model_formula,
          data = train_data,
          method = "anova",
          control = rpart::rpart.control(
            minsplit = input$minsplit,
            cp = input$cp,
            maxdepth = input$tree_maxdepth
          )
        )
        
        preds <- predict(model, newdata = test_data)
        
        if (!is.null(model$variable.importance)) {
          importance_df <- data.frame(
            feature = names(model$variable.importance),
            importance = as.numeric(model$variable.importance)
          )
        }
        
      } else if (input$model_type == "Random Forest") {
        
        model <- randomForest::randomForest(
          formula = model_formula,
          data = train_data,
          ntree = input$n_trees,
          mtry = min(input$mtry, length(predictors)),
          importance = TRUE
        )
        
        preds <- predict(model, newdata = test_data)
        
        rf_imp <- randomForest::importance(model)
        
        if (!is.null(rf_imp)) {
          if (is.matrix(rf_imp)) {
            importance_df <- data.frame(
              feature = rownames(rf_imp),
              importance = rf_imp[, 1]
            )
          } else {
            importance_df <- data.frame(
              feature = names(rf_imp),
              importance = as.numeric(rf_imp)
            )
          }
        }
        
      } else if (input$model_type == "XGBoost") {
        
        x_train <- as.matrix(train_data[, predictors, drop = FALSE])
        y_train <- train_data[[input$target]]
        x_test  <- as.matrix(test_data[, predictors, drop = FALSE])
        
        dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
        
        params <- list(
          objective = "reg:squarederror",
          max_depth = input$xgb_depth,
          eta = input$xgb_lr,
          min_child_weight = input$xgb_min_n,
          subsample = input$xgb_sample
        )
        
        model <- xgboost::xgb.train(
          params = params,
          data = dtrain,
          nrounds = input$xgb_trees,
          verbose = 0
        )
        
        preds <- predict(model, newdata = x_test)
        
        xgb_imp <- xgboost::xgb.importance(
          feature_names = predictors,
          model = model
        )
        
        if (!is.null(xgb_imp) && nrow(xgb_imp) > 0) {
          importance_df <- data.frame(
            feature = xgb_imp$Feature,
            importance = xgb_imp$Gain
          )
        }
      }
      
      actual <- test_data[[input$target]]
      mse <- mean((actual - preds)^2)
      rmse <- sqrt(mse)
      r2 <- 1 - sum((actual - preds)^2) / sum((actual - mean(actual))^2)
      
      if (nrow(importance_df) > 0) {
        importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), , drop = FALSE]
      }
      
      list(
        model = model,
        predictions = preds,
        actual = actual,
        mse = mse,
        rmse = rmse,
        r2 = r2,
        predictors = predictors,
        importance_df = importance_df
      )
    })
    
    observeEvent(input$save_model, {
      req(fitted_model())
      
      saved_models$results[[input$model_type]] <- list(
        model_type = input$model_type,
        target = input$target,
        predictors = fitted_model()$predictors,
        actual = fitted_model()$actual,
        predictions = fitted_model()$predictions,
        mse = fitted_model()$mse,
        rmse = fitted_model()$rmse,
        r2 = fitted_model()$r2
      )
      
      showNotification(
        paste(input$model_type, "saved for comparison."),
        type = "message"
      )
    })
    
    output$r2_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = format(round(fitted_model()$r2, 2), nsmall = 2),
        subtitle = HTML("R<sup>2</sup>"),
        color = "primary",
        icon = icon("chart-line")
      )
    })
    
    output$mse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = sprintf("%.2e", fitted_model()$mse),
        subtitle = "MSE",
        color = "info",
        icon = icon("calculator")
      )
    })
    
    output$rmse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = sprintf("%.2e", fitted_model()$rmse),
        subtitle = "RMSE",
        color = "warning",
        icon = icon("square-root-alt")
      )
    })
    
    output$metrics_table <- renderDT({
      req(fitted_model())
      
      results_df <- data.frame(
        Actual = fitted_model()$actual,
        Predicted = fitted_model()$predictions,
        Residual = fitted_model()$actual - fitted_model()$predictions
      )
      
      datatable(
        head(results_df, 20),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
    
    
    output$feature_plot_bottom <- renderPlot({
      req(fitted_model())
      
      imp <- fitted_model()$importance_df
      
      validate(
        need(nrow(imp) > 0, paste("Feature importance is not available for", input$model_type))
      )
      
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
    
    output$model_summary <- renderPrint({
      req(fitted_model())
      
      model <- fitted_model()$model
      
      tryCatch({
        if (input$model_type == "Linear Regression") {
          summary(model)
        } else if (input$model_type == "Regression Tree") {
          summary(model)
        } else if (input$model_type == "Random Forest") {
          print(model)
        } else if (input$model_type == "XGBoost") {
          cat("XGBoost Model Summary\n")
          cat("---------------------\n")
          cat("Number of boosting rounds:", input$xgb_trees, "\n")
          cat("Max depth:", input$xgb_depth, "\n")
          cat("Learning rate:", input$xgb_lr, "\n")
          cat("Min node size:", input$xgb_min_n, "\n")
          cat("Sample size:", input$xgb_sample, "\n")
        } else {
          print(model)
        }
      }, error = function(e) {
        cat("Model summary not available.\n")
        cat("Error:", e$message)
      })
    })
  })
}