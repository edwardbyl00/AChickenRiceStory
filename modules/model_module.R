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
          choices = c("Linear Regression", "Random Forest")
        ),
        
        uiOutput(ns("target_ui")),
        uiOutput(ns("predictors_ui")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Random Forest'", ns("model_type")),
          sliderInput(ns("n_trees"), "Number of Trees", min = 50, max = 500, value = 100),
          sliderInput(ns("max_depth"), "Maximum Depth", min = 2, max = 20, value = 6)
        ),
        
        actionButton(ns("run_model"), "Run Experiment", class = "btn-primary")
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
            width = 7,
            status = "teal",
            solidHeader = FALSE,
            DT::DTOutput(ns("metrics_table"))
          ),
          
          bs4Card(
            title = "Feature Importance",
            width = 5,
            status = "teal",
            solidHeader = FALSE,
            plotOutput(ns("feature_plot_top"), height = "250px")
          )
        )
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "Feature Importance",
        width = 4,
        status = "primary",
        solidHeader = FALSE,
        plotOutput(ns("feature_plot_bottom"), height = "300px")
      ),
      
      bs4Card(
        title = "Actual vs Predicted",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("scatter_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "Model Summary",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        verbatimTextOutput(ns("model_summary"))
      )
    )
  )
}

model_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    numeric_cols <- reactive({
      req(data())
      names(data())[sapply(data(), is.numeric)]
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
      req(data())
      req(length(numeric_cols()) >= 2)
      
      default_predictors <- numeric_cols()[numeric_cols() != numeric_cols()[1]]
      
      selectInput(
        inputId = session$ns("predictors"),
        label = "Select Predictor Variables",
        choices = numeric_cols(),
        selected = default_predictors,
        multiple = TRUE
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
      
      if (input$model_type == "Linear Regression") {
        model <- lm(model_formula, data = model_data())
      } else {
        if (!requireNamespace("randomForest", quietly = TRUE)) {
          stop("Please install the randomForest package.")
        }
        
        model <- randomForest::randomForest(
          formula = model_formula,
          data = model_data(),
          ntree = input$n_trees
        )
      }
      
      preds <- predict(model, newdata = model_data())
      actual <- model_data()[[input$target]]
      
      mse <- mean((actual - preds)^2)
      rmse <- sqrt(mse)
      r2 <- 1 - sum((actual - preds)^2) / sum((actual - mean(actual))^2)
      
      list(
        model = model,
        predictions = preds,
        actual = actual,
        mse = mse,
        rmse = rmse,
        r2 = r2,
        predictors = predictors
      )
    })
    
    output$r2_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = round(fitted_model()$r2, 3),
        subtitle = HTML("R<sup>2</sup>"),
        status = "primary",
        icon = icon("chart-line")
      )
    })
    
    output$mse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = format(round(fitted_model()$mse, 2), big.mark = ","),
        subtitle = "MSE",
        status = "info",
        icon = icon("calculator")
      )
    })
    
    output$rmse_box <- renderbs4ValueBox({
      req(fitted_model())
      bs4ValueBox(
        value = format(round(fitted_model()$rmse, 2), big.mark = ","),
        subtitle = "RMSE",
        status = "warning",
        icon = icon("square-root-alt")
      )
    })
    
    output$metrics_table <- DT::renderDT({
      req(fitted_model())
      
      results_df <- data.frame(
        Actual = fitted_model()$actual,
        Predicted = fitted_model()$predictions,
        Residual = fitted_model()$actual - fitted_model()$predictions
      )
      
      DT::datatable(
        head(results_df, 20),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
    
    output$feature_plot_top <- renderPlot({
      req(fitted_model())
      
      if (input$model_type == "Linear Regression") {
        coefs <- coef(fitted_model()$model)
        coefs <- coefs[names(coefs) != "(Intercept)"]
        importance_df <- data.frame(
          feature = names(coefs),
          importance = abs(as.numeric(coefs))
        )
      } else {
        importance_df <- data.frame(
          feature = rownames(fitted_model()$model$importance),
          importance = fitted_model()$model$importance[, 1]
        )
      }
      
      importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
      
      par(mar = c(5, 8, 3, 2))
      barplot(
        rev(importance_df$importance),
        names.arg = rev(importance_df$feature),
        horiz = TRUE,
        las = 1,
        col = "steelblue",
        main = "Top Features"
      )
    })
    
    output$feature_plot_bottom <- renderPlot({
      req(fitted_model())
      
      if (input$model_type == "Linear Regression") {
        coefs <- coef(fitted_model()$model)
        coefs <- coefs[names(coefs) != "(Intercept)"]
        importance_df <- data.frame(
          feature = names(coefs),
          importance = abs(as.numeric(coefs))
        )
      } else {
        importance_df <- data.frame(
          feature = rownames(fitted_model()$model$importance),
          importance = fitted_model()$model$importance[, 1]
        )
      }
      
      importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
      
      par(mar = c(5, 8, 3, 2))
      barplot(
        rev(importance_df$importance),
        names.arg = rev(importance_df$feature),
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
      summary(fitted_model()$model)
    })
    
  })
}
