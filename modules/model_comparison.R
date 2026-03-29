pacman::p_load(
  shiny, bs4Dash, DT, dplyr, ggplot2, rpart, xgboost, randomForest
)

comparison_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      bs4Card(
        title = "Compare Regression Models",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        actionButton(ns("clear_models"), "Clear Saved Models", class = "btn-danger"),
        br(), br(),
        verbatimTextOutput(ns("saved_models_info"))
      ),
      
      column(
        width = 8,
        
        fluidRow(
          bs4ValueBoxOutput(ns("best_r2_box"), width = 4),
          bs4ValueBoxOutput(ns("best_rmse_box"), width = 4),
          bs4ValueBoxOutput(ns("best_mse_box"), width = 4)
        ),
        
        fluidRow(
          bs4Card(
            title = "Model Comparison Table",
            width = 12,
            status = "teal",
            solidHeader = FALSE,
            DTOutput(ns("comparison_table"))
          )
        )
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "R² Comparison",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("r2_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "RMSE Comparison",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("rmse_plot"), height = "300px")
      ),
      
      bs4Card(
        title = "MSE Comparison",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("mse_plot"), height = "300px")
      )
    ),
    
    fluidRow(
      bs4Card(
        title = "Actual vs Predicted by Model",
        width = 8,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("actual_pred_plot"), height = "420px")
      ),
      
      bs4Card(
        title = "Residual Distribution by Model",
        width = 4,
        status = "teal",
        solidHeader = FALSE,
        plotOutput(ns("residual_plot"), height = "420px")
      )
    )
  )
}

model_colors <- c(
  "Linear Regression" = "#5B8FF9",
  "Regression Tree"   = "#61DDAA",
  "Random Forest"     = "#65789B",
  "XGBoost"           = "#F6BD16"
)

chart_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

comparison_server <- function(id, data, saved_models) {
  moduleServer(id, function(input, output, session) {
    
    
    comparison_results <- reactive({
      req(length(saved_models$results) > 0)
      
      model_list <- saved_models$results
      
      metrics_df <- dplyr::bind_rows(lapply(model_list, function(x) {
        data.frame(
          Model = x$model_type,
          Target = x$target,
          R2 = x$r2,
          MSE = x$mse,
          RMSE = x$rmse
        )
      }))
      
      preds_df <- dplyr::bind_rows(lapply(model_list, function(x) {
        data.frame(
          Model = x$model_type,
          Actual = x$actual,
          Predicted = x$predictions,
          Residual = x$actual - x$predictions
        )
      }))
      
      list(
        metrics = metrics_df,
        predictions = preds_df
      )
    })
    
    observeEvent(input$clear_models, {
      saved_models$results <- list()
      showNotification("Saved model comparison results cleared.", type = "message")
    })
    
    output$saved_models_info <- renderText({
      if (length(saved_models$results) == 0) {
        "No saved models yet. Save models from the Model Experimentation tab."
      } else {
        paste("Saved models:", paste(names(saved_models$results), collapse = ", "))
      }
    })
    output$best_r2_box <- renderbs4ValueBox({
      req(comparison_results())
      m <- comparison_results()$metrics
      best <- m[which.max(m$R2), ]
      
      bs4ValueBox(
        value = paste0(best$Model, " (", sprintf("%.2f", best$R2), ")"),
        subtitle = HTML("Best R<sup>2</sup>"),
        color = "primary",
        icon = icon("trophy")
      )
    })
    
    output$best_rmse_box <- renderbs4ValueBox({
      req(comparison_results())
      m <- comparison_results()$metrics
      best <- m[which.min(m$RMSE), ]
      
      bs4ValueBox(
        value = paste0(best$Model, " (", sprintf("%.2e", best$RMSE), ")"),
        subtitle = "Lowest RMSE",
        color = "success",
        icon = icon("bullseye")
      )
    })
    
    output$best_mse_box <- renderbs4ValueBox({
      req(comparison_results())
      m <- comparison_results()$metrics
      best <- m[which.min(m$MSE), ]
      
      bs4ValueBox(
        value = paste0(best$Model, " (", sprintf("%.2e", best$MSE), ")"),
        subtitle = "Lowest MSE",
        color = "info",
        icon = icon("calculator")
      )
    })
    
    output$comparison_table <- renderDT({
      req(comparison_results())
      
      datatable(
        comparison_results()$metrics,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
    
    output$r2_plot <- renderPlot({
      req(comparison_results())
      df <- comparison_results()$metrics
      
      ggplot(df, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = round(R2, 3)), hjust = -0.1, size = 4) +
        coord_flip() +
        scale_fill_manual(values = model_colors) +
        labs(
          title = expression(R^2 ~ "Comparison"),
          x = NULL,
          y = expression(R^2)
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        expand_limits(y = max(df$R2) * 1.15)
    })
    
    output$rmse_plot <- renderPlot({
      req(comparison_results())
      df <- comparison_results()$metrics
      
      ggplot(df, aes(x = reorder(Model, -RMSE), y = RMSE, fill = Model)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = sprintf("%.2e", RMSE)), hjust = -0.1, size = 4)+
        coord_flip() +
        scale_fill_manual(values = model_colors) +
        labs(
          title = "RMSE Comparison",
          x = NULL,
          y = "RMSE"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        expand_limits(y = max(df$RMSE) * 1.15)
    })
    
    output$mse_plot <- renderPlot({
      req(comparison_results())
      df <- comparison_results()$metrics
      
      ggplot(df, aes(x = reorder(Model, -MSE), y = MSE, fill = Model)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = signif(MSE, 3)), hjust = -0.1, size = 4) +
        coord_flip() +
        scale_fill_manual(values = model_colors) +
        labs(
          title = "MSE Comparison",
          x = NULL,
          y = "MSE"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        expand_limits(y = max(df$MSE) * 1.15)
    })
    
    output$actual_pred_plot <- renderPlot({
      req(comparison_results())
      df <- comparison_results()$predictions
      
      ggplot(df, aes(x = Actual, y = Predicted, color = Model)) +
        geom_point(alpha = 0.65, size = 2) +
        geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
        facet_wrap(~ Model, scales = "free") +
        scale_color_manual(values = model_colors) +
        labs(
          title = "Actual vs Predicted by Model",
          x = "Actual",
          y = "Predicted"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold")
        )
    })
    
    output$residual_plot <- renderPlot({
      req(comparison_results())
      df <- comparison_results()$predictions
      
      ggplot(df, aes(x = Model, y = Residual, fill = Model, color = Model)) +
        geom_violin(alpha = 0.25, trim = FALSE) +
        geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
        geom_jitter(width = 0.08, alpha = 0.25, size = 1.2) +
        coord_flip() +
        scale_fill_manual(values = model_colors) +
        scale_color_manual(values = model_colors) +
        labs(
          title = "Residual Distribution by Model",
          x = NULL,
          y = "Residual"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    })
  })
}