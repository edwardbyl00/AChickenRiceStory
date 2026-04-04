clustering_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      id = ns("cluster_tabs"),
      
      tabPanel(
        "Feature Selection",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "Select Features",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              uiOutput(ns("feature_select_ui")),
              br(),
              actionButton(ns("update_features"), "Update Features", class = "btn-primary")
            ),
            bs4Card(
              title = "Guide",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              p("Select at least 2 numeric variables."),
              p("Use Update Features to refresh the correlation matrix."),
              p("Then move to Cluster Selection.")
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "Correlation Matrix of Selected Clustering Features",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("corr_plot"), height = "500px")
            )
          )
        )
      ),
      
      tabPanel(
        "Cluster Selection",
        

        fluidRow(
          column(
            width = 6,
            bs4Card(
              title = "Clustering Controls",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              
              fluidRow(
                column(
                  width = 6,
                  sliderInput(
                    ns("k"),
                    "Number of Clusters",
                    min = 2,
                    max = 10,
                    value = 4,
                    step = 1
                  )
                ),
                column(
                  width = 6,
                  br(),
                  downloadButton(ns("export_clusters"), "Export Cluster Groupings")
                )
              )
            )
          ),
          
          column(
            width = 6,
            bs4Card(
              title = "Number of Customers by Cluster",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("cluster_size_plot"), height = "300px")
            )
          )
        ),
        

        fluidRow(
          column(
            width = 6,
            bs4Card(
              title = "Elbow Plot",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("elbow_plot"), height = "300px")
            )
          ),
          
          column(
            width = 6,
            bs4Card(
              title = "Average Silhouette Score by Number of Clusters",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("silhouette_plot"), height = "300px"),
              p("Silhouette is computed on a sample for speed.")
            )
          )
        )
      ),
      
      tabPanel(
        "Cluster Analysis",
        

        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "Selected K",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              style = "text-align:center; min-height: 150px;",
              br(),
              h2(textOutput(ns("selected_k_text")), style = "font-weight:700; margin-bottom:8px;"),
              p("Number of Clusters", style = "color:#6c757d; margin-bottom:0;")
            )
          ),
          column(
            width = 3,
            bs4Card(
              title = "Silhouette Score",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              style = "text-align:center; min-height: 150px;",
              br(),
              h2(textOutput(ns("selected_silhouette_text")), style = "font-weight:700; margin-bottom:8px;"),
              p("Average Separation", style = "color:#6c757d; margin-bottom:0;")
            )
          ),
          column(
            width = 6,
            bs4Card(
              title = "Clustering Settings",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              style = "min-height: 150px;",
              htmlOutput(ns("selected_features_text"))
            )
          )
        ),
        

        fluidRow(
          bs4Card(
            title = "Cluster Feature Heatmap",
            width = 6,
            status = "teal",
            solidHeader = FALSE,
            maximizable = TRUE,
            
            downloadButton(ns("download_heatmap"), "Download Plot"),
            br(), br(),
            
            plotOutput(ns("heatmap_plot"), height = "320px")
          ),
          bs4Card(
            title = "Number of Customers by Cluster",
            width = 6,
            status = "teal",
            solidHeader = FALSE,
            plotOutput(ns("cluster_size_plot_analysis"), height = "300px")
          )
        ),

        fluidRow(
          column(
            width = 6,
            bs4Card(
              title = "Parallel Plot by Cluster Mean",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              maximizable = TRUE,
              plotOutput(ns("parallel_plot"), height = "320px")
            )
          ),
          
          column(
            width = 6,
            bs4Card(
              title = "Customer Cluster Visualisation",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              maximizable = TRUE,
              plotOutput(ns("pca_plot"), height = "320px")
            )
          )
        )
      )
    )
  )
}

clustering_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # store the confirmed feature set
    selected_features <- reactiveVal(NULL)
    
    # numeric columns from uploaded CSV
    numeric_cols <- reactive({
      req(data())
      names(data())[sapply(data(), is.numeric)]
    })
    
    # feature selector UI
    output$feature_select_ui <- renderUI({
      req(data())
      req(length(numeric_cols()) >= 2)
      
      selectInput(
        inputId = session$ns("feature_vars"),
        label = "Select Clustering Features",
        choices = numeric_cols(),
        selected = numeric_cols()[1:min(4, length(numeric_cols()))],
        multiple = TRUE
      )
    })
    
    # set a default feature set
    observe({
      req(data())
      req(length(numeric_cols()) >= 2)
      
      if (is.null(selected_features())) {
        selected_features(numeric_cols()[1:min(4, length(numeric_cols()))])
      }
    })
    
    # update confirmed features after button clicked
    observeEvent(input$update_features, {
      req(input$feature_vars)
      validate(
        need(length(input$feature_vars) >= 2, "Please select at least 2 numeric variables.")
      )
      selected_features(input$feature_vars)
    })
    
    # data for selected features
    feature_data <- reactive({
      req(data())
      req(selected_features())
      
      df <- data()[, selected_features(), drop = FALSE]
      df <- na.omit(df)
      
      validate(
        need(nrow(df) > 2, "Not enough complete rows after removing missing values.")
      )
      
      df
    })
    
    # scaled features for clustering
    scaled_features <- reactive({
      req(feature_data())
      scale(feature_data())
    })
    
    # correlation matrix
    output$corr_plot <- renderPlot({
      req(feature_data())
      
      corr_mat <- cor(feature_data(), use = "complete.obs")
      corr_df <- as.data.frame(as.table(corr_mat))
      names(corr_df) <- c("Var1", "Var2", "Correlation")
      
      ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(Correlation, 2)), size = 4) +
        scale_fill_gradient2(
          low = "#3b82f6",
          mid = "white",
          high = "#ef4444",
          midpoint = 0,
          limits = c(-1, 1)
        ) +
        labs(
          x = NULL,
          y = NULL,
          fill = "Correlation"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        )
    }, height = 500)
    
    # elbow data
    elbow_data <- reactive({
      req(scaled_features())
      
      sf <- scaled_features()
      max_k <- min(10, nrow(sf) - 1)
      
      validate(
        need(max_k >= 2, "Not enough rows to compute elbow plot.")
      )
      
      wcss <- numeric(max_k)
      
      set.seed(888)
      for (k in 1:max_k) {
        km <- kmeans(
          sf,
          centers = k,
          nstart = 10,
          iter.max = 200,
          algorithm = "Lloyd"
        )
        wcss[k] <- km$tot.withinss
      }
      
      data.frame(
        k = 1:max_k,
        wcss = wcss
      )
    })
    
    # elbow plot
    output$elbow_plot <- renderPlot({
      req(elbow_data())
      
      ggplot(elbow_data(), aes(x = k, y = wcss)) +
        geom_line() +
        geom_point(size = 2) +
        scale_x_continuous(breaks = elbow_data()$k) +
        labs(
          title = "Elbow Plot of K Means Clustering",
          x = "Number of Clusters",
          y = "Within Cluster SSE"
        ) +
        theme_minimal()
    }, height = 300)
    
    # silhouette data
    silhouette_data <- reactive({
      req(scaled_features())
      
      sf <- scaled_features()
      
      if (nrow(sf) > 1000) {
        set.seed(888)
        sf <- sf[sample(nrow(sf), 1000), , drop = FALSE]
      }
      
      max_k <- min(10, nrow(sf) - 1)
      
      validate(
        need(max_k >= 2, "Need at least k >= 2 for silhouette.")
      )
      
      dist_mat <- dist(sf)
      
      sil_summary <- purrr::map_dfr(2:max_k, function(k) {
        set.seed(888)
        
        km <- kmeans(
          sf,
          centers = k,
          nstart = 5,
          iter.max = 200,
          algorithm = "Lloyd"
        )
        
        sil <- cluster::silhouette(km$cluster, dist_mat)
        
        tibble::tibble(
          k = k,
          avg_silhouette = mean(sil[, "sil_width"])
        )
      })
      
      sil_summary
    })
    
    # silhouette plot
    output$silhouette_plot <- renderPlot({
      req(silhouette_data())
      
      best_k <- silhouette_data()$k[which.max(silhouette_data()$avg_silhouette)]
      
      ggplot(silhouette_data(), aes(x = k, y = avg_silhouette)) +
        geom_line() +
        geom_point(size = 2) +
        geom_point(
          data = subset(silhouette_data(), k == best_k),
          size = 4
        ) +
        geom_text(
          aes(label = round(avg_silhouette, 3)),
          vjust = -0.6,
          size = 3
        ) +
        scale_x_continuous(breaks = silhouette_data()$k) +
        theme_minimal() +
        labs(
          title = "Average Silhouette Score by Number of Clusters",
          x = "Number of Clusters (k)",
          y = "Average Silhouette Score"
        )
    }, height = 300)
    
    # clustering updates automatically when inputs change
    cluster_result <- reactive({
      req(scaled_features())
      req(selected_features())
      
      validate(
        need(input$k < nrow(scaled_features()), "k must be smaller than the number of rows used.")
      )
      
      km <- kmeans(
        scaled_features(),
        centers = input$k,
        nstart = 25,
        iter.max = 200,
        algorithm = "Lloyd"
      )
      
      # reorder cluster labels by cluster size desc
      cluster_raw <- factor(km$cluster)
      
      cluster_order <- data.frame(cluster = cluster_raw) %>%
        count(cluster, name = "n") %>%
        arrange(desc(n)) %>%
        mutate(new_cluster = row_number())
      
      clustered_df <- feature_data() %>%
        mutate(cluster = cluster_raw) %>%
        left_join(cluster_order[, c("cluster", "new_cluster")], by = "cluster") %>%
        mutate(cluster = factor(new_cluster)) %>%
        select(-new_cluster)
      
      list(
        km = km,
        clustered_df = clustered_df,
        cluster_order = cluster_order
      )
    })
    
    # writeback on active_data
    observeEvent(cluster_result(), {
      req(data())
      req(selected_features())
      req(cluster_result())
      
      df <- data()
      
      complete_idx <- complete.cases(df[, selected_features(), drop = FALSE])
      
      df$cluster <- NA
      df$cluster[complete_idx] <- as.character(cluster_result()$clustered_df$cluster)
      df$cluster <- as.factor(df$cluster)
      
      data(df)
    })
    
    # selected k value box
    output$selected_k_text <- renderText({
      req(cluster_result())
      input$k
    })
    
    output$selected_silhouette_text <- renderText({
      req(cluster_result())
      req(silhouette_data())
      
      sil_df <- silhouette_data()
      selected_score <- sil_df$avg_silhouette[sil_df$k == input$k]
      
      validate(
        need(length(selected_score) == 1, "Silhouette score not available for selected k.")
      )
      
      round(selected_score, 3)
    })
    
    # selected silhouette score value box
    output$selected_silhouette_box <- renderbs4ValueBox({
      req(cluster_result())
      req(silhouette_data())
      
      sil_df <- silhouette_data()
      selected_score <- sil_df$avg_silhouette[sil_df$k == input$k]
      
      validate(
        need(length(selected_score) == 1, "Silhouette score not available for selected k.")
      )
      
      bs4ValueBox(
        value = round(selected_score, 3),
        subtitle = "Silhouette",
        color = "teal",
        icon = icon("chart-line")
      )
    })
    
    # selected features text
    output$selected_features_text <- renderUI({
      req(cluster_result())
      req(selected_features())
      
      tags$div(
        style = "padding-top: 10px; font-size: 15px; line-height: 1.8;",
        HTML(paste0(
          "<b>Features used:</b> ", paste(selected_features(), collapse = ", "),
          "<br><b>Selected k:</b> ", input$k
        ))
      )
    })
    
    # parallel plot by cluster mean
    output$parallel_plot <- renderPlot({
      req(cluster_result())
      
      cluster_mean <- cluster_result()$clustered_df %>%
        group_by(cluster) %>%
        summarise(across(all_of(selected_features()), mean), .groups = "drop")
      
      plot_df <- cluster_mean %>%
        tidyr::pivot_longer(-cluster, names_to = "feature", values_to = "value") %>%
        group_by(feature) %>%
        mutate(value_scaled = scales::rescale(value, to = c(0, 1))) %>%
        ungroup()
      
      ggplot(plot_df, aes(x = feature, y = value_scaled, color = cluster, group = cluster)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(
          x = NULL,
          y = "Scaled Mean",
          color = "Cluster"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1)
        )
    }, height = 320)
    
    # PCA cluster visualisation
    output$pca_plot <- renderPlot({
      req(cluster_result())
      
      pca_res <- prcomp(scaled_features())
      pca_df <- as.data.frame(pca_res$x[, 1:2])
      pca_df$cluster <- cluster_result()$clustered_df$cluster
      
      ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(alpha = 0.6, size = 2) +
        labs(
          x = "PC1",
          y = "PC2",
          color = "Cluster"
        ) +
        theme_minimal()
    }, height = 320)
    
    # cluster size plot for selection tab
    output$cluster_size_plot <- renderPlot({
      req(cluster_result())
      
      cluster_size <- cluster_result()$clustered_df %>%
        count(cluster) %>%
        mutate(pct = n / sum(n))
      
      ggplot(cluster_size, aes(x = cluster, y = n, fill = cluster)) +
        geom_col() +
        geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.3) +
        labs(
          x = "Cluster",
          y = "Customers",
          fill = "Cluster"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }, height = 300)
    
    # cluster size plot for analysis tab
    output$cluster_size_plot_analysis <- renderPlot({
      req(cluster_result())
      
      cluster_size <- cluster_result()$clustered_df %>%
        count(cluster) %>%
        mutate(pct = n / sum(n))
      
      ggplot(cluster_size, aes(x = cluster, y = n, fill = cluster)) +
        geom_col() +
        geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.3) +
        labs(
          x = "Cluster",
          y = "Customers",
          fill = "Cluster"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }, height = 300)
    
    # cluster feature heatmap based on median values
    heatmap_plot_obj <- reactive({
      req(cluster_result())
      
      cluster_median <- cluster_result()$clustered_df %>%
        group_by(cluster) %>%
        summarise(across(all_of(selected_features()), median), .groups = "drop")
      
      cluster_normalised <- cluster_median %>%
        mutate(across(-cluster, ~ scales::rescale(., to = c(0, 1))))
      
      cluster_long <- cluster_normalised %>%
        tidyr::pivot_longer(-cluster, names_to = "feature", values_to = "value")
      
      ggplot(cluster_long, aes(x = feature, y = cluster, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradientn(colours = c("#e5f5f9", "#99d8c9", "#2ca25f")) +
        labs(
          x = NULL,
          y = "Cluster",
          fill = "Relative\nIntensity"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 25, hjust = 1)
        )
    })
    
    # render
    output$heatmap_plot <- renderPlot({
      heatmap_plot_obj()
    }, height = 320)
    
    # download heatmap
    output$download_heatmap <- downloadHandler(
      filename = function() {
        paste0("cluster_heatmap_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = heatmap_plot_obj(),
          width = 10,
          height = 5,
          dpi = 300
        )
      }
    )
    
    # export clustered data
    output$export_clusters <- downloadHandler(
      filename = function() {
        paste0("cluster_groupings_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(data())
        req("cluster" %in% names(data()))
        write.csv(data(), file, row.names = FALSE)
      }
    )
    
  })
}