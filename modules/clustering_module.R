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
              p("Then move to Cluster Analysis.")
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "Correlation Matrix of Selected Clustering Features",
              width = 9,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("corr_plot"), height = "500px")
            )
          ),
        )
      ),
      
      tabPanel(
        "Cluster Analysis",
        
        fluidRow(
          
          # LEFT PANEL
          column(
            width = 4,
            
            bs4Card(
              title = "Clustering Controls",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              
              sliderInput(
                ns("k"),
                "Number of Clusters",
                min = 2,
                max = 10,
                value = 4,
                step = 1
              ),
              
              br(),
              
              actionButton(ns("run_cluster"), "Run Clustering", class = "btn-teal"),
              br(), br(),
              
              downloadButton(ns("export_clusters"), "Export Cluster Groupings")
            ),
            
            bs4Card(
              title = "Elbow Plot",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("elbow_plot"), height = "280px")
            ),
            
            bs4Card(
              title = "Average Silhouette Score by Number of Clusters",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("silhouette_plot"), height = "300px"),
              p("Silhouette is computed on a sample for speed.")
            )
          ),
          
          # RIGHT PANEL
          column(
            width = 8,
            
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Parallel Plot by Cluster Mean",
                  width = 12,
                  status = "teal",
                  solidHeader = FALSE,
                  plotOutput(ns("parallel_plot"), height = "300px")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Customer Cluster Visualisation",
                  width = 12,
                  status = "teal",
                  solidHeader = FALSE,
                  plotOutput(ns("pca_plot"), height = "300px")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Number of Customers by Cluster",
                  width = 12,
                  status = "teal",
                  solidHeader = FALSE,
                  plotOutput(ns("cluster_size_plot"), height = "300px")
                )
              ),
              column(
                width = 12,
                bs4Card(
                  title = "Cluster Feature Heatmap",
                  width = 12,
                  status = "teal",
                  solidHeader = FALSE,
                  plotOutput(ns("heatmap_plot"), height = "300px")
                )
              )
            )
          )
        )
      )
    )
  )
}

clustering_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # store the confirmed feature set after user clicks Update Features
    selected_features <- reactiveVal(NULL)
    
    # numeric columns from uploaded CSV
    numeric_cols <- reactive({
      req(data())
      df <- data()
      names(df)[sapply(df, is.numeric)]
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
    
    # set a default feature set once data is available
    observe({
      req(data())
      req(length(numeric_cols()) >= 2)
      
      if (is.null(selected_features())) {
        selected_features(numeric_cols()[1:min(4, length(numeric_cols()))])
      }
    })
    
    # update confirmed features only when button is clicked
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
    
    # correlation matrix plot
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
    
    # elbow plot
    output$elbow_plot <- renderPlot({
      req(scaled_features())
      
      max_k <- min(input$k, nrow(scaled_features()) - 1)
      
      validate(
        need(max_k >= 2, "Not enough rows to compute elbow plot.")
      )
      
      wcss <- numeric(max_k)
      
      set.seed(888)
      for (k in 1:max_k) {
        km <- kmeans(
          scaled_features(),
          centers = k,
          nstart = 10,
          iter.max = 200,
          algorithm = "Lloyd"
        )
        wcss[k] <- km$tot.withinss
      }
      
      elbow_df <- data.frame(
        k = 1:max_k,
        wcss = wcss
      )
      
      ggplot(elbow_df, aes(x = k, y = wcss)) +
        geom_line() +
        geom_point(size = 2) +
        scale_x_continuous(breaks = 1:max_k) +
        labs(
          title = "Elbow Plot of K Means Clustering",
          x = "Number of Clusters",
          y = "Within Cluster SSE"
        ) +
        theme_minimal()
    }, height = 280)
    
    # silhouette data runs only when button is clicked
    silhouette_data <- reactive({
      req(scaled_features())
      req(input$k)
      
      sf <- scaled_features()
      
      if (nrow(sf) > 1000) {
        set.seed(888)
        sf <- sf[sample(nrow(sf), 1000), , drop = FALSE]
      }
      
      max_k <- min(input$k, nrow(sf) - 1)
      
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
    
    # Add best k value box
    output$best_k_box <- renderbs4ValueBox({
      req(silhouette_data())
      
      best_k <- silhouette_data()$k[which.max(silhouette_data()$avg_silhouette)]
      best_score <- max(silhouette_data()$avg_silhouette)
      
      bs4ValueBox(
        value = best_k,
        subtitle = paste0("Recommended K (Silhouette = ", round(best_score, 3), ")"),
        color = "teal",
        icon = icon("chart-line")
      )
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
    
    # run clustering only when button is clicked
    cluster_result <- eventReactive(input$run_cluster, {
      req(scaled_features())
      
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
      
      # reorder cluster labels by cluster size, largest cluster becomes 1
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
        clustered_df = clustered_df
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
    }, height = 300)
    
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
    }, height = 300)
    
    # cluster size plot
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
    
    # cluster feature heatmap based on median values
    output$heatmap_plot <- renderPlot({
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
    }, height = 300)
    
    # export clustered data
    output$export_clusters <- downloadHandler(
      filename = function() {
        paste0("cluster_groupings_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(cluster_result())
        write.csv(cluster_result()$clustered_df, file, row.names = FALSE)
      }
    )
    
  })
}