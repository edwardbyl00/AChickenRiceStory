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
            )
          ),
          
          column(
            width = 7,
            bs4Card(
              title = "Correlation Matrix of Selected Clustering Features",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              plotOutput(ns("corr_plot"), height = "500px")
            )
          ),
          
          column(
            width = 2,
            bs4Card(
              title = "Guide",
              width = 12,
              status = "teal",
              solidHeader = FALSE,
              p("Select at least 2 numeric variables."),
              p("Use Update Features to refresh the correlation matrix."),
              p("Then move to Cluster Analysis.")
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
              title = "Clustering Controls",
              width = 12,
              status = "primary",
              solidHeader = FALSE,
              
              sliderInput(
                ns("k"),
                "Number of Clusters",
                min = 2,
                max = 10,
                value = 4,
                step = 1
              ),
              
              actionButton(ns("run_cluster"), "Run Clustering", class = "btn-primary"),
              br(), br(),
              downloadButton(ns("export_clusters"), "Export Cluster Groupings")
            )
          ),
          
          column(
            width = 9,
            fluidRow(
              column(
                width = 6,
                bs4Card(
                  title = "Parallel Plot by Cluster Mean",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput(ns("parallel_plot"), height = "300px")
                )
              ),
              column(
                width = 6,
                bs4Card(
                  title = "Customer Cluster Visualisation",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput(ns("pca_plot"), height = "300px")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 6,
                bs4Card(
                  title = "Number of Customers by Cluster",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput(ns("cluster_size_plot"), height = "300px")
                )
              ),
              column(
                width = 6,
                bs4Card(
                  title = "Cluster Feature Heatmap",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput(ns("heatmap_plot"), height = "300px")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                bs4Card(
                  title = "Elbow Plot",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput(ns("elbow_plot"), height = "280px")
                )
              )
            )
          )
        )
      )
    )
  )
}