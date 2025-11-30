library(shiny)
library(dplyr)
library(factoextra)

source("R/cleaning.R")
source("R/kmeans.R")

ui <- fluidPage(
  titlePanel("dotR Delivery Analysis"),
  fluidRow(
    # Sidebar fixed on left
    column(
      width = 3,
      wellPanel(
        h3("⚙️ Controls"),
        fileInput("file", "Upload CSV", accept = ".csv"),
        selectInput("col", "Choose Column for Histogram", choices = NULL),
        numericInput("bins", "Number of Bins", value = 15, min = 5, max = 50),
        numericInput("k", "Number of Clusters", value = 3, min = 2, max = 10),
        actionButton("run", "Generate")
      )
    ),
    # Main panel with Tabs
    column(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Data",
          h3("Uploaded Data"),
          tableOutput("data")
        ),
        tabPanel(
          "Histogram",
          h3("Histogram"),
          plotOutput("graph", height = "400px")
        ),
        tabPanel(
          "K-Means Clustering",
          h3("Cluster Table"),
          tableOutput("cluster_table"),
          h3("Cluster Plot"),
          plotOutput("cluster_plot", height = "400px")
        ),
        tabPanel(
          "Cluster Summary",
          h3("Cluster Summary"),
          uiOutput("cluster_summary_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    clean_data(df)
  })

  observe({
    req(data())
    updateSelectInput(session, "col", choices = names(data()))
  })

  output$data <- renderTable({
    data()
  })

  output$graph <- renderPlot({
    req(input$col)
    req(input$run)
    isolate({
      hist(
        data()[[input$col]],
        breaks = input$bins,
        col = "lightblue",
        border = "white",
        main = paste("Distribution of", input$col),
        xlab = input$col
      )
    })
  })

  clusters <- reactive({
    req(data())
    perform_kmeans(data(), k = input$k)
  })

  output$cluster_table <- renderTable({
    req(clusters())
    head(
      clusters()$data[
        ,
        c("Order_ID", "cluster", "Distance_km", "Delivery_Time_min", "Speed_kmph")
      ],
      20
    )
  })

  output$cluster_plot <- renderPlot({
    req(clusters())
    fviz_cluster(
      clusters()$km,
      data = clusters()$scaled,
      geom = "point",
      stand = FALSE
    )
  })

  output$cluster_summary_ui <- renderUI({
    req(clusters())

    summary_df <- clusters()$data %>%
      group_by(cluster) %>%
      summarise(
        avg_distance = round(mean(Distance_km), 2),
        avg_prep_time = round(mean(Preparation_Time_min), 2),
        avg_experience = round(mean(Courier_Experience_yrs), 2),
        avg_speed = round(mean(Speed_kmph), 2),
        avg_delivery = round(mean(Delivery_Time_min), 2),
        n_orders = n(),
        .groups = "drop"
      )

    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 15px;",
      lapply(1:nrow(summary_df), function(i) {
        tags$div(
          style = "flex: 1 1 200px; border: 1px solid #ccc; padding: 10px;",
          tags$h4(paste("Cluster", summary_df$cluster[i])),
          tags$p(paste("Avg Distance:", summary_df$avg_distance[i], "km")),
          tags$p(paste("Avg Prep Time:", summary_df$avg_prep_time[i], "min")),
          tags$p(paste("Avg Experience:", summary_df$avg_experience[i], "yrs")),
          tags$p(paste("Avg Speed:", summary_df$avg_speed[i], "km/min")),
          tags$p(paste("Avg Delivery:", summary_df$avg_delivery[i], "min")),
          tags$p(paste("Number of Orders:", summary_df$n_orders[i]))
        )
      })
    )
  })
}

shinyApp(ui = ui, server = server)
