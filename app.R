# app.R
library(shiny)
library(dplyr)
library(factoextra)
library(arules)

source("R/cleaning.R")
source("R/kmeans.R")
source("R/apriori.R")

ui <- fluidPage(
  titlePanel("dotR Delivery Analysis"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h3(" Controls"),
        fileInput("file", "Upload CSV", accept = ".csv"),
        selectInput("col", "Choose Column for Histogram", choices = NULL),
        numericInput("bins", "Number of Bins", value = 15, min = 5, max = 50),
        numericInput("k", "Number of Clusters", value = 3, min = 2, max = 10),
        actionButton("run", "Generate"),
        hr(),
        h4("Association Rules Settings"),
        numericInput("support", "Min Support", value = 0.1, min = 0, max = 1, step = 0.01),
        numericInput("confidence", "Min Confidence", value = 0.7, min = 0, max = 1, step = 0.01),
        actionButton("generate_rules", "Generate Rules")
      )
    ),
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
        ),
        tabPanel(
          "Association Rules",
          h3("Association Rules"),
          tableOutput("rules_table")
        ),
        tabPanel(
          "Apriori Algorithm",
          h3("Apriori Algorithm"),
          tableOutput("apriori")
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
      clusters()$data[, c("Order_ID", "cluster", "Distance_km", "Delivery_Time_min", "Speed_kmph")]
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
          tags$p(paste("Avg Speed:", summary_df$avg_speed[i], "km/h")),
          tags$p(paste("Avg Delivery:", summary_df$avg_delivery[i], "min")),
          tags$p(paste("Number of Orders:", summary_df$n_orders[i]))
        )
      })
    )
  })

  rules_data <- eventReactive(input$generate_rules, {
    req(data())

    df_rules <- data()[, c("Weather", "Traffic_Level", "Vehicle_Type", "Late_Delivery")]
    trans <- as(df_rules, "transactions")

    rules <- apriori(trans,
      parameter = list(support = input$support, confidence = input$confidence)
    )

    rules_df <- as(rules, "data.frame")
    rules_df <- rules_df[, c("rules", "support", "confidence", "lift")]
    return(rules_df)
  })

  output$rules_table <- renderTable({
    rules_data()
  })


  apriori <- reactive({
    req(clusters())
    perform_apriori(
      df = data(),
      support = 0.01,
      confidence = 0.4,
      clusters = clusters()$data$cluster # vector of cluster labels
    )
  })
  output$apriori <- renderTable({
    req(apriori())
    df_rules <- as(apriori()$rules, "data.frame")
    head(df_rules)
  })
}

shinyApp(ui = ui, server = server)
