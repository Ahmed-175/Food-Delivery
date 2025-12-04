library(shiny)
library(rpart)
library(rpart.plot)
library(factoextra)
library(dplyr)

source("R/cleaning.R")
source("R/kmeans.R")
source("R/tree.R")

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

  reactive_tree <- reactive({
    req(data())
    dt(data())
  })

  output$treePlot <- renderPlot({
    req(reactive_tree())
    rpart.plot(
      reactive_tree(),
      main = "Classification Tree - Late Delivery",
      type = 2,
      extra = 104,
      fallen.leaves = TRUE
    )
  })

  reactive_reg_tree <- reactive({
    req(data())
    rpart(
      Delivery_Time_min ~
        Traffic_Level + Time_of_Day + Vehicle_Type + Weather,
      data = data(),
      method = "anova",
      control = rpart.control(minsplit = 5)
    )
  })
  output$treePlotreg <- renderPlot({
    req(reactive_reg_tree())
    rpart.plot(
      reactive_reg_tree(),
      main = "Regression Tree - Expected Delivery Time",
      type = 4,
      extra = 101,
      fallen.leaves = TRUE
    )
  })
}
