# server.R
library(shiny)
library(rpart)
library(rpart.plot)
library(factoextra)
library(dplyr)

source("R/cleaning.R")
source("R/kmeans.R")
source("R/tree.R")
source("R/visualize_functions.R")

server <- function(input, output) {
  # ===== Raw Data =====
  raw_df <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })

  # ===== Cleaned Data =====
  data <- reactive({
    req(raw_df())
    clean_data(raw_df())
  })

  # ===== Show Data Table =====
  output$data <- renderTable(
    {
      req(data())
      head(data()$cleaned_df, 50)
    },
    striped = TRUE,
    hover = TRUE
  )

  # ===== K-Means =====
  clusters <- reactive({
    req(data())
    perform_kmeans(data()$cleaned_df, k = input$k)
  })

  output$cluster_table <- renderTable(
    {
      req(clusters())
      head(clusters()$data[, c(
        "Order_ID", "cluster", "Distance_km",
        "Delivery_Time_min", "Speed_kmph"
      )], 10)
    },
    striped = TRUE
  )

  output$cluster_plot <- renderPlot({
    req(clusters())
    fviz_cluster(
      clusters()$km,
      data = clusters()$scaled,
      geom = "point",
      stand = FALSE
    )
  })

  # ===== Cluster Summaries =====
  output$cluster_summary_ui <- renderUI({
    req(clusters())
    summary_df <- clusters()$data %>%
      group_by(cluster) %>%
      summarise(
        avg_distance = round(mean(Distance_km, na.rm = TRUE), 2),
        avg_prep_time = round(mean(Preparation_Time_min, na.rm = TRUE), 2),
        avg_experience = round(mean(Courier_Experience_yrs, na.rm = TRUE), 2),
        avg_speed = round(mean(Speed_kmph, na.rm = TRUE), 2),
        avg_delivery = round(mean(Delivery_Time_min, na.rm = TRUE), 2),
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
          tags$p(paste("Orders:", summary_df$n_orders[i]))
        )
      })
    )
  })

  # ===== Classification Tree =====
  reactive_tree <- reactive({
    req(data())
    dt(data()$cleaned_df)
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

  # ===== Regression Tree =====
  reactive_reg_tree <- reactive({
    req(data())
    rpart(
      Delivery_Time_min ~ Traffic_Level + Time_of_Day + Vehicle_Type + Weather,
      data = data()$cleaned_df,
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

  # ===== Boxplots Before Cleaning =====
  output$box_before <- renderPlot({
    req(raw_df())
    par(mfrow = c(2, 2))
    raw <- raw_df()
    boxplot(as.numeric(raw$Distance_km), main = "Distance_km")
    boxplot(as.numeric(raw$Preparation_Time_min), main = "Prep Time")
    boxplot(as.numeric(raw$Courier_Experience_yrs), main = "Experience")
    boxplot(as.numeric(raw$Delivery_Time_min), main = "Delivery Time")
  })

  # ===== Numeric Tendencies =====
  output$tend_plot <- renderPlot({
    req(data()$cleaned_df)
    par(mfrow = c(length(numeric_cols), 1), mar = c(1, 1, 1, 20))
    visualize_data_tend(data()$cleaned_df[["Delivery_Time_min"]], "Delivery_Time_min")
  })

  # ===== Categorical Plots =====
  output$cat_plots <- renderUI({
    req(data()$cleaned_df)
    cat_cols <- names(data()$cleaned_df)[sapply(data()$cleaned_df, function(x) is.character(x) || is.factor(x))]
    plot_output_list <- lapply(cat_cols, function(col) {
      plotname <- paste0("plot_", col)
      output[[plotname]] <- renderPlot({
        visualize_data_table(data()$cleaned_df[[col]])
      })
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })

  # ===== Relations Plots =====
  output$relation_plots <- renderUI({
    req(data()$cleaned_df)
    numeric_cols <- names(data()$cleaned_df)[sapply(data()$cleaned_df, is.numeric)]
    pairs <- combn(numeric_cols, 2, simplify = FALSE)
    plot_output_list <- lapply(pairs, function(pair) {
      plotname <- paste0("plot_rel_", pair[1], "_", pair[2])
      output[[plotname]] <- renderPlot({
        visualize_data_relation(
          data()$cleaned_df[[pair[1]]],
          data()$cleaned_df[[pair[2]]],
          main = paste(pair[1], "vs", pair[2]),
          xlab = pair[1], ylab = pair[2]
        )
      })
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
}
