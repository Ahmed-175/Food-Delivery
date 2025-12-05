# server.R
library(shiny)
library(rpart)
library(rpart.plot)
library(factoextra)
library(dplyr)

source("R/cleaning.R")
source("R/kmeans.R")
source("R/tree.R")

server <- function(input, output, session) {
  # Keep raw_df for "before cleaning" plots and for debugging
  raw_df <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })

  # data() now returns the LIST produced by clean_data()
  data <- reactive({
    req(raw_df())
    clean_data(raw_df())
  })

  # Update select input choices to numeric columns available after cleaning
  observe({
    req(data())
    cols <- names(data()$outliers_ui)
    # If there are no numeric columns, provide a safe default
    if (length(cols) == 0) cols <- character(0)
    updateSelectInput(session, "col", choices = cols, selected = if (length(cols) > 0) cols[1] else NULL)
  })

  # Render cleaned data table (show cleaned_df)
  output$data <- renderTable(
    {
      req(data())
      head(data()$cleaned_df, 50) # show first 50 rows to avoid huge table
    },
    striped = TRUE,
    hover = TRUE
  )

  # Histogram for selected numeric column (from cleaned_df)
  output$graph <- renderPlot({
    req(input$col)
    req(input$run)
    req(data())
    isolate({
      vec <- data()$cleaned_df[[input$col]]
      # ensure it's numeric
      if (!is.numeric(vec)) {
        plot.new()
        title(main = paste("Column", input$col, "is not numeric"))
        return(NULL)
      }
      hist(
        vec,
        breaks = ifelse(is.null(input$bins), 10, input$bins),
        col = "lightblue",
        border = "white",
        main = paste("Distribution of", input$col),
        xlab = input$col
      )
    })
  })

  # K-means clusters (pass cleaned_df to perform_kmeans)
  clusters <- reactive({
    req(data())
    perform_kmeans(data()$cleaned_df, k = input$k)
  })

  output$cluster_table <- renderTable(
    {
      req(clusters())
      head(
        clusters()$data[, c("Order_ID", "cluster", "Distance_km", "Delivery_Time_min", "Speed_kmph")],
        20
      )
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
          tags$p(paste("Number of Orders:", summary_df$n_orders[i]))
        )
      })
    )
  })

  # Classification tree (use cleaned_df)
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

  output$box_before <- renderPlot({
    req(raw_df())
    par(mfrow = c(2, 2))
    raw <- raw_df()
    if ("Distance_km" %in% names(raw)) boxplot(as.numeric(raw$Distance_km), main = "Distance_km")
    if ("Preparation_Time_min" %in% names(raw)) boxplot(as.numeric(raw$Preparation_Time_min), main = "Prep Time")
    if ("Courier_Experience_yrs" %in% names(raw)) boxplot(as.numeric(raw$Courier_Experience_yrs), main = "Experience")
    if ("Delivery_Time_min" %in% names(raw)) boxplot(as.numeric(raw$Delivery_Time_min), main = "Delivery Time")
  })
}
