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
source("R/description_fun.R")

server <- function(input, output) {
  # Keep raw_df for "before cleaning" plots and for debugging
  raw_df <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })

  data <- reactive({
    req(raw_df())
    clean_data(raw_df())
  })
  clusters <- reactive({
    req(data())
    perform_kmeans(data()$cleaned_df, k = input$k)
  })

  output$data <- renderTable(
    {
      req(data())
      head(data()$cleaned_df, 50)
    },
    striped = TRUE,
    hover = TRUE
  )

  output$cluster_table <- renderTable(
    {
      req(clusters())
      head(
        clusters()$data[, c("Order_ID", "cluster", "Distance_km", "Delivery_Time_min", "Speed_kmph")],
        10
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
          tags$p(paste("Number of Orders:", summary_df$n_orders[i])),
          tags$p(paste(
            description_fun_distance(summary_df$avg_distance[i], as.numeric(mean(data()$cleaned_df$Distance_km))), ",",
            description_fun_delivery_time(summary_df$avg_delivery[i], as.numeric(mean(data()$cleaned_df$Delivery_Time_min))), ",",
            description_fun_experience(summary_df$avg_experience[i], as.numeric(mean(data()$cleaned_df$Courier_Experience_yrs)))
          ))
        )
      })
    )
  })
  split_data <- reactive({
    req(data())
    splitted_data(data()$cleaned_df)
  })
  reactive_tree <- reactive({
    req(data())
    dt(
      split_data()$training_data,
      split_data()$testing_data
    )
  })

  output$treePlot <- renderPlot({
    req(reactive_tree()$model)
    rpart.plot(
      reactive_tree()$model,
      main = "Classification Tree - Late Delivery",
      type = 2,
      extra = 104,
      fallen.leaves = TRUE
    )
  })
  output$tree_predictions <- renderTable({
    req(reactive_tree()$predictions)
    test_data <- split_data()$testing_data

    # Calculate accuracy
    predictions <- reactive_tree()$predictions
    actual <- test_data$Late_Delivery

    # Confusion matrix
    confusion <- table(Actual = actual, Predicted = predictions)

    # Calculate metrics
    accuracy <- sum(diag(confusion)) / sum(confusion) * 100
    precision <- confusion[2, 2] / sum(confusion[, 2]) * 100
    recall <- confusion[2, 2] / sum(confusion[2, ]) * 100

    data.frame(
      Metric = c("Accuracy", "Precision", "Recall", "Total Test Cases"),
      Value = c(
        paste0(round(accuracy, 2), "%"),
        paste0(round(precision, 2), "%"),
        paste0(round(recall, 2), "%"),
        nrow(test_data)
      )
    )
  })
  reactive_reg_tree <- reactive({
    req(split_data()$training_data)
    rpart(
      Delivery_Time_min ~ Traffic_Level + Time_of_Day + Vehicle_Type + Weather + Distance_km,
      data = split_data()$training_data,
      method = "anova",
      control = rpart.control(minsplit = 5)
    )
  })
  reactive_reg_tree_predictions <- reactive({
    req(reactive_reg_tree())
    req(split_data()$testing_data)

    test_data <- split_data()$testing_data
    predictions <- predict(reactive_reg_tree(), newdata = test_data)

    # Calculate metrics
    rmse <- sqrt(mean((test_data$Delivery_Time_min - predictions)^2))
    mae <- mean(abs(test_data$Delivery_Time_min - predictions))

    list(
      predictions = predictions,
      test_data = test_data,
      rmse = rmse,
      mae = mae
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
  output$reg_predictions <- renderTable({
    req(reactive_reg_tree_predictions())
    data.frame(
      Actual = reactive_reg_tree_predictions()$test_data$Delivery_Time_min,
      Predicted = round(reactive_reg_tree_predictions()$predictions, 2),
      Error = round(abs(reactive_reg_tree_predictions()$test_data$Delivery_Time_min -
        reactive_reg_tree_predictions()$predictions), 2)
    ) %>%
      head(20)
  })

  output$reg_tree_metrics <- renderText({
    req(reactive_reg_tree_predictions())
    paste(
      "RMSE:", round(reactive_reg_tree_predictions()$rmse, 2),
      "| MAE:", round(reactive_reg_tree_predictions()$mae, 2)
    )
  })

  output$box_before <- renderPlot({
    req(raw_df())
    par(mfrow = c(2, 2))
    raw <- raw_df()
    boxplot(as.numeric(raw$Distance_km), main = "Distance_km")
    boxplot(as.numeric(raw$Preparation_Time_min), main = "Prep Time")
    boxplot(as.numeric(raw$Courier_Experience_yrs), main = "Experience")
    boxplot(as.numeric(raw$Delivery_Time_min), main = "Delivery Time")
  })

  output$unusual_data <- renderPlot({
    req(raw_df())
    par(mfrow = c(2, 2))
    raw <- raw_df()
    visualize_data_unusual(as.numeric(raw$Distance_km))
    visualize_data_unusual(as.numeric(raw$Preparation_Time_min))
    visualize_data_unusual(as.numeric(raw$Courier_Experience_yrs))
    visualize_data_unusual(as.numeric(raw$Delivery_Time_min))
  })
  output$time_of_day_pie <- renderPlot({
    req(data()$cleaned_df)
    visualize_data_pie(data()$cleaned_df$Time_of_Day)
  })

  # ======================= Data Visualization =================
  output$tend_plot <- renderPlot({
    req(data()$cleaned_df)
    numeric_cols <- names(data()$cleaned_df)[sapply(data()$cleaned_df, is.numeric)]
    par(mfrow = c(length(numeric_cols), 3), mar = c(3, 3, 3, 1)) # 3 plots per column
    for (col in numeric_cols) {
      visualize_data_tend(data()$cleaned_df[[col]])
    }
  })

  output$cat_plots <- renderUI({
    req(data()$cleaned_df)
    df <- data()$cleaned_df
    selected_cols <- c("Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type")

    list_plots <- lapply(selected_cols, function(col) {
      output[[col]] <- renderPlot({
        visualize_data_table(df[[col]], col)
      })

      plotOutput(col)
    })

    do.call(tagList, list_plots)
  })


  # ================ 3 ==========================

  output$relation_plots <- renderUI({
    req(data()$cleaned_df)
    df <- data()$cleaned_df

    relations_cols <- list(
      c("Delivery_Time_min", "Distance_km"),
      c("Delivery_Time_min", "Speed_kmph")
    )

    plot_ui_list <- lapply(relations_cols, function(pair) {
      xcol <- pair[1]
      ycol <- pair[2]
      plot_id <- paste0("rel_", xcol, "_", ycol)

      output[[plot_id]] <- renderPlot({
        visualize_data_relation(
          df[[xcol]],
          df[[ycol]],
          main = paste(xcol, "vs", ycol),
          xlab = xcol,
          ylab = ycol
        )
      })

      plotOutput(plot_id, height = 300)
    })

    do.call(tagList, plot_ui_list)
  })


  # ================== just test from Ahmed Farag =====================

  result <- reactive({
    req(data())
    df <- data()$cleaned_df %>%
      mutate(late_rate = ifelse(Late_Delivery == "Yes", 1, 0))

    # Calculate performance metrics by vehicle type
    result_df <- df %>%
      group_by(Vehicle_Type) %>%
      summarise(
        orders = n(),
        avg_delivery_time = mean(Delivery_Time_min, na.rm = TRUE),
        median_delivery_time = median(Delivery_Time_min, na.rm = TRUE),
        delivery_time_sd = sd(Delivery_Time_min, na.rm = TRUE),
        avg_speed = mean(Speed_kmph, na.rm = TRUE),
        late_delivery_rate = mean(late_rate) * 100,
        on_time_rate = (1 - mean(late_rate)) * 100,
        avg_distance = mean(Distance_km, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # Speed score (normalized 0-1, higher is better)
        speed_score = avg_speed / max(avg_speed),

        # Reliability score (lower delivery time = better)
        reliability_score = 1 - (median_delivery_time / max(median_delivery_time)),

        # Consistency score (lower variance = better)
        consistency_score = 1 - (delivery_time_sd / max(delivery_time_sd, na.rm = TRUE)),

        # On-time score (higher on-time rate = better)
        on_time_score = on_time_rate / 100,

        # Composite performance score
        performance_score = (speed_score * 0.25 +
          reliability_score * 0.35 +
          consistency_score * 0.2 +
          on_time_score * 0.2) * 100
      ) %>%
      arrange(desc(performance_score)) %>%
      select(
        Vehicle_Type, orders, avg_delivery_time, median_delivery_time,
        on_time_rate, avg_speed, performance_score
      )
  })

  output$vehicle_table <- renderTable({
    req(result())
    result() %>%
      mutate(
        performance_score = round(performance_score, 2),
        on_time_rate = round(on_time_rate, 2),
        avg_speed = round(avg_speed, 2),
        avg_delivery_time = round(avg_delivery_time, 2)
      )
  })
}
