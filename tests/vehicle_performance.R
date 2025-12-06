library("dplyr")

vehicle_performance <- function(df, vehicle_type) {
  df %>%
    filter(Vehicle_Type == vehicle_type) %>%
    summarise(
      avg_delivery_time = mean(Delivery_Time, na.rm = TRUE),
      median_delivery_time = median(Delivery_Time, na.rm = TRUE),
      .groups = 'drop'
    )
}