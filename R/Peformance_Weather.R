per_weather <- function(df) {
    
 res <- df %>% 
    group_by(Vehicle_Type, Weather) %>%
    summarise(
      avg_speed = round(mean(Speed_kmph, na.rm = TRUE), 2),
      avg_delivery_time = round(mean(Delivery_Time_min, na.rm = TRUE), 2),
      count = n(),  # Add count to verify grouping
      .groups = 'drop'
    ) %>%
    arrange(Vehicle_Type, Weather)
    

    return(res)
}