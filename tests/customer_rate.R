library(dplyr)

predict_customer_rate <- function(df) {
  df <- df %>%
    mutate(
      total_time = Delivery_Time_min + Preparation_time
    )

  t_min <- min(df$total_time)
  t_max <- max(df$total_time)
  m <- mean(df$total_time)

  df <- df %>%
    mutate(
      Customer_Rating = case_when(
        total_time <= m ~ 2.5 + (5 - 2.5) * (m - total_time) / (m - t_min),
        total_time > m ~ 2.5 - (2.5 - 1) * (total_time - m) / (t_max - m)
      ),
      Customer_Rating = pmin(pmax(Customer_Rating, 1), 5)
    )

  return(df)
}
