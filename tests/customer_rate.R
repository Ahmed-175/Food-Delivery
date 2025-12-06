library(dplyr)

predict_customer_rate <- function(df) {
  # Build a linear regression model
  model <- lm(Customer_Rating ~ Delivery_Time_min, data = df)
  
  # Add predictions to dataframe
  df$predicted_rating <- predict(model, newdata = df)
  
  # Clamp predictions between 1-5 (rating scale)
  df$predicted_rating <- pmax(1, pmin(5, df$predicted_rating))
  
  # Calculate model performance
  rmse <- sqrt(mean((df$Customer_Rating - df$predicted_rating)^2, na.rm = TRUE))
  r_squared <- summary(model)$r.squared
  
  print(paste("RMSE:", round(rmse, 2)))
  print(paste("R-squared:", round(r_squared, 4)))
  
  return(list(
    data = df,
    model = model,
    rmse = rmse,
    r_squared = r_squared
  ))
}

# Example usage:
# result <- predict_customer_rate(cleaned_data)
# result$data %>% select(Delivery_Time_min, Customer_Rating, predicted_rating)