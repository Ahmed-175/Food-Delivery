# R/cleaning.R

clean_data <- function(df) {
  library(dplyr)
  
  df <- df %>%
    # Remove rows with NA in important numeric columns
    filter(!is.na(Distance_km), !is.na(Delivery_Time_min),
           !is.na(Preparation_Time_min), !is.na(Courier_Experience_yrs)) %>%
    
    # Remove negative or extreme outliers
    filter(Distance_km >= 0 & Distance_km <= 50,
           Delivery_Time_min >= 5 & Delivery_Time_min <= 180,
           Preparation_Time_min >= 0 & Preparation_Time_min <= 60,
           Courier_Experience_yrs >= 0 & Courier_Experience_yrs <= 40) %>%
    
    # Correct data types
    mutate(
      Weather = factor(Weather),
      Traffic_Level = factor(Traffic_Level),
      Time_of_Day = factor(Time_of_Day),
      Vehicle_Type = factor(Vehicle_Type),

      # Feature engineering
      Speed_kmph = Distance_km / (Delivery_Time_min / 60),
      Late_Delivery = ifelse(Delivery_Time_min > 30, 1, 0)
    )

  return(df)
}
