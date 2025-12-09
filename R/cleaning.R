clean_data <- function(df) {
  library(dplyr)

  df <- df[!duplicated(df), ]
  df[df == ""] <- NA

  df <- df %>%
    mutate(
      Order_ID = as.integer(Order_ID),
      Weather = as.factor(Weather),
      Traffic_Level = as.factor(Traffic_Level),
      Time_of_Day = as.factor(Time_of_Day),
      Vehicle_Type = as.factor(Vehicle_Type),
      Distance_km = as.numeric(Distance_km),
      Preparation_Time_min = as.numeric(Preparation_Time_min),
      Courier_Experience_yrs = as.numeric(Courier_Experience_yrs),
      Delivery_Time_min = as.numeric(Delivery_Time_min)
    ) %>%
    na.omit()

  out_dist <- boxplot(df$Distance_km, plot = FALSE)$out
  out_prep <- boxplot(df$Preparation_Time_min, plot = FALSE)$out
  out_cour <- boxplot(df$Courier_Experience_yrs, plot = FALSE)$out
  out_del <- boxplot(df$Delivery_Time_min, plot = FALSE)$out

  outliers <- list(
    Distance_km = out_dist,
    Preparation_Time_min = out_prep,
    Courier_Experience_yrs = out_cour,
    Delivery_Time_min = out_del
  )

  remove_rows <- unique(c(
    which(df$Distance_km %in% out_dist),
    which(df$Preparation_Time_min %in% out_prep),
    which(df$Courier_Experience_yrs %in% out_cour),
    which(df$Delivery_Time_min %in% out_del)
  ))

  df <- df[-remove_rows, ]

  df <- df %>%
    mutate(
      Speed_kmph = round((Distance_km / Delivery_Time_min) * 60, 2),
      total_time = Delivery_Time_min + Preparation_Time_min
    )

  t_min <- min(df$total_time)
  t_max <- max(df$total_time)
  m <- mean(df$total_time)

  df <- df %>%
    mutate(
      Customer_Rating = case_when(
        total_time <= m ~ round(2.5 + (5 - 2.5) * (m - total_time) / (m - t_min), 1),
        total_time > m ~ round(2.5 - (2.5 - 1) * (total_time - m) / (t_max - m), 1)
      ),
      Customer_Rating = pmin(pmax(Customer_Rating, 1), 5)
    )
  print(paste("Rows after cleaning:", nrow(df)))

  df$Late_Delivery <- ifelse(df$Delivery_Time_min >= quantile(df$Delivery_Time_min , 0.6), "Yes", "No")
  df$traffic_score <- ifelse(df$Traffic_Level == "High", 1,
    ifelse(df$Traffic_Level == "Medium", 0.5, 0)
  )

  return(list(
    cleaned_df = df,
    outliers = outliers
  ))
}
splitted_data<-function(df){
training_data <- df %>% slice_sample(prop = 2/3)
testing_data<- df %>% anti_join(training_data, by = names(df))
return(list(training_data=training_data,testing_data=testing_data))
}
