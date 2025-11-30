clean_data <- function(df) {
  library(dplyr)

  df <- df[!duplicated(df), ]
  df[df == ""] <- NA

  df$Order_ID <- as.integer(df$Order_ID)
  df$Weather <- as.character(df$Weather)
  df$Traffic_Level <- as.character(df$Traffic_Level)
  df$Time_of_Day <- as.character(df$Time_of_Day)
  df$Vehicle_Type <- as.character(df$Vehicle_Type)

  df$Distance_km <- as.numeric(df$Distance_km)
  df$Preparation_Time_min <- as.numeric(df$Preparation_Time_min)
  df$Courier_Experience_yrs <- as.numeric(df$Courier_Experience_yrs)
  df$Delivery_Time_min <- as.numeric(df$Delivery_Time_min)

  df <- na.omit(df)

  out_dist <- boxplot(df$Distance_km)$out
  out_prep <- boxplot(df$Preparation_Time_min)$out
  out_cour <- boxplot(df$Courier_Experience_yrs)$out
  out_del <- boxplot(df$Delivery_Time_min)$out

  remove_rows <- unique(c(
    which(df$Distance_km %in% out_dist),
    which(df$Preparation_Time_min %in% out_prep),
    which(df$Courier_Experience_yrs %in% out_cour),
    which(df$Delivery_Time_min %in% out_del)
  ))

  df <- df[-remove_rows, ]

  df$Speed_kmph <- round((df$Distance_km / df$Delivery_Time_min) * 60, 2)
  df$Late_Delivery <- ifelse(df$Delivery_Time_min > 30, "Yes", "No")

  print(paste("Rows after cleaning:", nrow(df)))

  return(df)
}
