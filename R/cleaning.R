clean_data <- function(df) {
  library(dplyr)

  df <- df[!duplicated(df), ]
  df[df == ""] <- NA
  df <- na.omit(df)


  df$Order_ID <- as.integer(df$Order_ID)
  df$Weather <- as.character(df$Weather)
  df$Traffic_Level <- as.character(df$Traffic_Level)
  df$Time_of_Day <- as.character(df$Time_of_Day)
  df$Vehicle_Type <- as.character(df$Vehicle_Type)
  df$Distance_km <- as.numeric(df$Distance_km)
  df$Preparation_Time_min <- as.integer(df$Preparation_Time_min)
  df$Courier_Experience_yrs <- as.numeric(df$Courier_Experience_yrs)
  df$Delivery_Time_min <- as.integer(df$Delivery_Time_min)


  out_dist <- boxplot(df$Distance_km, plot = FALSE)$out
  out_prep <- boxplot(df$Preparation_Time_min, plot = FALSE)$out
  out_cour <- boxplot(df$Courier_Experience_yrs, plot = FALSE)$out
  out_del <- boxplot(df$Delivery_Time_min, plot = FALSE)$out


  df <- df[-which(df$Distance_km %in% out_dist), ]
  df <- df[-which(df$Preparation_Time_min %in% out_prep), ]
  df <- df[-which(df$Courier_Experience_yrs %in% out_cour), ]
  df <- df[-which(df$Delivery_Time_min %in% out_del), ]


  df$Speed <- round(df$Distance_km / df$Delivery_Time_min, 2)
  df$Late_Delivery <- ifelse(df$Delivery_Time_min > 30, 1, 0)

  return(df)
}
