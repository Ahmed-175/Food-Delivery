

per_weather <- function(df) {
    
 res <- df %>% 
    group_by(Vehicle_Type)  %>%
    summarise(
        Vehicle_Type = Vehicle_Type,
        Speed_kmph = Speed_kmph,
        Weather = Weather
    )
    

    return(res)
}