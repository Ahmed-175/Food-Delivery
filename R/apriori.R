perform_apriori <- function(df, support = 0.01, confidence = 0.4, clusters) {
    library(arules)

    # Build item-like columns (factor + labels)
    df_items <- df %>%
        mutate(
            Weather = paste0("Weather=", Weather),
            Traffic_Level = paste0("Traffic=", Traffic_Level),
            Time_of_Day = paste0("Time=", Time_of_Day),
            Vehicle_Type = paste0("Vehicle=", Vehicle_Type),
            Cluster = paste0("Cluster=", clusters)
        ) %>%
        select(Weather, Traffic_Level, Time_of_Day, Vehicle_Type, Cluster)

    # Convert to transactions for apriori
    trans <- as(df_items, "transactions")

    # Run apriori
    rules <- apriori(
        trans,
        parameter = list(supp = support, conf = confidence)
    )

    return(list(rules = rules))
}
