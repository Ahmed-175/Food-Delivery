perform_kmeans <- function(df, k = 3) {
    cluster_features <- df %>%
        select(Distance_km, Preparation_Time_min, Courier_Experience_yrs, Speed_kmph)

    cluster_scaled <- scale(cluster_features)
    km <- kmeans(cluster_scaled, centers = k, nstart = 25)

    print(km)
    df$cluster <- factor(km$cluster)
    overall_avg <- mean(df$Delivery_Time_min)

    cluster_summary <- df %>%

      group_by(cluster) %>%

      summarise(avg_delivery = mean(Delivery_Time_min))

    cluster_summary <- cluster_summary %>%

      mutate(flag = ifelse(avg_delivery > overall_avg, "Late", "Fast"))

    df <- df %>%

      left_join(cluster_summary %>% select(cluster, flag), by = "cluster")
    return(list(
        data = df,
        km = km,
        scaled = cluster_scaled
    ))
}
