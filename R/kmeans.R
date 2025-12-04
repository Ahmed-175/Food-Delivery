perform_kmeans <- function(df, k = 3) {
    cluster_features <- df %>%
        select(Distance_km, Preparation_Time_min, Courier_Experience_yrs, Speed_kmph)

    cluster_scaled <- scale(cluster_features)
    km <- kmeans(cluster_scaled, centers = k, nstart = 25)

    print(km)
    df$cluster <- factor(km$cluster)
    return(list(
        data = df,
        km = km,
        scaled = cluster_scaled
    ))
}
