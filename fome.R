library(dplyr)
library(factoextra)
library(ggplot2)

source("R/cleaning.R")


perform_kmeans <- function(df, k = 3, seed = 123) {
    cluster_features <- df %>%
        select(Distance_km, Preparation_Time_min, Courier_Experience_yrs, Speed_kmph)

    cluster_scaled <- scale(cluster_features)

    set.seed(seed)


    km <- kmeans(cluster_scaled, centers = k, nstart = 25)

    df$cluster <- factor(km$cluster)

    return(list(data = df, km = km, scaled = cluster_scaled))
}


data <- read.csv("data/Food_Delivery_Times.csv")
df <- clean_data(data)


result <- perform_kmeans(df, k = 5)

print(head(result$data, 10))

cluster_summary <- result$data %>%
    group_by(cluster) %>%
    summarise(
        avg_distance = mean(Distance_km),
        avg_prep_time = mean(Preparation_Time_min),
        avg_experience = mean(Courier_Experience_yrs),
        avg_speed = mean(Speed_kmph),
        ava_time_delivary = mean(Delivery_Time_min),
        n_orders = n()
    )

print(cluster_summary)

fviz_cluster(result$km,
    data = result$scaled,
    geom = "point",
    ellipse.type = "norm",
    palette = "jco"
)

ggplot(result$data, aes(x = Distance_km, y = Preparation_Time_min, color = cluster)) +
    geom_point(size = 3) +
    labs(
        title = "Clusters of Food Delivery Orders",
        x = "Distance (km)",
        y = "Preparation Time (min)"
    ) +
    theme_minimal()


fviz_nbclust(result$scaled, kmeans, method = "wss") +
    labs(subtitle = "Elbow Method")


fviz_nbclust(result$scaled, kmeans, method = "silhouette") +
    labs(subtitle = "Silhouette Method")
