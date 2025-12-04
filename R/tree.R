dt <- function(df) {
    df$Late_Delivery <- as.factor(df$Late_Delivery)
    tree_class <- rpart(
        Late_Delivery ~ Distance_km + Traffic_Level +
            Preparation_Time_min + Courier_Experience_yrs,
        data = df,
        method = "class"
    )

    return(tree_class)
}
