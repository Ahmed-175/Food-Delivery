dt <- function(df,test_data) {
    df$Late_Delivery <- as.factor(df$Late_Delivery)
    tree_class <- rpart(
        Late_Delivery ~ Distance_km + Traffic_Level +
            Preparation_Time_min + Courier_Experience_yrs,
        data = df,
        method = "class"
    )
    validating<- predict(tree_class, test_data, type = "class")
    return(list(model=tree_class,predictions=validating))
}
