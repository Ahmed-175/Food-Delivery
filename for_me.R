  output$tend_plot <- renderPlot({
    req(data()$cleaned_df)

    numeric_cols <- names(data()$cleaned_df)[sapply(data()$cleaned_df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, "Order_ID")

    par(mfrow = c(length(numeric_cols), 1), mar = c(1, 1, 1, 20))
    # for (col in numeric_cols) {
    #     visualize_data_tend(data()$cleaned_df[[col]], col)
    # }
    visualize_data_tend(data()$cleaned_df[["Delivery_Time_min"]], "Delivery_Time_min")
  })