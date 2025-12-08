library(shiny)
library(modeest)


visualize_data_tend_mean <- function(data, col_name) {
  mean_val <- mean(data)
  hist(
    data,
    main = paste("Histogram of", col_name),
    col = "lightblue", border = "black",
    xlab = col_name
  )

  # Mean
  abline(v = mean_val, col = "red", lwd = 4) # font my frind lwd
  text(
    x = mean_val,
    y = max(hist(data, plot = FALSE)$counts) * 0.9,
    labels = paste("Mean =", round(mean_val, 2)), col = "red", pos = 4
  )
}

visualize_data_tend_median <- function(data, col_name) {
  median_val <- median(data, na.rm = TRUE)
  hist(
    data,
    main = paste("Histogram of", col_name),
    col = "lightblue", border = "black",
    xlab = col_name
  )
  abline(v = median_val, col = "black", lwd = 4)
  text(
    x = median_val,
    y = max(hist(data, plot = FALSE)$counts) * 0.8,
    labels = paste("Median =", round(median_val, 2)), col = "black", pos = 4
  )
}

visualize_data_tend_mode <- function(data, col_name) {
  mode_val <- mfv(data)
  hist(
    data,
    main = paste("Histogram of", col_name),
    col = "lightblue", border = "black",
    xlab = col_name
  )

  abline(v = mode_val, col = "purple", lwd = 4)
  text(
    x = mode_val,
    y = max(hist(data, plot = FALSE)$counts) * 0.7,
    labels = paste("Mode =", mode_val), col = "purple", pos = 4
  )
}

# Table/Barplot for categorical data
visualize_data_table <- function(data, col) {
  counts <- table(data)
  barplot(counts,
    col = "orange", main = paste("Barplot of", col),
    xlab = col, ylab = "Frequency"
  )
}


# histogram for frequency and distribution of numeric data (4)
visualize_data_freq <- function(data, main = "", xlab = "", ylab = "Frequency") {
  hist(data, col = "purple", border = "blue", main = main, xlab = xlab, ylab = ylab)
}

# Scatter plot for relation between two numeric columns
visualize_data_relation <- function(data1, data2, main = "", xlab = "", ylab = "") {
  plot(
    x = data1, y = data2, main = main, xlab = xlab, ylab = ylab,
    col = "black", pch = 19
  )
}
# box plot for unusual numeric data (5)
visualize_data_unusual <- function(data) {
  outlier <- boxplot(data,
    main = "Box Plot - Outlier Detection",
    ylab = "Value", col = "lightblue"
  )

  # Print outlier values
  if (length(outlier$out) > 0) {
    cat("Outliers detected:", outlier$out, "\n")
  } else {
    cat("No outliers detected\n")
  }

  return(outlier)
}
# table/pie delivery time distribution (8)
visualize_data_pie <- function(data) {
  counts <- table(data)
  percentage <- paste0(round(100 * counts / sum(counts)), "%")

  # Define colors and labels based on data
  colors <- c("pink", "lightblue", "red", "black")
  labels_legend <- c("Afternoon", "Evening", "Morning", "Night")

  # Use only the colors/labels that match the data
  n_categories <- length(counts)
  colors_used <- colors[1:n_categories]
  labels_used <- labels_legend[1:n_categories]

  pie(counts,
    labels = percentage, main = "Delivery Time Distribution",
    col = colors_used
  )
  legend("bottomright",
    legend = labels_used,
    fill = colors_used
  )
}
