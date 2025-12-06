library(shiny)
library(modeest)

visualize_data_tend <- function(vec, col_name) {
  # Mean plot
  hist(vec,
    main = paste("Histogram of", col_name, "(Mean)"),
    col = "lightblue", border = "black"
  )
  abline(v = mean(vec), col = "red", lwd = 2)
  mtext(paste("The mean represents the average value of", col_name),
    side = 3, line = 0.5, col = "blue"
  )

  # Median plot
  hist(vec,
    main = paste("Histogram of", col_name, "(Median)"),
    col = "lightgreen", border = "black"
  )
  abline(v = median(vec), col = "red", lwd = 2)
  mtext(paste("The median is the middle value of", col_name),
    side = 3, line = 0.5, col = "darkgreen"
  )

  # Mode plot
  mode_val <- mfv(vec)
  hist(vec,
    main = paste("Histogram of", col_name, "(Mode)"),
    col = "lightpink", border = "black"
  )
  abline(v = mode_val, col = "red", lwd = 2)
  mtext(paste("The mode is the most frequent value of", col_name),
    side = 3, line = 0.5, col = "purple"
  )
}






# Table/Barplot for categorical data
visualize_data_table <- function(data) {
  counts <- table(data)
  barplot(counts,
    col = "orange", main = "Barplot of Categorical Data",
    xlab = "Category", ylab = "Frequency"
  )
}


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
