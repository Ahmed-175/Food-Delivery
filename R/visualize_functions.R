library(shiny)
library(modeest)
visualize_data_tend <- function(data) {
    par(mfrow = c(3, 1)) # 3 plots vertically
    # Mean
    hist(data, main = "Histogram with Mean", col = "lightblue", border = "black")
    abline(v = mean(data), col = "red", lwd = 2)
    text(
        x = mean(data) * 1.1, y = max(hist(data, plot = FALSE)$counts) * 0.9,
        labels = paste("Mean =", round(mean(data), 2)), col = "red"
    )

    # Median
    hist(data, main = "Histogram with Median", col = "lightgreen", border = "black")
    abline(v = median(data), col = "red", lwd = 2)
    text(
        x = median(data) * 1.1, y = max(hist(data, plot = FALSE)$counts) * 0.9,
        labels = paste("Median =", round(median(data), 2)), col = "red"
    )

    # Mode
    hist(data, main = "Histogram with Mode", col = "lightpink", border = "black")
    mode_val <- mfv(data)
    abline(v = mode_val, col = "red", lwd = 2)
    text(
        x = mode_val * 1.1, y = max(hist(data, plot = FALSE)$counts) * 0.9,
        labels = paste("Mode =", mode_val), col = "red"
    )
}


# Table/Barplot for categorical data
visualize_data_table <- function(data) {
  counts <- table(data)
  barplot(counts, col="orange", main="Barplot of Categorical Data",
          xlab="Category", ylab="Frequency")
}



visualize_data_freq <- function(data, main="", xlab="", ylab="Frequency") {
  hist(data, col="purple", border="blue", main=main, xlab=xlab, ylab=ylab)
}


# Scatter plot for relation between two numeric columns
visualize_data_relation <- function(data1, data2, main="", xlab="", ylab="") {
  plot(x=data1, y=data2, main=main, xlab=xlab, ylab=ylab,
       col="black", pch=19)
}

