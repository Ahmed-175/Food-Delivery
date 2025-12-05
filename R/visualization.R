
library(modeest)
library(ggplot2)

# ==========================
# 1️ Central Tendency
# ==========================
visualize_data_tend <- function(data) {
    # Histogram + mean, median, mode
    hist(data, col = "lightblue", border = "white", main = "Central Tendency", xlab = "Values")

    # Mean
    abline(v = mean(data, na.rm = TRUE), col = "red", lwd = 2)
    text(
        x = mean(data, na.rm = TRUE), y = max(hist(data, plot = FALSE)$counts) * 0.9,
        labels = paste("Mean =", round(mean(data, na.rm = TRUE), 2)), col = "red"
    )

    # Median
    abline(v = median(data, na.rm = TRUE), col = "green", lwd = 2)
    text(
        x = median(data, na.rm = TRUE), y = max(hist(data, plot = FALSE)$counts) * 0.8,
        labels = paste("Median =", round(median(data, na.rm = TRUE), 2)), col = "green"
    )

    # Mode
    abline(v = mfv(data, na_rm = TRUE), col = "blue", lwd = 2)
    text(
        x = mfv(data, na_rm = TRUE), y = max(hist(data, plot = FALSE)$counts) * 0.7,
        labels = paste("Mode =", mfv(data, na_rm = TRUE)), col = "blue"
    )
}

# ==========================
# 2️ Categorical Table / Barplot
# ==========================
visualize_data_table <- function(data) {
    freq <- table(data)
    barplot(freq,
        col = "orange", border = "white",
        main = "Categorical Distribution",
        xlab = "Category", ylab = "Frequency"
    )
}

# ==========================
# Numerical Frequency
# ==========================
visualize_data_freq <- function(data, main_title = "Histogram", xlab_title = "Values", ylab_title = "Frequency") {
    hist(data,
        col = "lightgreen", border = "blue",
        main = main_title, xlab = xlab_title, ylab = ylab_title
    )
}

# ==========================
#  Relation Plot (Scatter)
# ==========================
visualize_data_relation <- function(data1, data2, main_title = "Scatter Plot", xlab_title = "X", ylab_title = "Y") {
    plot(
        x = data1, y = data2, col = "darkblue", pch = 19,
        main = main_title, xlab = xlab_title, ylab = ylab_title
    )
    abline(lm(data2 ~ data1), col = "red", lwd = 2) # optional regression line
}
