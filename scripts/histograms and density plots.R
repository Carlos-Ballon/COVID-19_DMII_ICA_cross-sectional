# Function to calculate the p-value of the Shapiro-Wilk test
shapiro_p <- function(x) {
  test <- stats::shapiro.test(x)
  p_value <- round(test$p.value, 3)
  return(p_value)
}

# Function to create histograms and density plots for each variable in a data frame
total_plots <- function(.data) {
  # Create function components
  plots <- lapply(names(.data), function(var) {
    # Calculate the p-value of the Shapiro-Wilk test for each variable
    p_value <- shapiro_p(.data[[var]])
    
    # Summary statistics for each variable
    mean_value <- mean(.data[[var]], na.rm = TRUE)
    median_value <- median(.data[[var]], na.rm = TRUE)
    q1_value <- quantile(.data[[var]], probs = 0.25, na.rm = TRUE)
    q3_value <- quantile(.data[[var]], probs = 0.75, na.rm = TRUE)
    
    # Histogram and density plot for each variable
    plot <- ggplot2::ggplot(data = .data, aes_string(x = var)) +
      ggplot2::geom_histogram(
        aes(y = after_stat(density)),
        bins = 10,
        alpha = 0.8,
        fill = "#3B3B3BFF",
        color = "white",
        na.rm = TRUE
      ) +
      ggplot2::geom_density(
        fill = "#0073C2FF",
        col = "#00B5E2FF",
        alpha = 0.3,
        linewidth = 1,
        na.rm = TRUE
      ) +
      ggplot2::geom_vline(
        aes(xintercept = mean(.data[[var]], na.rm = TRUE)),
        color = "#D51317FF",
        linetype = "dashed",
        linewidth = 1
      ) +
      ggplot2::geom_vline(
        aes(xintercept = median(.data[[var]], na.rm = TRUE)),
        color = "#95C11FFF",
        linetype = "dashed",
        linewidth = 1
      ) +
      ggplot2::ggtitle(paste("Histogram and Density Plot of", var)) +
      ggplot2::labs(x = element_blank()) +
      ggplot2::ylab("Density") +
      ggplot2::scale_y_continuous(expand = c(0.1, 0)) +
      theme_538()
    
    # Add the p-value and summary statistics for each variable to the plot
    plot <- plot +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = 0,
        hjust = 1,
        vjust = 1.2,
        label = paste("Shapiro-Wilk test p-value =", p_value),
        size = 3.3
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 1,
        label = paste("First Quartile =", round(q1_value, 2)),
        size = 3.3
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 2.5,
        label = paste("Mean =", round(mean_value, 2)),
        size = 3.3,
        color = "#D51317FF"
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 4,
        label = paste("Median =", round(median_value, 2)),
        size = 3.3,
        color = "#95C11F"
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 5.5,
        label = paste("Third Quartile =", round(q3_value, 2)),
        size = 3.3
      )
    return(plot)
  })
  
  # Arrange multiple plots
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
}
