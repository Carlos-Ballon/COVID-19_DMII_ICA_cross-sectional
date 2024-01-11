# Function to calculate the p-value of the Shapiro-Wilk test
shapiro_p <- function(x) {
  test <- stats::shapiro.test(x)
  p_value <- round(test$p.value, 3)
  return(p_value)
}

# Function to create histograms and density plots for each variable in a data frame
total_plots <- function(df) {
  
  # Create function components
  plots <- lapply(names(df), function(var) {
    # calculate the p-value of the Shapiro-Wilk test for each variable
    p_value <- shapiro_p(df[[var]])
    
    # Summary statistics for each variable
    mean_value <- mean(df[[var]])
    median_value <- median(df[[var]])
    q1_value <- quantile(df[[var]], probs = 0.25) 
    q3_value <- quantile(df[[var]], probs = 0.75)
    
    # Histogram and density plot for each variable
    plot <- ggplot(data = df, aes_string(x = var)) +
      geom_histogram(aes(y = after_stat(density)), bins = 10, 
                     fill = "#3B3B3BFF", color = "white", alpha = 0.8) +
      geom_density(fill = "#0073C2FF", col = "#00B5E2FF", alpha = 0.3, linewidth = 1) +
      geom_vline(aes(xintercept = mean(df[[var]])), 
                 color = "#D51317FF", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = median(df[[var]])), 
                 color = "#95C11FFF", linetype = "dashed", linewidth = 1) +
      ggtitle(paste("Histogram and Density Plot of\n", var)) +
      labs(x = element_blank()) +
      ylab("Density") +
      theme_minimal() +
      scale_y_continuous(expand = c(0.1, 0)) +
      theme(axis.line = element_line(color = "black"),
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black"), 
            strip.text = element_text(color = "black"))
    
    # Add the p-value and summary statistics for each variable to the plot
    plot <- plot + 
      annotate("text", x = Inf, y = 0, hjust = 1.20, vjust = 1,
               label = paste("Shapiro-Wilk test p-value =", p_value), 
               size = 3.3) +
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
               label = paste("First Quartile =", round(q1_value, 2)),
               size = 3.3) +
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2.5,
               label = paste("Mean =", round(mean_value, 2)),
               size = 3.3, color = "#D51317FF") +
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 4,
               label = paste("Median =", round(median_value, 2)),
               size = 3.3, color = "#95C11F") + 
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 5.5,
               label = paste("Third Quartile =", round(q3_value, 2)),
               size = 3.3)
    return(plot)
  })
  
  # Arrange multiple plots
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
}
