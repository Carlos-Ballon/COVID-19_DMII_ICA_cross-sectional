# Function to create density plots for two groups of each variable in a data frame
two_groups_plots <- function(df1, df2, df3) {
  # Create function components
  plots <- lapply(names(df1), function(var) {
    # calculate the p-value of the Shapiro-Wilk test for two groups of each variable
    surv_p_value <- shapiro_p(df1[[var]])
    non_p_value <- shapiro_p(df2[[var]])
    
    # Summary statistics of survivor group variables
    surv_mean_value <- mean(df1[[var]])
    surv_median_value <- median(df1[[var]])
    surv_q1_value <- quantile(df1[[var]], probs = 0.25)
    surv_q3_value <- quantile(df1[[var]], probs = 0.75)
    
    # Summary statistics of non-survivor group variables
    non_mean_value <- mean(df2[[var]])
    non_median_value <- median(df2[[var]])
    non_q1_value <- quantile(df2[[var]], probs = 0.25)
    non_q3_value <- quantile(df2[[var]], probs = 0.75)
    
    # Density plot for two groups of each variable
    plot <- ggplot(df3, aes_string(
      x = var,
      fill = df3$a_f,
      color = df3$a_f
    )) +
      geom_density(aes(y = after_stat(density)),
                   adjust = 1.5,
                   linewidth = 1) +
      ggtitle(paste("Density Plot of\n", var)) +
      labs(x = element_blank(), color = "Patients") +
      ylab("Density") +
      theme_minimal() +
      scale_fill_igv(palette = "alternating", alpha = 0.3) +
      scale_color_igv(palette = "alternating", alpha = 1) +
      scale_y_continuous(expand = c(0.1, 0)) +
      guides(fill = "none") +
      theme(
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        strip.text = element_text(color = "black")
      )
    
    # Add the p-value and summary statistics for two groups of each variable to the plot
    plot <- plot +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 1,
        label = paste("First Quartile =", round(surv_q1_value, 2)),
        size = 3.3,
        color = "#003399ff"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 2.5,
        label = paste("Mean =", round(surv_mean_value, 2)),
        size = 3.3,
        color = "#003399ff"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 4,
        label = paste("Median =", round(surv_median_value, 2)),
        size = 3.3,
        color = "#003399ff"
      ) +
      annotate(
        "text",
        x = Inf,
        y = 0,
        hjust = 1,
        vjust = 5.5,
        label = paste("Third Quartile =", round(surv_q3_value, 2)),
        size = 3.3,
        color = "#003399ff"
      ) +
      annotate(
        "text",
        x = Inf,
        y = 0,
        hjust = 2.6,
        vjust = 1,
        label = paste("Shapiro-Wilk test =", surv_p_value),
        size = 3.3,
        color = "#003399ff"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 8,
        label = paste("First Quartile =", round(non_q1_value, 2)),
        size = 3.3,
        color = "#CC9900FF"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 9.5,
        label = paste("Mean =", round(non_mean_value, 2)),
        size = 3.3,
        color = "#CC9900FF"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 11,
        label = paste("Median =", round(non_median_value, 2)),
        size = 3.3,
        color = "#CC9900FF"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1,
        vjust = 12.5,
        label = paste("Third Quartile =", round(non_q3_value, 2)),
        size = 3.3,
        color = "#CC9900FF"
      ) +
      annotate(
        "text",
        x = Inf,
        y = 0,
        hjust = 1.20,
        vjust = 1,
        label = paste("Shapiro-Wilk test =", surv_p_value),
        size = 3.3,
        color = "#CC9900FF"
      )
    
    return(plot)
  })
  
  # Arrange multiple plots
  ggpubr::ggarrange(
    plotlist = plots[1:6],
    ncol = 2,
    nrow = 3,
    common.legend = TRUE,
    legend = "bottom"
  )
}
