# Summary statistics and normality tests
group_stat_table_plot <- function(data_used, var, outcome) {
  # Check if "var" and "outcome" are strings or variable names
  if (is.character(var)) {
    var <- as.name(var)
  }
  if (is.character(outcome)) {
    outcome <- as.name(outcome)
  }
  
  # Levene’s test
  levene_result <- car::leveneTest(formula(paste(var, "~", outcome)), data = data_used)
  
  # Extract p-value
  levene_p_value <- levene_result$`Pr(>F)`[1]
  
  # Bartlett’s test
  barlett_result <- stats::bartlett.test(formula(paste(var, "~", outcome)), data = data_used)
  
  # Extract p-value
  barlett_p_value <- barlett_result$p.value
  
  stat_table <- data_used |>
    dplyr::group_by({{ outcome }}) |>
    dplyr::summarise(
      Mean = round(mean({{ var }}, na.rm = TRUE), 2),
      SD = round(sd({{ var }}, na.rm = TRUE), 2),
      Median = round(median({{ var }}, na.rm = TRUE), 2),
      IQR = paste(round(quantile({{ var }}, probs = c(0.25, 0.75), na.rm = TRUE), 2), collapse = "-"),
      Shapiro_Wilk = round(stats::shapiro.test({{ var }})$p.value, 3),
      Anderson_Darling = round(nortest::ad.test({{ var }})$p.value, 3),
      Kurtosis = round(moments::kurtosis({{ var }}, na.rm = TRUE), 2),
      Skewness = round(moments::skewness({{ var }}, na.rm = TRUE), 2)
    ) |>
    dplyr::rename(
      "Shapiro-Wilk" = Shapiro_Wilk,
      "Anderson-Darling" = Anderson_Darling,
      "IQR*" = IQR,
      "Group" = {{ outcome }}
    ) |>
    
    # Draw a textual table
    ggpubr::ggtexttable(
      rows = NULL,
      theme = ttheme(
        base_style = "blank",
        colnames.style = colnames_style(
          color = "black",
          fill = "#f0f0f0",
          linecolor = "#d0d0d0"
        ),
        tbody.style = tbody_style(
          color = "black",
          fill = "#f0f0f0",
          linecolor = "#d0d0d0"
        )
      )
    )
  
  foot_note <- paste0(
    "*IQR = Rango intercuartílico; ",
    "Levene's test p-value = ",
    round(levene_p_value, 3),
    "; ",
    "Bartlett’s test p-value = ",
    round(barlett_p_value, 3),
    ".\n",
    "Normal distribution if: p-values of hypothesis tests are >0.05, ",
    "skewness between -0.5 to 0.5, and kurtosis between 2.5 to 3.5"
  ) |> 
    paste(collapse = "\n")
  
  # Customization
  custom_stat_table <- stat_table |>
    ggpubr::tab_add_hline(at.row = 1:2,
                          row.side = "top",
                          linewidth = 2) |>
    ggpubr::tab_add_hline(
      at.row = 2:tab_nrow(stat_table),
      row.side = "bottom",
      linetype = 1
    ) |>
    ggpubr::table_cell_bg(
      row = 2:tab_nrow(stat_table),
      column = 2,
      fill = "#CE3D32FF",
      alpha = 0.4,
      color = "#d0d0d0"
    ) |>
    ggpubr::table_cell_bg(
      row = 2:tab_nrow(stat_table),
      column = 4,
      fill = "#99CC00FF",
      alpha = 0.4,
      color = "#d0d0d0"
    ) |>
    ggpubr::table_cell_bg(
      row = 2,
      column = 1,
      fill = "#5773CCFF",
      alpha = 0.4,
      color = "#d0d0d0"
    ) |>
    ggpubr::table_cell_bg(
      row = 3,
      column = 1,
      fill = "#FFB900FF",
      alpha = 0.4,
      color = "#d0d0d0"
    ) |>
    ggpubr::tab_add_title(
      text = "Summary statistics and normality tests",
      hjust = -0.88,
      padding = unit(1.5, "line"),
      size = 11,
      face = "italic"
    ) |>
    ggpubr::tab_add_footnote(
      text = foot_note,
      padding = unit(0.5, "line"),
      size = 10,
      face = "italic",
      hjust = 0.97
    )
  
  # Density plot by groups
  plot <- ggplot2::ggplot(data = data_used,
                          aes_string(x = var, fill = outcome, color = outcome)) +
    ggplot2::geom_density(
      aes(y = after_stat(density)),
      adjust = 1.5,
      linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = element_blank(),
      y = "Density",
      color = "Group",
      title = paste("Density Plot of", var)
    ) +
    ggsci::scale_fill_igv(palette = "alternating", alpha = 0.4) +
    ggsci::scale_color_igv(palette = "alternating", alpha = 0.7) +
    ggplot2::guides(fill = "none") +
    theme_538() +
    theme(
      plot.title = element_text(size = rel(2.8), family = "Chivo"),
      axis.title = element_text(size = rel(2.3), family = "Chivo"),
      legend.position = "none"
    )
  
  ggpubr::ggarrange(plot,
                    custom_stat_table,
                    ncol = 1,
                    heights = c(3, 1))
}

# Use "var" and "outcome" to perform calculations and create the table
# group_stat_table_plot(data, "edad", "a_f")





