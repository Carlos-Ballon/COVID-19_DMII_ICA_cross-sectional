# Define custom `gtsummary` theme
my_gtsummary_theme <-
  list(
    "pkgwide-fn:pvalue_fun" = function(x)
      style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x)
      style_pvalue(x, digits = 2, prepend_p = TRUE),
    "tbl_summary-str:continuous_stat" = "{median} ({p25}, {p75})",
    "tbl_summary-str:categorical_stat" = "{n} ({p}%)",
    "tbl_summary-fn:percent_fun" = function(x)
      style_number(x, digits = 1, scale = 100),
    "tbl_summary-arg:missing" = "no"
  )

# Define custom `flextable` theme
my_flextable_theme <- function(x, bold_header = FALSE) {
  std_border <- fp_border(width = 1, color = "grey14")
  
  x <- border_remove(x)
  x <- hline_top(x, border = std_border, part = "header")
  x <- hline_bottom(x, border = std_border, part = "header")
  x <- bold(x, bold = bold_header, part = "header")
  x <- hline_bottom(x, border = std_border, part = "body")
  x <- align_text_col(x, align = "left", header = TRUE)
  x <- font(x, part = "all", fontname = "Segoe UI")
  fix_border_issues(x, part = "all")
  autofit(x)
}

# Set custom `ggplot2` theme 537
theme_537 <- function(..., base_size = 18) {
  ggplot2::theme(
    
    # drop minor grid lines
    panel.grid.minor = element_blank(),
    
    # change grid lines to gray
    panel.grid.major =  element_line(color = "#d0d0d0"),
    
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    # change legend background to gray
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
    
    # remove strip background
    strip.background = element_blank(),
    
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    axis.ticks = element_blank(),
    
    # change the color of axis lines
    axis.line = element_line(color = "#272822"),
    
    # change text family, size, and adjust position of titles
    text = element_text(family = "Syne", size = base_size),
    axis.text = element_text(color = "#3D3E38", size = base_size),
    axis.title = element_text(color = "#272822", size = rel(1.33)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
    plot.title = element_text(color = "#272822", family = "Syne", size = rel(1.80), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    strip.text = element_text(color = "#272822", size = rel(1.30),face = "bold"),
    ...
  )
}

# Set custom theme `ggplot2` 538
theme_538 <- function(..., base_size = 10) {
  ggplot2::theme(
    
    # drop minor grid lines
    panel.grid.minor = element_line(color = "#d0d0d0"),
    
    # change grid lines to gray
    panel.grid.major =  element_line(color = "#d0d0d0"),
    
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    # change legend background to gray
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
    
    # remove strip background
    strip.background = element_blank(),
    
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    axis.ticks = element_blank(),
    
    # change the color of axis lines
    axis.line = element_blank(),
    
    # change text family, size, and adjust position of titles
    text = element_text(family = "Syne", size = base_size),
    axis.text = element_text(color = "#3D3E38", size = base_size),
    axis.title = element_text(color = "#272822", size = rel(1.33)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
    plot.title = element_text(color = "#272822", family = "Syne", size = rel(1.85), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    strip.text = element_text(color = "#272822", size = rel(1.30), face = "bold"),
    ...
  )
}

# Set custom `ggplot2` theme 539
theme_539 <- function(..., base_size = 12) {
  ggplot2::theme(
    
    # drop minor grid lines
    panel.grid.minor = element_line(color = "#d0d0d0"),
    
    # change grid lines to gray
    panel.grid.major =  element_line(color = "#d0d0d0"),
    
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    # change legend background to gray
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
    
    # remove strip background
    strip.background = element_blank(),
    
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    axis.ticks = element_blank(),
    
    # change the color of axis lines
    axis.line = element_blank(),
    
    # change text family, size, and adjust position of titles
    text = element_text(family = "Syne", size = base_size),
    axis.text = element_text(color = "#3D3E38", size = base_size),
    axis.title = element_text(color = "#272822", size = rel(1.33)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
    plot.title = element_text(color = "#272822", family = "Syne", size = rel(1.80), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    strip.text = element_text(color = "#272822", size = rel(1.30), face = "bold"),
    ...
  )
}

# Set custom theme `ggplot2` 540
theme_540 <- function(..., base_size = 9) {
  ggplot2::theme(
    
    # drop minor grid lines
    panel.grid.minor = element_line(color = "#d0d0d0"),
    
    # change grid lines to gray
    panel.grid.major =  element_line(color = "#d0d0d0"),
    
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    # change legend background to gray
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
    
    # remove strip background
    strip.background = element_blank(),
    
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    axis.ticks = element_blank(),
    
    # change the color of axis lines
    axis.line = element_blank(),
    
    # change text family, size, and adjust position of titles
    text = element_text(family = "Syne", size = base_size),
    axis.text = element_text(color = "#3D3E38", size = base_size),
    axis.title = element_text(color = "#272822", size = rel(1.33)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
    plot.title = element_text(color = "#272822", family = "Syne", size = rel(1.85), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    strip.text = element_text(color = "#272822", size = rel(1.30), face = "bold"),
    ...
  )
}
