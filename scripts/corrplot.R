my_ggcorrplor <- function(df) {
  ggcorrplot(df,
             method="square",
             type = "lower",
             legend.title = "Correlation\ncoefficient",
             outline.color = "gray",
             hc.order = TRUE,
             lab = TRUE,
             lab_col = "black", 
             lab_size = 4,
             tl.col = "black",
             tl.srt = 45,
             digits = 1) +
    theme(text = element_text(color = "black"), 
          axis.text = element_text(color = "black")) +
    labs(x = element_blank(), y = element_blank())
}
