create_empty_plot <- function(message = "No data available") {
  ggplot() + 
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    ) +
    annotate("text", x = 0.5, y = 0.5, label = message, size = 4) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
}
