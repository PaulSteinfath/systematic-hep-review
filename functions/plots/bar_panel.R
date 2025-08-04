bar_panel <- function(df,
                      value_col,
                      column_col = "Column",
                      color_col = "Step",
                      colors = NULL,
                      plot_fill = plot_fill_default_single,
                      percentages = F,
                      x_lab = "Methodological choice",
                      y_lab = "",
                      title = NULL,
                      x_ticks = T,
                      y_ticks = T,
                      tilt_labels = F,
                      show_legend = F,
                      flip = F) 
{
  p <- ggplot(df, aes(x = !!sym(column_col), y = !!sym(value_col)))
  
  if (!is.null(colors)) {
    p <- p + 
      geom_bar(aes(fill = !!sym(color_col)), stat = "identity", color = "white", linewidth = 0.5) +
      scale_fill_manual(
        name = "Category",
        values = colors, 
        guide = if (show_legend) "legend" else "none"
      )
  } else {
    p <- p + 
      geom_bar(stat = "identity", fill = plot_fill, color = "white", linewidth = 0.5)
  }
  
  p <- p + scale_y_continuous(labels = if (percentages) scales::percent else waiver(),
                              expand = expansion(mult = c(0, .1)))
  
  if (flip) {
    p <- p + coord_flip()
  }
  
  # Set labels
  if (!is.null(title)) {
    # Add empty subtitle to get some offset
    p <- p + labs(title = title)
  }
  if (!is.null(x_lab)) {
    p <- p + labs(x = x_lab)
  }
  if (!is.null(y_lab)) {
    p <- p + labs(y = y_lab)
  }
  
  # Set theme
  p <- p + theme_classic(base_family = "sans") +
    plot_theme_default
    theme(
      axis.text.x = element_text(angle = if (tilt_labels) 45 else 0,
                                 hjust = if (tilt_labels) 1 else 0.5),
        legend.position = c(0.35, 0.35), # move legend well inside plot area
        legend.justification = c("left", "center"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0)
    )

  # Hide ticks if necessary
  if (!x_ticks) {
    p <- p + theme(axis.text.x = element_blank())
  }
  if (!y_ticks) {
    p <- p +
      theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  }
  
  p
}