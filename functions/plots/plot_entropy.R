plot_entropy <- function(entropy_df,
                         column_mapping_readable = column_mapping_readable_default,
                         pipeline_steps = NULL,
                         pipeline_colors = NULL,
                         x_lab = "Methodological Choice",
                         y_lab = "Entropy",
                         plot_fill = plot_fill_default_single,
                         show_wordy_title = FALSE,
                         show_title = FALSE,
                         show_legend = FALSE,
                         tilt_labels = FALSE,
                         x_ticks = TRUE,
                         y_ticks = TRUE,
                         flip = FALSE,
                         fixed = FALSE) {
  
  # Apply readable mapping to Column names
  entropy_df$Column <- apply_column_mapping(entropy_df$Column, column_mapping_readable)
  
  # For provided entropy_df, preserve the existing order (it's already sorted in figures.R)
  if (fixed) {
    entropy_df$Column <- factor(entropy_df$Column, levels = unique(entropy_df$Column))
  }
  
  my_title <- if (!show_title) {
    NULL
  } else if (show_wordy_title) {
    "Entropy" 
  } else {
    paste("n =", dplyr::n_distinct(df$PMID), "studies")
  }
  
  p <- ggplot(entropy_df, aes(x = Column, y = Entropy)) +
    theme_classic(base_family = "sans") +
    labs(title = my_title, x = x_lab, y = y_lab) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(size = 9,
                                 angle = if (tilt_labels) 45 else 0,
                                 hjust = if (tilt_labels) 1 else 0.5),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.title.y = element_text(size = 9)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  if (!is.null(pipeline_colors)) {
    p <- p + geom_bar(aes(fill = Step), stat = "identity", color = "white", linewidth = 0.5) +
      scale_fill_manual(values = pipeline_colors, guide = if (show_legend) "legend" else "none") +
      theme(legend.position = "right", legend.justification = "center", legend.margin = margin(0, -1, 0, 0))
  } else {
    p <- p + geom_bar(stat = "identity", fill = plot_fill, color = "white", linewidth = 0.5)
  }
  
  if (!x_ticks) p <- p + theme(axis.text.x = element_blank())
  if (!y_ticks) {
    p <- p +
      theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  }
  if (flip)     p <- p + coord_flip()
  
  return(p)
}