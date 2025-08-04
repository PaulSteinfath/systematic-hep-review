plot_rejected_components <- function(df) {

  counts_df <- rejections_per_component_type(df)
  level <- unique(counts_df$level)
  total_count <- unique(counts_df$total)

  p <- ggplot(counts_df, aes(x = reorder(rejected_components, count, decreasing = TRUE),
                             y = percentage)) +
    geom_bar(stat = "identity", 
             fill = common_colors$fill_default, 
             color = "white", 
             linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent, 
                       expand = expansion(mult = c(0, .1))) +
    labs(
      x = "",
      y = "Proportion of pipelines",
      title = "Types of rejected ICA components",
      subtitle = paste("n =", total_count, level)
    ) +
    plot_theme_default

  return(p)
}
