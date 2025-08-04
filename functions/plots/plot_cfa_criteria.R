plot_cfa_criteria <- function(df) {
  main_counts <- cfa_criteria_counts(df_included, 
                                     mapping = allowed$cfa_criteria)
  level <- unique(main_counts$level)
  total_count <- unique(main_counts$total)
  
  # Create main plot
  main_plot <- ggplot(main_counts, 
                      aes(x = reorder(cfa_rej_criteria, count, decreasing = TRUE), 
                          y = percentage)) +
    geom_bar(stat = "identity", 
             fill = common_colors$fill_default, 
             color = "white", 
             linewidth = 0.5) +
    plot_theme_default +
    custom_theme() +
    scale_y_continuous(labels = scales::percent, 
                       expand = expansion(mult = c(0, .1))) +
    labs(
      x = "",
      y = paste("Proportion of", level),
      title = "CFA rejection criteria",
      subtitle = paste("n =", total_count, level)
    ) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.title.y = element_text(size = 9)
    )

  # Create algorithm inset data
  algo_counts <- cfa_criteria_counts(df_included, 
                                     mapping = c("iclabel" = "ICLabel", 
                                                 "sasica" = "SASICA", 
                                                 "corrmap" = "CORRMAP"))
  
  # Exit immediately if not enough info for algo_plot
  if (nrow(algo_counts) == 0) {
    return(main_plot)
  }

  # Create algorithm inset plot
  algo_plot <- ggplot(algo_counts, 
                      aes(x = reorder(cfa_rej_criteria, count, decreasing = TRUE), 
                          y = count)) +
    geom_bar(stat = "identity", 
             fill = common_colors$fill_default, 
             color = "white", 
             width = 0.7) +
    labs(x = "",
         y = "Number of studies",
         title = "Algorithm") +
    scale_y_continuous(expand = c(0, 0)) +
    plot_theme_default +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 2, l = 2))
    )

  # Combine plots
  combined_plot <- ggdraw() +
    draw_plot(main_plot) +
    draw_plot(algo_plot, x = 0.6, y = 0.5, width = 0.35, height = 0.4)

  return(combined_plot)
}
