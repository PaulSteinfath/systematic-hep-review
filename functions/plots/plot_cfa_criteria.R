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
    scale_y_continuous(labels = scales::percent, 
                       expand = expansion(mult = c(0, .1))) +
    labs(
      x = "",
      y = paste("Proportion of", level),
      title = "Criteria for rejecting CFA ICs",
      subtitle = paste("n =", total_count, level)
    )

  return(main_plot)
}
