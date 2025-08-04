plot_cfa_algorithm <- function(df) {
  # Create algorithm inset data
  algo_counts <- cfa_criteria_counts(df, 
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
         y = "Number of\nstudies",
         title = "Algorithm") +
    scale_y_continuous(expand = c(0, 0)) +
    plot_theme_default +
    theme(
      title = element_text(size = 11),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 2, l = 2))
    )
  
}