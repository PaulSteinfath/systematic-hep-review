plot_control_variables <- function(df) {
  # Get usage stats for all control variables
  controls_df <- control_counts(df,
                                by = "study",
                                level = "variable")
  
  # Filter and arrange data
  controls_df <- controls_df %>%
    mutate(category = factor(category, levels = names(control_category_colors))) %>%
    arrange(category, desc(percentage)) %>%
    mutate(variable = factor(variable, levels = rev(variable)))
  
  # Create plot with adjusted legend position
  total_count <- unique(controls_df$total)
  p <- bar_panel(controls_df, 
                 value_col = "percentage", 
                 column_col = "variable", 
                 color_col = "category",
                 colors = control_category_colors,
                 flip = T, 
                 percentages = T,
                 show_legend = T,
                 title = "Control variables",
                 x_lab = "",
                 y_lab = "Proportion of studies") +
    labs(fill = "Category",
         subtitle = paste0("n = ", total_count, " studies")) +
    theme(
      legend.position = c(0.4, 0.5),
      panel.grid.major.x = element_line(color = common_colors$grid_lines),
      panel.grid.minor = element_blank()
    )
}
