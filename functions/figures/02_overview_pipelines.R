figure_overview_pipelines <- function(df, save_path, ext = 'svg') {
  # Consider columns from all categories
  target_columns <- unlist(pipeline_steps, use.names = FALSE)
  
  # Use entropy to determine column order
  entropy_df <- compute_entropy(df, 
                                method_columns = target_columns,
                                num_bins = params$entropy_num_bins, 
                                unique_threshold = params$entropy_unique_threshold, 
                                drop_paper_duplicates = TRUE)
  entropy_df$Step <- sapply(entropy_df$Column, function(var_name) {
    get_pipeline_step(var_name)
  })
  
  # Sort by entropy within each step
  step_order <- c("Statistics", 
                  "HER Estimation", 
                  "Preprocessing", 
                  "Acquisition", 
                  "Experiment")
  entropy_df$Step <- factor(entropy_df$Step, levels = step_order)
  entropy_df <- entropy_df %>%
    dplyr::arrange(Step, Entropy) %>%
    dplyr::mutate(Column = factor(Column, levels = unique(Column)))
  
  # Final fixed order to reuse
  ordered_columns <- levels(entropy_df$Column)
  entropy_df <- prepare_column_plot_data(entropy_df, ordered_columns,
                                         pipeline_steps, pipeline_colors)
  
  # Multiple choices
  multiple_df <- multiple_choices(df, target_columns)
  multiple_df <- prepare_column_plot_data(multiple_df, ordered_columns,
                                          pipeline_steps, pipeline_colors)
  
  # Missing information
  missing_df <- missing_information(df, target_columns)
  missing_df <- prepare_column_plot_data(missing_df, ordered_columns,
                                         pipeline_steps, pipeline_colors)
  
  # Build all three plots with unified config
  p1 <- bar_panel(entropy_df,
                  value_col = "Entropy",
                  flip = T,
                  colors = pipeline_colors,
                  title = "Entropy",
                  y_lab = "Entropy")
  
  p2 <- bar_panel(multiple_df,
                  value_col = "percentage",
                  percentages = T,
                  flip = T,
                  colors = pipeline_colors,
                  title = "Multiple Choices",
                  x_lab = "",
                  y_lab = "Proportion of Studies",
                  y_ticks = F)
  
  p3 <- bar_panel(missing_df,
                  value_col = "percentage",
                  percentages = T,
                  flip = T,
                  colors = pipeline_colors,
                  title = "Missing Information",
                  x_lab = "",
                  y_lab = "Proportion of Studies",
                  y_ticks = F,
                  show_legend = T)
  
  # Combine plots
  fig <- plot_grid(p1, NULL, p2, NULL, p3,
                   ncol = 5,
                   align = "h",
                   axis = "l",
                   labels = c("A", "", "B", "", "C"),
                   label_x = c(0.35, NA, -0.13, NA, -0.1), 
                   label_y = c(1, NA, 1, NA, 1),  
                   rel_widths = c(1, 0.025, 0.7, 0.025, 1))
  
  ggsave(
    filename = file.path(save_path, paste0("fig2_pipelines_overview.", ext)),
    plot = fig,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
