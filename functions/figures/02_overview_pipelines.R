figure_overview_pipelines <- function(df, save_path = NULL, ext = 'svg') {
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
                  x_lab = "",
                  y_lab = "Entropy")
  
  p2 <- bar_panel(multiple_df,
                  value_col = "percentage",
                  percentages = T,
                  flip = T,
                  colors = pipeline_colors,
                  title = "Multiple choices",
                  x_lab = "",
                  y_lab = "Proportion of studies",
                  y_ticks = F)
  
  p3 <- bar_panel(missing_df,
                  value_col = "percentage",
                  percentages = T,
                  flip = T,
                  colors = pipeline_colors,
                  title = "Unreported information",
                  x_lab = "",
                  y_lab = "Proportion of studies",
                  y_ticks = F,
                  show_legend = F)
  
  # Combine plots
  fig <- plot_grid(NULL, p1, NULL, p2, NULL, p3,
                   nrow = 1,
                   align = "h",
                   axis = "l",
                   labels = c("", "A", "", "B", "", "C"),
                   label_x = c(NA, 0.35, NA, -0.13, NA, -0.1), 
                   label_y = c(NA, 1, NA, 1, NA, 1),  
                   rel_widths = c(0.1, 1.1, 0.025, 0.6, 0.025, 0.6))
  
  if (!is.null(save_path)) {
    save_figure(fig,
              aspect_ratio = 1.1,  # height / width
              save_path,
              filename = "fig2_overview_pipelines",
              ext = ext)
  }
  
  return(fig)
}
