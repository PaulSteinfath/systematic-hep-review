figure_overview_pipelines <- function(df, save_path, ext = 'svg') {
  
  target_columns <- unlist(pipeline_steps, use.names = FALSE)
  
  # Use entropy to determine column order
  entropy_df <- compute_entropy(df, 
                                method_columns = target_columns,
                                drop_paper_duplicates = TRUE)
  entropy_df$Step <- sapply(entropy_df$Column, function(var_name) {
    get_pipeline_step(var_name)
  })
  entropy_df$Step <- factor(entropy_df$Step, levels = names(pipeline_colors))
  
  #Sort by entropy within each step
  step_order <- c("Statistics", "HER Estimation", "Preprocessing", "Acquisition", "Experiment")
  entropy_df$Step <- factor(entropy_df$Step, levels = step_order)
  entropy_df <- entropy_df %>%
    dplyr::arrange(Step, Entropy) %>%
    dplyr::mutate(Column = factor(Column, levels = unique(Column)))
  
  # Final fixed order to reuse
  ordered_columns_original <- levels(entropy_df$Column)
  
  # Build all three plots with unified config
  p1 <- plot_entropy(entropy_df = entropy_df,
                     column_mapping_readable = column_mapping_readable_default,
                     pipeline_steps = pipeline_steps,
                     pipeline_colors = pipeline_colors,
                     fixed = TRUE,
                     flip = TRUE,
                     show_title = TRUE,
                     show_wordy_title = TRUE)
  
  
  p2 <- plot_multiple_choices(df,
                              variables = ordered_columns_original,
                              column_mapping_readable = column_mapping_readable_default,
                              pipeline_steps = pipeline_steps,
                              pipeline_colors = pipeline_colors,
                              fixed = TRUE,
                              flip = TRUE,
                              y_ticks = FALSE,
                              show_title = TRUE,
                              show_wordy_title = TRUE,
                              x_lab = "")
  p3 <- plot_missing(df,
                     columns = ordered_columns_original,
                     column_mapping_readable = column_mapping_readable_default,
                     pipeline_steps = pipeline_steps,
                     pipeline_colors = pipeline_colors,
                     fixed = TRUE,
                     flip = TRUE,
                     y_ticks = FALSE,
                     show_title = TRUE,
                     show_wordy_title = TRUE,
                     x_lab = "", 
                     show_legend = TRUE)
  
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
