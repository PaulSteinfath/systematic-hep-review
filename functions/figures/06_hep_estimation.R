figure_hep_estimation_summary <- function(df, save_path = NULL, ext = 'png') {
  # Reference event
  rt_peak_prop_plot <- hist_panel(
    df,
    col = "reference_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Reference event",
    tilt_labels = FALSE,
    preserve_order = TRUE
  )
  
  # Baseline correction
  baseline_def_prop_plot <- hist_panel(
    df,
    col = "baseline_category",
    discrete = TRUE, 
    use_proportion = TRUE,
    title = "Baseline correction",
    tilt_labels = FALSE,
    allowed = c("Yes" = "Yes", "No" = "No", "Both" = "Both"),
    preserve_order = TRUE
  )
  
  # Averaging vs. clustering
  df_deter <- df %>% 
    filter(determination_category != "None") %>%
    mutate(determination_category = factor(determination_category, 
                                           levels = c("Averaging", "Clustering", "Both")))
  avg_cluster_prop_plot <- hist_panel(
    df_deter,
    col = "determination_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Analysis approach",
    tilt_labels = FALSE,
    preserve_order = TRUE
  )
  
  # Primary vs. secondary
  hep_type_prop_plot <- hist_panel(
    df,
    col = "window_type_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Time window type",
    tilt_labels = FALSE,
    custom_labels = c("Primary" = "Primary",
                      "Both" = "Primary &\nsecondary"),
    preserve_order = TRUE
  )
  
  first_row_histograms <- plot_grid(
    rt_peak_prop_plot,
    baseline_def_prop_plot,
    avg_cluster_prop_plot, 
    hep_type_prop_plot,
    ncol = 4,
    labels = c("A", "B", "C", "D"),
    align = "hv",
    axis = "tblr",
    rel_widths = c(1.1, 0.9, 0.9, 0.9)
  )
  
  # Main HEP time windows plots
  hep_average_plot <- create_single_ecg_plot(
    df,
    avg_value = "Averaging",
    shared_limits = c(-300, 1000),
    plot_title = "Averaging over channels / time points",
    reference_var = "hep_relative_to",
    reference_values = c("R-peak", "T-peak"),
    by = "study",
    debug_inset = F
  ) 

  hep_cluster_plot <- create_single_ecg_plot(
    df,
    avg_value = "Clustering",
    shared_limits = c(-300, 1000),
    plot_title = "Cluster-based permutation tests",
    reference_var = "hep_relative_to",
    reference_values = c("R-peak", "T-peak"),
    by = "study",
    debug_inset = F
  ) 

  hep_comparison_row <- plot_grid(
    hep_average_plot, 
    hep_cluster_plot,
    ncol = 2,
    align = "hv",
    axis = "tblr",
    labels = c('E', 'F')
  )
  
  hep_time_windows_combined <- plot_grid(
    first_row_histograms,
    NULL,
    hep_comparison_row,
    ncol = 1,
    rel_heights = c(0.25, 0.02, 0.75)
  )
  
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, paste0("fig6_hep_estimation.", ext)),
      plot = hep_time_windows_combined,
      width = 10, 
      height = 9,
      units = "in",
      dpi = 300,
      device = ext,
      bg = "white"
    )
  }
  
  return(hep_time_windows_combined)
}
