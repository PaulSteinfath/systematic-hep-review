figure_hep_estimation_summary <- function(df, save_path, ext = 'png') {
  
  #cluster / average histogram
  df_hep_determination <- df %>%
    group_by(PMID) %>%
    summarise(
      has_averaging = any(method_category == "Averaging"),
      has_clustering = any(method_category == "Clustering"),
      .groups = "drop"
    ) %>%
    mutate(
      determination_category = case_when(
        has_averaging & has_clustering ~ "Both",
        has_averaging & !has_clustering ~ "Averaging",
        !has_averaging & has_clustering ~ "Clustering",
        TRUE ~ "Other"
      )
    ) %>%
    filter(determination_category != "Other") %>%
    mutate(determination_category = factor(determination_category, 
                                           levels = c("Averaging", "Clustering", "Both")))
  
  avg_cluster_prop_plot <- hist_panel(
    df_hep_determination,
    col = "determination_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Analysis approach",
    tilt_labels = FALSE,
    preserve_order = TRUE
  )
  
  #R-/T-peak histogram
  df_hep_reference <- df %>%
    group_by(PMID) %>%
    summarise(
      has_rpeak = any(hep_relative_to == "R-peak"),
      has_tpeak = any(hep_relative_to == "T-peak"),
      .groups = "drop"
    ) %>%
    mutate(
      reference_category = case_when(
        has_rpeak & has_tpeak ~ "Both",
        has_rpeak & !has_tpeak ~ "R-peak",
        !has_rpeak & has_tpeak ~ "T-peak",
        TRUE ~ "Other"
      )
    ) %>%
    filter(reference_category != "Other") %>%
    mutate(reference_category = factor(reference_category, 
                                       levels = c("R-peak", "T-peak", "Both")))
  
  rt_peak_prop_plot <- hist_panel(
    df_hep_reference,
    col = "reference_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Reference event",
    tilt_labels = FALSE,
    preserve_order = TRUE
  )
  
  #baseline correction histogram
  df_baseline_correction <- df %>%
    group_by(PMID) %>%
    summarise(
      has_yes = any(baseline_defined == "Yes"),
      has_no = any(baseline_defined == "No"),
      .groups = "drop"
    ) %>%
    mutate(
      baseline_category = case_when(
        has_yes & has_no ~ "Both",
        has_yes & !has_no ~ "Yes",
        !has_yes & has_no ~ "No",
        TRUE ~ "Other"
      )
    ) %>%
    filter(baseline_category != "Other") %>%
    mutate(baseline_category = factor(baseline_category, 
                                      levels = c("Yes", "No", "Both")))
  
  baseline_def_prop_plot <- hist_panel(
    df_baseline_correction,
    col = "baseline_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Baseline correction",
    tilt_labels = FALSE,
    preserve_order = TRUE
  )
  
  #primary / secondary histogram
  df_hep_window_type <- df %>%
    group_by(PMID) %>%
    summarise(
      has_primary = any(hep_window_type == "Primary"),
      has_secondary = any(hep_window_type == "Secondary"),
      .groups = "drop"
    ) %>%
    mutate(
      window_type_category = case_when(
        has_primary & has_secondary ~ "Primary &\nsecondary",
        has_primary & !has_secondary ~ "Primary",
        !has_primary & has_secondary ~ "Secondary",
        TRUE ~ "Other"
      )
    ) %>%
    filter(window_type_category != "Other") %>%
    mutate(window_type_category = factor(window_type_category, 
                                         levels = c("Primary", "Secondary", "Primary &\nsecondary")))
  
  hep_type_prop_plot <- hist_panel(
    df_hep_window_type,
    col = "window_type_category",
    discrete = TRUE, use_proportion = TRUE,
    title = "Time window type",
    tilt_labels = FALSE,
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
  
  ggsave(
    filename = file.path(save_path, paste0("fig6_hep_estimation.", ext)),
    plot = hep_time_windows_combined,
    width = 190, 
    height = 228.6,
    units = "mm",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
