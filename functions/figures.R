# Import plotting functions
source(file.path(func_path, "plots", "plotting_helpers.R"))
source(file.path(func_path, "plots", "utils.R"))
source(file.path(func_path, "plots", "preprocess_controls.R"))
source(file.path(func_path, "plots", "create_combined_plot.R"))
source(file.path(func_path, "plots", "hist_panel.R"))
source(file.path(func_path, "plots", "create_ica_usage_plot.R"))
source(file.path(func_path, "plots", "create_ica_rej.R"))
source(file.path(func_path, "plots", "create_simple_ica_plot.R"))
source(file.path(func_path, "plots", "summarize_cfa_criteria.R"))
source(file.path(func_path, "plots", "rr_intervals_plot.R"))
source(file.path(func_path, "plots", "other_strategy_plot.R"))
source(file.path(func_path, "plots", "epoch_continuous_ica.R"))
source(file.path(func_path, "plots", "minimal_artifact_windows_plot.R"))
source(file.path(func_path, "plots", "create_control_variables_plot.R"))
source(file.path(func_path, "plots", "create_control_categories_plot.R"))
source(file.path(func_path, "plots", "rejected_cardiac_ics.R"))
source(file.path(func_path, "plots", "plot_eeg_locations.R"))
source(file.path(func_path, "plots", "plot_ecg_locations.R"))
source(file.path(func_path, "plots", "plot_missing.R"))
source(file.path(func_path, "plots", "plot_multiple_choices.R"))
source(file.path(func_path, "plots", "plot_entropy.R"))
source(file.path(func_path, "plots", "create_time_windows_plot.R"))
source(file.path(func_path, "plots", "create_single_ecg_plot.R"))

create_combined_plot_for_columns <- function(df){

  analysis_steps <- colnames(df)[6:45] 

  p1 <- plot_entropy(df,method_columns = analysis_steps, column_mapping_readable = column_mapping_readable_default, vertical = F, align_by_magnitude = F,  x_lab = "", x_ticks = FALSE)

  p2 <- plot_multiple_choices(df,variables = analysis_steps, column_mapping_readable = column_mapping_readable_default, vertical = F, align_by_magnitude = F, x_lab = "", x_ticks = FALSE)

  p3 <- plot_missing(df, columns = analysis_steps, column_mapping_readable = column_mapping_readable_default, vertical = F, align_by_magnitude = F, x_ticks = TRUE)

  fig_ABC <- plot_grid(
    p1, p2, p3, 
    ncol = 1, 
    align = "v",
    axis = "l",
    labels = c("A", "B", "C"),
    vjust = 1,
    rel_heights = c(1, 1, 1.2)
  )

  return(fig_ABC)

}

# Create filter cutoff plots
create_filter_plots <- function(df) {
  filter_plot <- create_combined_plot(
    df = df,
    start_var = "high_pass",
    end_var = "low_pass",
    x_scale = "log",
    custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
    x_label = "Filter Cutoff (Hz)",
    y_label = "Individual Studies"
  )
  return(filter_plot)
}

# Create EEG Acquisition & Preprocessing
eeg_acq_prep <- function(df) {
  # Define major reference categories once
  major_categories <- tolower(c(
    "Cz", "Nose", "Linked earlobes", "Linked mastoids",
    "FCz", "Common average", "Fpz", "CMS", "CMS and DRL",
    "unknown", "Laplacian reference", "REST"
  ))

  # Process reference categories
  df_ref <- df %>%
    mutate(across(
      .cols = c(reference_online, reference_offline),
      .fns = ~ case_when(
        tolower(.) %in% major_categories ~ tolower(.),
        TRUE ~ "Other"
      )
    ))

  # Common reference category mapping
  ref_categories <- c(
    "Common average" = "CAR",
    "Linked mastoids" = "LM",
    "Linked earlobes" = "LE",
    "Cz" = "Cz",
    "FCz" = "FCz",
    "Fpz" = "Fpz",
    "CMS" = "CMS",
    "CMS and DRL" = "CMS",
    "Nose" = "Nose",
    "Laplacian reference" = "LAP",
    "REST" = "REST",
    "Other" = "Other",
    "unknown" = "N/M"
  )

  # Create individual histogtams for online / offline references
  ref_online <- hist_panel(df_ref, "reference_online",
    x.label = "Reference (online)",
    discrete = TRUE, tilt_labels = F,
    modality_filter = "EEG",
    allowed = ref_categories[c(
      "Common average", "Linked mastoids", "Cz", "FCz",
      "Fpz", "CMS and DRL", "CMS", "Nose",
      "Linked earlobes", "Other", "unknown"
    )]
  )

  ref_offline <- hist_panel(df_ref, "reference_offline",
    x.label = "Reference (offline)",
    discrete = TRUE, tilt_labels = F,
    modality_filter = "EEG",
    allowed = ref_categories[c(
      "Common average", "Linked mastoids", "Linked earlobes",
      "Laplacian reference", "unknown", "Other"
    )]
  )

  # Create plots for filtering cutoffs, ICA rejection, and ICA usage
  filter_plot <- create_filter_plots(df)
  ica_rej_plot <- create_ica_rej(df)
  ica_simple_plot <- create_simple_ica_plot(df)

  plot_BC <- plot_grid( 
    ref_offline,
    ica_simple_plot,
    ncol = 2,
    align = "hv",
    axis = "tblr",
    labels = c("B", "C"),
    rel_widths = c(1.4, 0.6)
  )

  # Combine plots
  fig_ABC <- plot_grid(
    ref_online, plot_BC, ica_rej_plot,
    ncol = 1,
    align = "h",
    axis = "l",
    labels = c("A", "", "D"),
    vjust = 1
  )

  plot_grid(
    fig_ABC,
    NULL,
    filter_plot,
    nrow = 1,
    align = "hv",
    axis = "tblr",
    labels = c("", "", "E"),
    rel_widths = c(1.2, 0.05, 1),
    hjust = 0.5,
    vjust = 1
  )
}


# CFA Removal
cfa_removal <- function(df) {

  ica_usage_plot <- epoch_continuous_ica(df)
  cfa_criteria_plot <- summarize_cfa_criteria(df)
  cardiac_ics_plot <- rejected_cardiac_ics(df)
  other_strategies_plot <- other_strategy_plot(df)
  rr_plot <- rr_intervals_plot(df)
  minimal_artifact_plot <- minimal_artifact_windows_plot(df, t_peak_offset = 300)
 
  # Combine subplots into rows
  top_row <- plot_grid(
    ica_usage_plot,
    cfa_criteria_plot,
    ncol = 2, labels = c("A", "B"),
    align = "v", axis = "b",
    rel_widths = c(0.25, 0.75)
  )

  middle_row <- plot_grid(
    cardiac_ics_plot,
    other_strategies_plot,
    ncol = 2, labels = c("C", "D"),
    align = "hv", rel_widths = c(0.4, 0.6)
  )

  bottom_row <- plot_grid(
    rr_plot,
    minimal_artifact_plot,
    ncol = 2, labels = c("E", "F"),
    align = "hv"
  )

  plot_grid(top_row, middle_row, bottom_row, ncol = 1)
}


ecg_summary <- function(df) {
  # Map "unknown" to 9 so that it isn't lost during conversion to numeric and
  # is positioned nicely
  df <- df %>%
    mutate(ecg_num_electrodes = replace(ecg_num_electrodes,
                                        ecg_num_electrodes == "unknown", 9))
  p_ecg_num_electrodes <- hist_panel(df, "ecg_num_electrodes", 
                                     force.numeric = T, binwidth = 1,
                                     x.label = "Number of ECG electrodes") +
    scale_x_continuous(breaks = seq(0, 9),
                       labels = c(seq(0, 8), "N/M"))
  p_ecg_leads <- hist_panel(df, "ecg_lead", fill_as_aesthetic = T,
                            discrete = T, x.label = "ECG lead") +
    scale_fill_manual(values = leads_palette,
                      na.value = plot_fill_default_single,
                      guide = "none") 
  p_ecg_locations <- plot_ecg_locations(df, leads_palette)
  fig_AB = plot_grid(
    p_ecg_num_electrodes,
    p_ecg_leads,
    ncol = 1,
    labels = c("A", "B"),
    align = "v",
    axis = "lr"
  )

  fig <- plot_grid(
    fig_AB,
    p_ecg_locations,
    nrow = 1,
    rel_widths = c(1, 1.5),
    labels = c("", "C"),
    align = "h",
    axis = "tb"
  )
  fig
}

eeg_locations_summary <- function(df) {
  # Temporary function for testing the display of EEG locations
  p_separate_primary <- plot_eeg_locations(df[df$hep_window_type == "Primary",], 
                                           "hep_channels_selected", combined = F) +
    guides(fill = guide_none())
  p_separate_secondary <- plot_eeg_locations(df[df$hep_window_type == "Secondary",],
                                             "hep_channels_selected", combined = F) +
    guides(fill = guide_none())
  p_separate_significant <- plot_eeg_locations(df, "significant_channels", 
                                               lim = NULL, combined = F) +
    guides(fill = guide_none())

  p_combined_primary <- plot_eeg_locations(df[df$hep_window_type == "Primary",], 
                                           "hep_channels_selected", combined = T) +
    labs(title = "Primary")
  p_combined_secondary <- plot_eeg_locations(df[df$hep_window_type == "Secondary",],
                                             "hep_channels_selected", combined = T) +
    labs(title = "Secondary")
  p_combined_significant <- plot_eeg_locations(df, "significant_channels", 
                                               lim = NULL, combined = T) +
    labs(title = "Significant")

  fig <- plot_grid(
    p_separate_primary, p_combined_primary,
    p_separate_secondary, p_combined_secondary,
    p_separate_significant, p_combined_significant,
    nrow = 3, ncol = 2, rel_widths = c(4, 1.5)
    )
  fig
}


create_hep_time_windows_summary_plot <- function(df) {

  #cluster / average histogram
  df_hep_method_prop <- df %>%
    filter(method_category %in% c("Averaging", "Clustering"))

  avg_cluster_prop_plot <- hist_panel(
    df_hep_method_prop,
    col = "method_numeric",
    discrete = TRUE, use_proportion = TRUE,
    x.label = "HEP Determination",
    custom_labels = c("Clustering", "Averaging"), 
    tilt_labels = FALSE
  )
  
  #R-/T-peak histogram
  rt_peak_prop_plot <- hist_panel(
    df,
    col = "hep_relative_to",
    discrete = TRUE, use_proportion = TRUE,
    x.label = "HEP Reference",
    tilt_labels = FALSE
  )
 
  #baseline correction histogram
  baseline_def_prop_plot <- hist_panel(
    df,
    col = "baseline_defined",
    discrete = TRUE, use_proportion = TRUE,
    x.label = "Baseline Correction",
    custom_labels = c("No", "Yes"),
    tilt_labels = FALSE
  )

  #primary / secondary histogram
  hep_type_prop_plot <- hist_panel(
    df,
    col = "hep_window_type",
    discrete = TRUE, use_proportion = TRUE,
    x.label = "HEP Window Type",
    tilt_labels = FALSE
  )

  first_row_histograms <- plot_grid(
    avg_cluster_prop_plot, rt_peak_prop_plot,
    baseline_def_prop_plot, hep_type_prop_plot,
    ncol = 4,
    labels = c("A", "B", "C", "D"),
    align = "hv",
    axis = "tblr"
  )

  # Main HEP time windows plots
  hep_average_plot <- create_single_ecg_plot(
    df,
    avg_value = "Averaging",
    shared_limits = c(-300, 1000),
    plot_title = "E) HEP Windows (Averaging)",
    reference_var = "hep_relative_to",
    reference_values = c("R-peak", "T-peak")
  )

  hep_cluster_plot <- create_single_ecg_plot(
    df,
    avg_value = "Clustering",
    shared_limits = c(-300, 1000),
    plot_title = "F) HEP Windows (Clustering)",
    reference_var = "hep_relative_to",
    reference_values = c("R-peak", "T-peak")
  )

  hep_comparison_row <- plot_grid(
    hep_average_plot, 
    hep_cluster_plot,
    ncol = 2,
    align = "hv",
    axis = "tblr",
    label_x = 0.01,
    hjust = 0
  )

  hep_time_windows_combined <- plot_grid(
    first_row_histograms,
    hep_comparison_row,
    ncol = 1,
    rel_heights = c(0.25, 0.75)
  )
  return(hep_time_windows_combined)
}

# Here we generate all figures
make_figures <- function(df, save_path, ext = "svg") {

  eeg_acq_prep_plot <- eeg_acq_prep(df)

  ggsave(
    filename = file.path(save_path, paste0("eeg_acq_prep_plot.", ext)),
    plot = eeg_acq_prep_plot,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(eeg_acq_prep_plot)


  cfa_removal_plot <- cfa_removal(df)

  ggsave(
    filename = file.path(save_path, paste0("cfa_removal_plot.", ext)),
    plot = cfa_removal_plot,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(cfa_removal_plot)


  combined_plot_for_columns <- create_combined_plot_for_columns(df)

  ggsave(
    filename = file.path(save_path, paste0("combined_plot_for_columns.", ext)),
    plot = combined_plot_for_columns,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = "svg",
    bg = "white"
  )

  control_vars_plot <- create_control_variables_plot(df)

  ggsave(
    filename = file.path(save_path, paste0("control_variables_plot.", ext)),
    plot = control_vars_plot,
    width = 7,
    height = 12,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(control_vars_plot)


  control_categories_plot <- create_control_categories_plot(df)

  ggsave(
    filename = file.path(save_path, paste0("control_categories_plot.", ext)),
    plot = control_categories_plot,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(control_categories_plot)


  ecg_summary_plot <- ecg_summary(df)

  ggsave(
    filename = file.path(save_path, paste0("ecg_summary_plot.", ext)),
    plot = ecg_summary_plot,
    width = 10,
    height = 4,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(ecg_summary_plot)

  eeg_summary_plot <- eeg_locations_summary(df)
  ggsave(
    filename = file.path(save_path, paste0("eeg_summary_plot.", ext)),
    plot = eeg_summary_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(eeg_summary_plot)

 
  hep_time_windows_combined <- create_hep_time_windows_summary_plot(df)
  ggsave(
    filename = file.path(save_path, paste0("hep_time_windows_combined.", ext)),
    plot = hep_time_windows_combined,
    width = 13, 
    height = 9,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(hep_time_windows_combined)
}
