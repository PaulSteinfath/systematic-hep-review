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
source(file.path(func_path, "plots", "plot_hedges_g.R"))
source(file.path(func_path, "plots", "plot_control_categories.R"))
source(file.path(func_path, "plots", "plot_ecg_controls.R"))
source(file.path(func_path, "plots", "plot_hedges_g_adjusted_for_noise.R"))
source(file.path(func_path, "plots", "plot_simulated_effects.R"))

source(file.path(func_path, 'figures', '01_overview_studies.R'))
source(file.path(func_path, 'figures', '06_hep_estimation.R'))
source(file.path(func_path, 'figures', '07_stats.R'))
source(file.path(func_path, 'figures', '08_controls.R'))

create_epoch_simulation_plot <- function(df){
  
  a <- plot_hedges_g_adjusted_for_noise(df, 
                                        sigma_s_vals = c(1), 
                                        sigma_t_vals = c(0.5, 1, 2, 4, 10), 
                                        r_thresh = 4)
  b <- plot_simulated_effects(d_type = 'g', 
                              plot_type = 'pure', 
                              Ns = seq(10, 300, 10), 
                              ks = seq(10, 300, 10), 
                              sigma_ratio = c(0.1, 0.25, 0.5, 1, 2))
 
  p <- plot_grid(
    a, NULL, b, 
    nrow = 1, ncol = 3, 
    labels = c("A", "", "B"),
    rel_widths = c(0.5, 0.05, 0.5)
  )
  
}

create_combined_plot_for_columns <- function(df) {
  
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

  figABC <- plot_grid(p1, NULL, p2, NULL, p3,
                      ncol = 5,
                      align = "h",
                      axis = "l",
                      labels = c("A", "", "B", "", "C"),
                      label_x = c(0.35, NA, -0.13, NA, -0.1), 
                      label_y = c(1, NA, 1, NA, 1),  
                      rel_widths = c(1, 0.025, 0.7, 0.025, 1))

  return(figABC)
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
    y_label = "Individual Studies",
    show_legend = TRUE 
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
    title = "Reference (online)",
    discrete = TRUE, tilt_labels = F,
    modality_filter = "EEG",
    allowed = ref_categories[c(
      "Common average", "Linked mastoids", "Cz", "FCz",
      "Fpz", "CMS and DRL", "CMS", "Nose",
      "Linked earlobes", "Other", "unknown"
    )]
  )

  ref_offline <- hist_panel(df_ref, "reference_offline",
    title = "Reference (offline)",
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
    rel_widths = c(1.2, 0.8)
  )

  # Combine plots
  fig_ABCD <- plot_grid(
    plot_grid(ref_online, ncol = 1, labels = "A"),    
    NULL,                                              # Spacer
    plot_BC,                                          
    plot_grid(ica_rej_plot, ncol = 1, labels = "D"),  
    ncol = 1,
    align = "hv",
    axis = "tblr",
    vjust = 1,
    rel_heights = c(1, 0.05, 1, 1) # A, spacer, B&C, D
    )

  plot_grid(
    fig_ABCD,
    NULL,
    filter_plot,
    nrow = 1,
    labels = c("", "", "E"),
    rel_widths = c(1.2, 0.05, 1),
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
    rel_widths = c(0.25, 0.75),
    label_x = c(0, 0.07),  
    label_y = c(1, 1)
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


figure_ecg_summary <- function(df, save_path, ext = 'png') {
  # Map "unknown" to 9 so that it isn't lost during conversion to numeric and
  # is positioned nicely
  df <- df %>%
    mutate(ecg_num_electrodes = replace(ecg_num_electrodes,
                                        ecg_num_electrodes == "unknown", 9))
  p_ecg_num_electrodes <- hist_panel(df, "ecg_num_electrodes", 
                                     force.numeric = T, binwidth = 1,
                                     x.label = "Number of ECG electrodes",
                                     title = "Number of ECG electrodes") +
    scale_x_continuous(breaks = seq(0, 9),
                       labels = c(seq(0, 8), "N/M"))
  p_ecg_leads <- hist_panel(df, "ecg_lead", fill_as_aesthetic = T,
                            discrete = T, title = "ECG lead") +
    scale_fill_manual(values = leads_palette,
                      na.value = plot_fill_default_single,
                      guide = "none") 
  p_ecg_locations <- plot_ecg_locations(df, leads_palette)
  fig_AB = plot_grid(
    p_ecg_num_electrodes,
    p_ecg_leads,
    ncol = 1,
    labels = c("A", "B")
  )

  fig <- plot_grid(
    fig_AB,
    p_ecg_locations,
    nrow = 1,
    rel_widths = c(1, 1.5),
    labels = c("", "C")
  )
  
  ggsave(
    filename = file.path(save_path, paste0("ecg_summary_plot.", ext)),
    plot = fig,
    width = 10,
    height = 4,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}




# Here we generate all figures
make_figures <- function(df, save_path, ext = "svg") {

  fig1BCD_studies <- studies_overview(df)
  ggsave(
    filename = file.path(save_path, paste0("studies_overview.", ext)),
    plot = fig1BCD_studies,
    width = 190,
    height = 76.2,
    units = "mm",
    dpi = 300,
    device = ext,
    bg = "white"
  )

  eeg_acq_prep_plot <- eeg_acq_prep(df)
  ggsave(
    filename = file.path(save_path, paste0("eeg_acq_prep_plot.", ext)),
    plot = eeg_acq_prep_plot,
    width = 190,
    height = 279.4,
    units = "mm",
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

  pipelines_overview <- create_combined_plot_for_columns(df)

  ggsave(
    filename = file.path(save_path, paste0("pipelines_overview.", ext)),
    plot = pipelines_overview,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(pipelines_overview)

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

  epoch_simulation_plot <- create_epoch_simulation_plot(df)
  ggsave(
    filename = file.path(save_path, paste0("epoch_simulation_plot.", ext)),
    plot = epoch_simulation_plot,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(epoch_simulation_plot)
  
  additional_hedges_g_plot <- plot_hedges_g(df = df, with_clustering = T, with_regression = T)
  ggsave(
    filename = file.path(save_path, paste0("additional_hedges_g_plot.", ext)),
    plot = additional_hedges_g_plot,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
  show(additional_hedges_g_plot)
  
}
