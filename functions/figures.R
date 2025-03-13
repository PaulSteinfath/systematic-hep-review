# Import plotting functions
source(file.path(func_path, "plots", "create_combined_plot.R"))
source(file.path(func_path, "plots", "hist_panel.R"))
source(file.path(func_path, "plots", "create_ica_usage_plot.R"))
source(file.path(func_path, "plots", "create_ica_rej.R"))
source(file.path(func_path, "plots", "create_time_windows_plot.R"))
source(file.path(func_path, "plots", "create_simple_ica_plot.R"))
source(file.path(func_path, "plots", "summarize_cfa_criteria.R"))
source(file.path(func_path, "plots", "rr_intervals_plot.R"))
source(file.path(func_path, "plots", "other_strategy_plot.R"))
source(file.path(func_path, "plots", "epoch_continuous_ica.R"))
source(file.path(func_path, "plots", "minimal_artifact_windows_plot.R"))
source(file.path(func_path, "plots", "create_control_variables_plot.R"))
source(file.path(func_path, "plots", "rejected_cardiac_ics.R"))
source(file.path(func_path, "plots", "create_empty_plot.R"))

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


# Create synthetic ECG wave data
create_ecg_wave <- function(t) {
  # R peak at t=0, T wave at t=300ms
  r_wave <- 1.8 * exp(-(t / 10)^2) # R peak
  q_wave <- -0.1 * exp(-(t + 20)^2 / 100) # Q wave
  s_wave <- -0.15 * exp(-(t - 20)^2 / 100) # S wave
  p_wave <- 0.15 * exp(-(t + 100)^2 / 400) # P wave
  t_wave <- 0.2 * exp(-(t - 300)^2 / 3000) # T wave

  return(p_wave + q_wave + r_wave + s_wave + t_wave)
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

  plot_BC <- plot_grid( # Create a row with ref_offline and simple ICA plot
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

# Here we generate all figures
# Ideally, each panel / figure should be generated by a function that
# accepts the dataframe as the first argument so that the functions could be
# re-used in the Shiny app
make_figures <- function(df, save_path) {
  eeg_acq_prep_plot <- eeg_acq_prep(df)

  ggsave(
    filename = file.path(save_path, "eeg_acq_prep_plot.svg"),
    plot = eeg_acq_prep_plot,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = "svg",
    bg = "white"
  )
  show(eeg_acq_prep_plot)


  cfa_removal_plot <- cfa_removal(df)

  ggsave(
    filename = file.path(save_path, "cfa_removal_plot.svg"),
    plot = cfa_removal_plot,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = "svg",
    bg = "white"
  )

  # Generate and save the control variables plot
  control_vars_plot <- create_control_variables_plot(df)
  
  ggsave(
    filename = file.path(save_path, "control_variables_plot.svg"),
    plot = control_vars_plot,
    width = 7,
    height = 12,
    units = "in",
    dpi = 300,
    device = "svg",
    bg = "white"
  )
  show(control_vars_plot)
}


