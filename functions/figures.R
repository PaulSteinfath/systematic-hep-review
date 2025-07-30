# Import plotting functions
source(file.path(func_path, "plots", "plotting_helpers.R"))
source(file.path(func_path, "plots", "utils.R"))
source(file.path(func_path, "plots", "preprocess_controls.R"))
source(file.path(func_path, "plots", "plot_segments.R"))
source(file.path(func_path, "plots", "bar_panel.R"))
source(file.path(func_path, "plots", "hist_panel.R"))
source(file.path(func_path, "plots", "create_ica_usage_plot.R"))
source(file.path(func_path, "plots", "plot_rejected_components.R"))
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
source(file.path(func_path, "plots", "create_time_windows_plot.R"))
source(file.path(func_path, "plots", "create_single_ecg_plot.R"))
source(file.path(func_path, "plots", "plot_hedges_g.R"))
source(file.path(func_path, "plots", "plot_control_categories.R"))
source(file.path(func_path, "plots", "plot_ecg_controls.R"))
source(file.path(func_path, "plots", "plot_hedges_g_adjusted_for_noise.R"))
source(file.path(func_path, "plots", "plot_simulated_effects.R"))

source(file.path(func_path, 'figures', '01_overview_studies.R'))
source(file.path(func_path, 'figures', '02_overview_pipelines.R'))
source(file.path(func_path, 'figures', '03_meeg_acq_prep.R'))
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
