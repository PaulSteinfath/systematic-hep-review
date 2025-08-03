# Import plotting functions
source(file.path(func_path, "plots", "plotting_helpers.R"))
source(file.path(func_path, "plots", "utils.R"))
source(file.path(func_path, "plots", "plot_segments.R"))
source(file.path(func_path, "plots", "bar_panel.R"))
source(file.path(func_path, "plots", "hist_panel.R"))
source(file.path(func_path, "plots", "create_ica_usage_plot.R"))
source(file.path(func_path, "plots", "plot_rejected_components.R"))
source(file.path(func_path, "plots", "summarize_cfa_criteria.R"))
source(file.path(func_path, "plots", "rr_intervals_plot.R"))
source(file.path(func_path, "plots", "other_strategy_plot.R"))
source(file.path(func_path, "plots", "minimal_artifact_windows_plot.R"))
source(file.path(func_path, "plots", "plot_control_variables.R"))
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
source(file.path(func_path, 'figures', '04_ecg_summary.R'))
source(file.path(func_path, 'figures', '05_cfa_removal.R'))
source(file.path(func_path, 'figures', '06_hep_estimation.R'))
source(file.path(func_path, 'figures', '07_stats.R'))
source(file.path(func_path, 'figures', '08_controls.R'))
source(file.path(func_path, 'figures', 's1_additional_hedges_g.R'))
source(file.path(func_path, 'figures', 's3_control_variables.R'))

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


# Here we generate all figures
make_figures <- function(df, save_path, ext = "svg") {

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
}
