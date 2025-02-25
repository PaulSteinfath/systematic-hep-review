library(cowplot)
library(tidyr)
library(dplyr)
library(stringr)
library(gridGraphics)
library(ggtext)

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
# source(file.path(func_path, "plots", "plotting_helpers.R"))

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
  # For rejected cardiac ICs histogram: convert counts to percentage.
  df_rej_ic <- df %>%
    distinct(PMID, rejected_cardiac_ics, .keep_all = TRUE) %>%
    filter(rejected_cardiac_ics != "")

  rejected_cardiac_ics <- hist_panel(df_rej_ic, "rejected_cardiac_ics",
    x.label = "# rejected cardiac ICs",
    discrete = FALSE,
    binwidth = 1
  ) + scale_y_continuous(
    expand = c(0, 0),
    breaks = function(x) seq(0, ceiling(max(x)), by = 1),
    limits = c(0, NA)
  )

  # Use the imported plotting functions
  plot1 <- rejected_cardiac_ics
  plot2 <- summarize_cfa_criteria(df)
  plot3 <- rr_intervals_plot(df)
  plot4 <- other_strategy_plot(df)
  plot5 <- epoch_continuous_ica(df)
  plot6 <- minimal_artifact_windows_plot(df)

  # Layout: 3 rows, 2 columns
  top_row <- plot_grid(plot5, plot2, ncol = 2, labels = c("A", "B"), align = "hv", axis = "b", rel_widths = c(0.25, 0.75))
  middle_row <- plot_grid(plot1, plot4, ncol = 2, labels = c("C", "D"), align = "hv", rel_widths = c(0.4, 0.6))
  bottom_row <- plot_grid(plot3, plot6, ncol = 2, labels = c("E", "F"), align = "hv")
  plot_grid(top_row, middle_row, bottom_row, ncol = 1)
}

source(file.path(func_path, "plots", "create_time_windows_plot.R"))

# Create time windows with ECG plot (refactored as a function)
create_time_windows_with_ecg_plot <- function(df) {
  # Calculate overall x-axis limits from all three time windows and force inclusion of 0 and 300
  all_limits <- rbind(
    df %>% select(start = hep_start, end = hep_end),
    df %>% select(start = baseline_start_ms, end = baseline_end_ms),
    df %>% select(start = significant_start_ms, end = significant_end_ms)
  ) %>%
    summarise(
      min = min(as.numeric(start), na.rm = TRUE),
      max = max(as.numeric(end), na.rm = TRUE)
    )

  shared_limits <- range(c(all_limits$min, all_limits$max, 0, 300))

  # Define the R and T peak locations precisely
  r_peak_ms <- 0
  t_peak_ms <- 300

  # Create plots with simple plain text titles
  hep_windows_plot <- create_time_windows_plot(df, 
    "hep_start", "hep_end", "hep_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits,
    add_vlines = TRUE) +
    ggtitle("A) HEP Time of Interest") +
    theme(plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 10)))

  baseline_windows_plot <- create_time_windows_plot(df,
    "baseline_start_ms", "baseline_end_ms", "hep_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits,
    add_vlines = FALSE) +
    ggtitle("B) Baseline Window") +
    theme(plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 10)))

  significant_windows_plot <- create_time_windows_plot(df,
    "significant_start_ms", "significant_end_ms", "significant_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits,
    add_vlines = FALSE) +
    ggtitle("C) Significant Effects Found") +
    theme(plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 10)))

  # Create ECG data
  if (!is.null(shared_limits)) {
    t <- seq(shared_limits[1], shared_limits[2], length.out = 1000)
  } else {
    t <- seq(-200, 800, length.out = 1000)
  }
  ecg <- create_ecg_wave(t)
  ecg_df <- data.frame(Time = t, ECG = ecg)
  
  # Regular ECG plot without lines for final display
  ecg_plot <- ggplot(ecg_df, aes(x = Time, y = ECG)) +
    geom_line(color = "grey80", size = 1) +
    labs(x = "Time relative to R-peak (ms)", y = "ECG amplitude") +
    theme_classic() +
    plot_theme_default + # Apply default theme
    theme(
      axis.text.y = element_blank(), # Remove y-axis tick labels
      axis.ticks.y = element_blank() # Remove y-axis ticks
    ) +
    scale_x_continuous(expand = c(0, 0))

  # Combine all density plots
  time_windows_density <- plot_grid(
    hep_windows_plot,
    baseline_windows_plot,
    significant_windows_plot,
    ncol = 1,
    align = "vh"
  )

  base_plot <- plot_grid(
    time_windows_density,
    ecg_plot,
    ncol = 1,
    rel_heights = c(0.75, 0.25),
    align = "v"
  )

  # Adjust these values to align the lines with R and T peaks
  # I determined these values by trial and error :(.
  # Don't know how to add a vertical line spanning the entire match exactly specific time points
  r_line_pos <- 0.2635
  t_line_pos <- 0.483

  final_plot <- ggdraw(base_plot) +
    draw_line(
      x = c(r_line_pos, r_line_pos),
      y = c(0.025, 0.98),
      color = "#0072B2",
      alpha = 0.5,
      linetype = "dashed",
      size = 0.75
    ) +
    draw_line(
      x = c(t_line_pos, t_line_pos),
      y = c(0.025, 0.98),
      color = "#E69F00",
      alpha = 0.5,
      linetype = "dashed",
      size = 0.75
    )

  # Create a simple legend plot
  legend_plot <- ggplot() +
    geom_segment(aes(x = 0, y = 2, xend = 1, yend = 2, color = "R Peak", linetype = "dashed"), size = 1) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, color = "T Peak", linetype = "dashed"), size = 1) +
    scale_color_manual(values = c("R-Peak" = "#0072B2", "T-Peak" = "#E69F00")) +
    scale_linetype_manual(values = c("dashed" = "dashed")) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0),
      legend.text = element_text(size = 8),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    guides(linetype = "none") # Hide the duplicate linetype legend

  # Extract just the legend
  legend_grob <- get_legend(legend_plot)

  # Add the legend to the bottom-right of the ECG panel
  final_plot <- final_plot +
    draw_grob(legend_grob, x = 0.95, y = 0.25, width = 0.25, height = 0.15, hjust = 1, vjust = 1)

  return(final_plot)
}

create_time_windows_with_ecg_plot(df)

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
  show(cfa_removal_plot)


  time_windows_plot <- create_time_windows_with_ecg_plot(df)

  ggsave(
    filename = file.path(save_path, "time_windows_plot.svg"),
    plot = time_windows_plot,
    width = 9,
    height = 7,
    units = "in",
    dpi = 300,
    device = "svg",
    bg = "white"
  )

}
