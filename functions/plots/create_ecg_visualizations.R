#' Create time windows visualization with ECG traces
#' 
#' Contains functions for creating ECG visualizations: 
#' - create_time_windows_with_ecg_plot: Main function called by other scripts
#' - create_single_ecg_plot: Helper function for single condition plot

# Create time windows including ECG trace - creates separate plots
create_time_windows_with_ecg_plot <- function(df, averaging_type = "both") {
  
  shared_limits <- c(-300, 1000)
  
  # For averaging_type "both", create both plots
  if (averaging_type == "both") {
    avg_plot <- create_single_ecg_plot(df, "1", shared_limits, "Averaging")
    cluster_plot <- create_single_ecg_plot(df, "0", shared_limits, "Clustering")
    return(list(averaging = avg_plot, clustering = cluster_plot))
  } else {
    # For specific averaging_type, return just that plot
    return(create_single_ecg_plot(df, averaging_type, shared_limits, 
                                  ifelse(averaging_type == "1", "Averaging", "Clustering")))
  }
}

# Helper function to create a single ECG plot with time windows
create_single_ecg_plot <- function(df, avg_value, shared_limits, plot_title) {

  df_filtered <- df %>% filter(averaging_time == avg_value)
  
  # Create plots for HEP time windows
  if (nrow(df_filtered %>% filter(!is.na(hep_start), !is.na(hep_end))) > 0) {
    hep_plot <- create_time_windows_plot(df_filtered, 
      "hep_start", "hep_end", "hep_relative_to",
      "Time relative to R-peak (ms)",
      t_peak_offset = 300,
      x_limits = shared_limits) +
      ggtitle("A) HEP Time of Interest") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  } else {
    hep_plot <- create_empty_plot("No valid data", x_limits = shared_limits) +
      ggtitle("A) HEP Time of Interest") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  }

  # Create plots for baseline windows
  if (nrow(df_filtered %>% filter(!is.na(baseline_start_ms), !is.na(baseline_end_ms))) > 0) {
    baseline_plot <- create_time_windows_plot(df_filtered,
      "baseline_start_ms", "baseline_end_ms", "hep_relative_to",
      "Time relative to R-peak (ms)",
      t_peak_offset = 300,
      x_limits = shared_limits) +
      ggtitle("B) Baseline Window") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  } else {
    baseline_plot <- create_empty_plot("No valid data", x_limits = shared_limits) +
      ggtitle("B) Baseline Window") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  }

  # For Significant effects we use hep_start/end if averaging ==1 otherwise use significant_start/end
  df_signif <- df_filtered %>% filter(significant_test == 1)
  
  # Create and check for significant effects data
  if (nrow(df_signif) > 0) {
    # Add merged_start/end columns
    df_signif <- df_signif %>%
      mutate(
        merged_start = if_else(averaging_time == "1", as.character(hep_start), as.character(significant_start_ms)),
        merged_end   = if_else(averaging_time == "1", as.character(hep_end), as.character(significant_end_ms)),
        merged_ref   = if_else(averaging_time == "1", hep_relative_to, significant_relative_to)
      )
      
    significant_plot <- create_time_windows_plot(
      df_signif,
      "merged_start",
      "merged_end",
      "merged_ref",
      "Time relative to R-peak (ms)",
      t_peak_offset = 300,
      x_limits = shared_limits
    ) +
      ggtitle("C) Significant Effects Found") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  } else {
    significant_plot <- create_empty_plot("No valid data", x_limits = shared_limits) +
      ggtitle("C) Significant Effects Found") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  }

  # Combine all density plots in a vertical stack
  time_windows_density <- plot_grid(
    hep_plot,
    baseline_plot,
    significant_plot,
    ncol = 1,
    align = "vh"
  )

  # Create ECG trace 
  r_peak_ms <- 0
  t_peak_ms <- 300

  # Ensure we have finite limits for the sequence
  x_min <- max(min(shared_limits), -1000)  # Reasonable lower bound
  x_max <- min(max(shared_limits), 2000)   # Reasonable upper bound
  t <- seq(x_min, x_max, length.out = 1000)
  ecg <- create_ecg_wave(t)
  ecg_df <- data.frame(Time = t, ECG = ecg)
  
  # ECG plot
  ecg_plot <- ggplot(ecg_df, aes(x = Time, y = ECG)) +
    geom_line(color = "grey80", size = 1) +
    geom_vline(aes(xintercept = r_peak_ms, color = "R Peak"), linetype = "dashed", alpha = 0.5) +
    geom_vline(aes(xintercept = t_peak_ms, color = "T Peak"), linetype = "dashed", alpha = 0.5) +
    scale_color_manual(values = r_t_peak_palette, name = NULL) + 
    labs(x = "Time relative to R-peak (ms)", y = "ECG amplitude") +
    theme_classic() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = c(0.85, 0.8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.8, "lines")
    ) +
    scale_x_continuous(expand = c(0, 0))

  # Combine density plots and ECG trace
  full_plot <- plot_grid(
    time_windows_density,
    ecg_plot,  
    ncol = 1,
    rel_heights = c(0.7, 0.3),
    align = "v"
  ) 
  
  # Add main title for the plot
  title <- ggdraw() + 
    draw_label(plot_title, fontface = 'bold', x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 0, 7))
  
  final_plot <- plot_grid(
    title, full_plot,
    ncol = 1,
    rel_heights = c(0.05, 0.95)
  )
  
  return(final_plot)
}
