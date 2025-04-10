create_single_ecg_plot <- function(df, avg_value, shared_limits, plot_title) {
  # Filter data for the specific averaging_time value
  df_filtered <- df %>% filter(averaging_time == avg_value)
  
  # Create plots for HEP time windows
  if (nrow(df_filtered %>% filter(!is.na(hep_start), !is.na(hep_end))) > 0) {
    hep_plot <- create_time_windows_plot(df_filtered, 
      "hep_start", "hep_end", "hep_relative_to",
      "Time relative to R-peak (ms)",
      t_peak_offset = 300,
      x_limits = shared_limits) +
      ggtitle("A) HER Time of Interest") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  } else {
    hep_plot <- no_valid_data_stub("No valid data") +
      ggtitle("A) HER Time of Interest") +
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
    baseline_plot <- no_valid_data_stub("No valid data") +
      ggtitle("B) Baseline Window") +
      theme(plot.title = element_text(hjust = -0.02, size = 12, margin = margin(b = 10)))
  }

  # For "Significant effects" subplot we use hep_start/end if averaging == 1 otherwise use significant_start/end
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
    significant_plot <- no_valid_data_stub("No valid data") +
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
  x_min <- max(min(shared_limits), -1000)  
  x_max <- min(max(shared_limits), 2000)   
  
  ecg_df <- create_ecg_data(x_min, x_max, n_points = 1000)
  
  # ECG plot
  ecg_plot <- ggplot(ecg_df, aes(x = time, y = voltage)) +
    geom_line(color = "grey80", size = 1) +
    geom_vline(aes(xintercept = r_peak_ms, color = "R Peak"), linetype = "dashed", alpha = 0.5) +
    geom_vline(aes(xintercept = t_peak_ms, color = "T Peak"), linetype = "dashed", alpha = 0.5) +
    scale_color_manual(values = c("R Peak" = "#0072B2", "T Peak" = "#E69F00"), 
                      name = NULL) + 
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
