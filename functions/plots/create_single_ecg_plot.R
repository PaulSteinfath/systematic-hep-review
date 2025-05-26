create_single_ecg_plot <- function(df, avg_value = NULL, # takes "Averaging", "Clustering", or NULL/"Both" as input
                                   shared_limits, plot_title,
                                   reference_var = "hep_relative_to",
                                   reference_values = c("R-peak", "T-peak"),
                                   x_label_main = "Time (ms)") {

  # Filter based on avg_value using the preprocessed method_category column
  df_filtered <- df 

  if (!is.null(avg_value)) {
    if (avg_value == "Averaging") {
      df_filtered <- df %>% filter(method_category == "Averaging")
    } else if (avg_value == "Clustering") {
      df_filtered <- df %>% filter(method_category == "Clustering")
    }
    
  }
  
  # --- Create Time Window Plots ---
  title_hjust <- -0.02 

 # Create plot for HER Time of Interest
  if (nrow(df_filtered %>% filter(!is.na(hep_start), !is.na(hep_end))) > 0) {
    hep_plot <- create_time_windows_plot(df_filtered, 
      "hep_start", "hep_end", reference_var,
      x_label_main,
      reference_values = reference_values,
      t_peak_offset = t_peak_offset,
      x_limits = shared_limits) +
   
      ggtitle("HEP Time of Interest") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)))
  } else {
    hep_plot <- no_valid_data_stub("No valid data") +
      ggtitle("HEP Time of Interest") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)),
        legend.position = "none")
    }

    # Create plot for Baseline Window
    if (nrow(df_filtered %>% filter(!is.na(baseline_start_ms), !is.na(baseline_end_ms))) > 0) {
    baseline_plot <- create_time_windows_plot(df_filtered,
      "baseline_start_ms", "baseline_end_ms", reference_var,
      x_label_main,
      reference_values = reference_values,
      t_peak_offset = t_peak_offset,
      x_limits = shared_limits) +
      ggtitle("Baseline Window") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)),
        legend.position = "none")
    } else {
    baseline_plot <- no_valid_data_stub("No valid data") +
      ggtitle("Baseline Window") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)),
        legend.position = "none")
    }

    # Create plot for Significant Effects Found
    df_signif <- df_filtered %>% filter(significant_test == 1)
    if (nrow(df_signif) > 0) {
    # Create merged start/end/ref columns
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
      ggtitle("Significant Effects Found") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)))
    } else {
    significant_plot <- no_valid_data_stub("No valid data") +
      ggtitle("Significant Effects Found") +
      theme(plot.title = element_text(hjust = title_hjust, size = 11, margin = margin(b = 5)))
    }

    # --- Create ECG Plot ---
    r_peak_ms <- r_peak_offset
    t_peak_ms <- t_peak_offset
    ecg_df <- create_ecg_data(shared_limits[1],shared_limits[2], n_points = 1000)

    ecg_plot <- ggplot(ecg_df, aes(x = time, y = voltage)) +
    geom_line(color = plot_fill_default_single, linewidth = 0.5)

    # Add R/T peak lines only if that's the current contrast
    if (identical(reference_values, c("R-peak", "T-peak"))) {
    ecg_plot <- ecg_plot +
      geom_vline(aes(xintercept = r_peak_ms, color = "R-peak"), linetype = "dashed", alpha = 0.5) +
      geom_vline(aes(xintercept = t_peak_ms, color = "T-peak"), linetype = "dashed", alpha = 0.5) +
      scale_color_manual(values = r_t_peak_palette, guide = "none")
    } else {
     ecg_plot <- ecg_plot + guides(color = "none")
    }

    ecg_plot <- ecg_plot +
    labs(x = x_label_main, y = "ECG amplitude") +
    theme_classic() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(size = 8)
    ) +
    scale_x_continuous(limits = shared_limits, expand = c(0, 0))

    # --- Create Y Label Grob ---
    y_label_text <- "Number of Pipelines"
    y_axis_label_grob <- ggdraw() +
      # Adjust y position to align with the center of the baseline plot ~ 0.7 from bottom
      draw_label(y_label_text, angle = 90, fontface = 'plain', size = 10, x = 0.5, y = 0.7) +
      theme(plot.margin = margin(0, 0, 0, 0))

    # --- Modify Plots for Alignment ---
    plot_margin_top <- margin(t = 1, r = 5, b = 10, l = 5)
    plot_margin_bottom <- margin(t = 5, r = 5, b = 1, l = 5)

    hep_plot_aligned <- hep_plot +
    theme(
      legend.position.inside = c(0.98, 0.98), 
      legend.justification = c("right", "top"), 
      legend.background = element_rect(fill = alpha("white", 0.5)),
      legend.title = element_text(size = 8), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.8, "lines"),
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), 
      axis.title.y = element_blank(), 
      plot.margin = plot_margin_top
    )

    theme_aligned_middle <- theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = plot_margin_top
    )

    baseline_plot_aligned <- baseline_plot + theme_aligned_middle
    significant_plot_aligned <- significant_plot + theme_aligned_middle

    ecg_plot_aligned <- ecg_plot +
    theme(legend.position = "none",
        axis.title.y = element_text(size=10), 
        plot.margin = plot_margin_bottom) 

    combined_plots_core <- plot_grid(
    hep_plot_aligned,
    baseline_plot_aligned,
    significant_plot_aligned,
    ecg_plot_aligned,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.3, 0.3, 0.3, 0.2)
    )

 # Add the Y label to the left
     plots_with_ylabel <- plot_grid(
      y_axis_label_grob,
      combined_plots_core,
      ncol = 2,
      rel_widths = c(0.03, 0.97)
    )

    title_grob <- ggdraw() +
    draw_label(plot_title, fontface = 'bold', x = 0.02, hjust = 0) +
    theme(plot.margin = margin(t = 5, r = 0, b = 5, l = 5))

    final_plot <- plot_grid(
    title_grob, plots_with_ylabel,
    ncol = 1,
    rel_heights = c(0.05, 0.95)
    )

    return(final_plot)
}
