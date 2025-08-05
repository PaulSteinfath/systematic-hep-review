create_single_ecg_plot <- function(df, avg_value = NULL, # takes "Averaging", "Clustering", or NULL/"Both" as input
                                   shared_limits, plot_title,
                                   reference_var = "hep_relative_to",
                                   reference_values = c("R-peak", "T-peak"),
                                   by = "pipeline",
                                   x_label_main = "Time (ms)",
                                   debug_inset = FALSE) {

  # Alignment
  plot_margin_top <- margin(t = 1, r = 5, b = 10, l = 5)
  plot_margin_bottom <- margin(t = 5, r = 5, b = 1, l = 5)
  theme_aligned_middle <- theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = plot_margin_top
  )
  
  # Filter based on avg_value using the preprocessed method_category column
  df_filtered <- df
  if (!is.null(avg_value)) {
    if (avg_value == "Averaging") {
      df_filtered <- df %>% filter(method_category == "Averaging")
    } else if (avg_value == "Clustering") {
      df_filtered <- df %>% filter(method_category == "Clustering")
    }
  }

  # Create plot for HER Time of Interest
  hep_windows_defined <- df_filtered %>% 
    filter(!is.na(hep_start), !is.na(hep_end))
  if (nrow(hep_windows_defined) > 0) {
    hep_plot <- create_time_windows_plot(df_filtered, 
        "hep_start", "hep_end", reference_var,
        x_label_main,
        "Analyzed window",
        reference_values = reference_values,
        by = by,
        t_peak_offset = t_peak_offset,
        x_limits = shared_limits
      ) + 
      plot_theme_default + 
      theme_aligned_middle + 
      theme(
        plot.title = element_text(margin = margin(b = 5)),
        legend.position = "none"
      )

    selected_channels <- plot_eeg_locations(df_filtered, 
                                            "hep_channels_selected", 
                                            by = "study",
                                            divide.over = NULL,
                                            combined = T,
                                            colormap = "Greys",
                                            stretch_palette = 1.25,
                                            main_title = NULL, 
                                            show_colorbar = T)
  } else {
    hep_plot <- no_valid_data_stub("No valid data") + 
      ggtitle("Analyzed window") +
      theme(plot.title = element_text(margin = margin(b = 5)),
            legend.position = "none")
    
    selected_channels <- NULL
  }

  # Create plot for Baseline Window
  if (nrow(df_filtered %>% filter(!is.na(baseline_start_ms), !is.na(baseline_end_ms))) > 0) {
    baseline_plot <- create_time_windows_plot(df_filtered,
      "baseline_start_ms", "baseline_end_ms", reference_var,
      x_label_main,
      "Baseline window",
      reference_values = reference_values,
      by = by,
      t_peak_offset = t_peak_offset,
      x_limits = shared_limits) +
      theme_aligned_middle + 
      theme(plot.title = element_text(margin = margin(b = 5)))
  } else {
    baseline_plot <- no_valid_data_stub("No valid data") +
      ggtitle("Baseline window") +
      theme(plot.title = element_text(margin = margin(b = 5)),
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
      "Significant effects",
      by = by,
      t_peak_offset = 300,
      x_limits = shared_limits
    ) +
      plot_theme_default + 
      theme_aligned_middle + 
      theme(plot.title = element_text(margin = margin(b = 5)))
    
    significant_channels <- plot_eeg_locations(df_filtered, 
                                               "significant_channels", 
                                               by = "study",
                                               divide.over = NULL,
                                               combined = T,
                                               colormap = "Greys",
                                               stretch_palette = 1.25,
                                               main_title = NULL, 
                                               show_colorbar = T)
  } else {
    significant_plot <- no_valid_data_stub("No valid data") +
      ggtitle("Significant effects") +
      theme(plot.title = element_text(size = 9, margin = margin(b = 5)))
    
    significant_channels <- NULL
  }

  # --- Create ECG Plot ---
  r_peak_ms <- r_peak_offset
  t_peak_ms <- t_peak_offset
  ecg_df <- create_ecg_data(shared_limits[1], shared_limits[2], n_points = 1000)

  ecg_plot <- ggplot(ecg_df, aes(x = time, y = voltage)) +
    geom_line(color = common_colors$fill_default, 
              linewidth = 0.5)

  # Add R/T peak lines only if that's the current contrast
  if (identical(reference_values, c("R-peak", "T-peak"))) {
    ecg_plot <- ecg_plot +
      annotate("text", x = 990, y = 0.3, label = "Simulated ECG", hjust = 1, size = 3) + 
      geom_vline(aes(xintercept = r_peak_ms, color = "R-peak"), linetype = "dashed", alpha = 0.5) +
      geom_vline(aes(xintercept = t_peak_ms, color = "T-peak"), linetype = "dashed", alpha = 0.5) +
      scale_color_manual(values = r_t_peak_palette, guide = "none")
  } else {
    ecg_plot <- ecg_plot + guides(color = "none")
  }

  ecg_plot <- ecg_plot +
    labs(x = x_label_main, y = "Amplitude") +
    plot_theme_default +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.margin = plot_margin_bottom
    ) +
    scale_x_continuous(limits = shared_limits, 
                       breaks = seq(-200, 800, by = 200),
                       expand = c(0, 0))

  # --- Create Y Label Grob ---
  y_label_text <- paste("Number of", if (by == "pipeline") "Pipelines" else "studies")
  y_axis_label_grob <- ggdraw() +
    # Adjust y position to align with the center of the baseline plot ~ 0.59 from bottom
    draw_label(y_label_text, angle = 90, fontface = 'plain', size = 8, x = 0.5, y = 0.59) +
    theme(plot.margin = margin(0, 0, 0, 0))

  # --- Modify Plots for Alignment ---

  baseline_plot_aligned <- baseline_plot +
  plot_theme_default +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.98, 1.5), 
      legend.justification = c("right", "top"), 
      legend.background = element_rect(fill = alpha("white", 0.5)),
      legend.key.size = unit(0.8, "lines"),
      plot.title = element_text(margin = margin(b = 5)),
      plot.subtitle = element_text(margin = margin(b = 5)),
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), 
      axis.title.y = element_blank(), 
      plot.margin = plot_margin_top
    )

  hep_plot_aligned <- hep_plot + theme_aligned_middle
  significant_plot_aligned <- significant_plot + theme_aligned_middle

  combined_plots_core <- plot_grid(
    baseline_plot_aligned,
    hep_plot_aligned,
    significant_plot_aligned,
    ecg_plot,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.25, 0.35, 0.35, 0.2)
  )

  # Add the Y label to the left
  plots_with_ylabel <- plot_grid(
    y_axis_label_grob,
    combined_plots_core,
    ncol = 2,
    rel_widths = c(0.0625, 0.9375)
  )

  title_grob <- ggdraw() +
    draw_label(plot_title, fontface = 'bold', x = 0.53, y = 0.5, hjust = 0.5, size = 11) +
    theme(plot.margin = margin(t = 5, r = 0, b = 10, l = 5))

  final_plot <- plot_grid(
    title_grob, plots_with_ylabel,
    ncol = 1,
    rel_heights = c(0.05, 0.95)
  ) 
  
  # Add channel location plots as insets
  # NOTE: this is performed at the very end since ggdraw screws up alignment
  final_plot <- ggdraw(final_plot)
  
  x_selected <- 0.79
  y_selected <- 0.6
  w_selected <- 0.2
  h_selected <- 0.15
  
  x_selected_legend <- 0.83
  y_selected_legend <- 0.57
  w_selected_legend <- 0.12
  h_selected_legend <- 0.035
  
  x_significant <- 0.79
  y_significant <- 0.3
  w_significant <- 0.2
  h_significant <- 0.15
  
  x_significant_legend <- 0.83
  y_significant_legend <- 0.27
  w_significant_legend <- 0.12
  h_significant_legend <- 0.035
  
  adjust_legend <- theme(legend.position = "bottom",
                         legend.key.width = unit(0.5, "lines"),
                         legend.key.height = unit(0.3, "lines"),
                         legend.ticks = element_blank(),
                         legend.title = element_blank())

  if (!is.null(selected_channels)) {
    selected_legend <- get_plot_component(selected_channels + adjust_legend,
                                          "guide-box", return_all = T)[[3]]
    
    final_plot <- final_plot + 
      draw_plot(selected_channels + theme(legend.position = "none"),
                x = x_selected, y = y_selected, 
                width = w_selected, height = h_selected) + 
      draw_plot(selected_legend,
                x = x_selected_legend, y = y_selected_legend, 
                width = w_selected_legend, height = h_selected_legend)
    
    if (debug_inset) {
      final_plot <- final_plot + 
        draw_line(x = c(x_selected, x_selected + w_selected),
                  y = c(y_selected, y_selected + h_selected)) +
        draw_line(x = c(x_selected_legend, x_selected_legend + w_selected_legend),
                  y = c(y_selected_legend, y_selected_legend + h_selected_legend))
    }
  }
  if (!is.null(significant_channels)) {
    significant_legend <- get_plot_component(significant_channels + adjust_legend,
                                             "guide-box", return_all = T)[[3]]
    
    final_plot <- final_plot + 
      draw_plot(significant_channels + theme(legend.position = "none"),
                x = x_significant, y = y_significant, 
                width = w_significant, height = h_significant) + 
      draw_plot(significant_legend,
                x = x_significant_legend, y = y_significant_legend, 
                width = w_significant_legend, height = h_significant_legend)
    
    if (debug_inset) {
      final_plot <- final_plot + 
        draw_line(x = c(x_significant, x_significant + w_significant),
                  y = c(y_significant, y_significant + h_significant)) +
        draw_line(x = c(x_significant_legend, x_significant_legend + w_significant_legend),
                  y = c(y_significant_legend, y_significant_legend + h_significant_legend))
    }
  }

  return(final_plot)
}
