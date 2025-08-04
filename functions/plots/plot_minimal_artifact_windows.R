plot_minimal_artifact_windows <- function(df, t_peak_offset = 300) {

  df_minimal <- df %>%
    distinct(PMID, other_cfa_removal_strategy, .keep_all = TRUE) %>%
    filter(cfa_use_minimal_artifact_window) %>%
    select(PMID, hep_start, hep_end, hep_relative_to) %>%
    distinct()
  
  if (nrow(df_minimal) == 0) {
    return(no_valid_data_stub("No minimal artifact window data"))
  }
    
  df_minimal <- df_minimal %>%
     mutate(
      # Create T-peak indicator
      is_tpeak = tolower(hep_relative_to) == "t-peak",
      # Shift T-peak
      hep_start = hep_start + if_else(is_tpeak, t_peak_offset, 0),
      hep_end = hep_end + if_else(is_tpeak, t_peak_offset, 0),
      reference = if_else(is_tpeak, "T-peak", "R-peak"),
      reference = factor(reference, levels = c("T-peak", "R-peak"))
    )
  
  df_minimal <- df_minimal %>%
    arrange(reference, hep_start) %>%
    group_by(reference) %>%
    mutate(rank_in_group = row_number()) %>%
    ungroup()

  # offset between lines
  group_offsets <- df_minimal %>%
    group_by(reference) %>%
    summarise(group_count = n(), .groups = "drop") %>%
    arrange(reference) %>%
    mutate(offset = lag(cumsum(group_count), default = 0) + (row_number()-1)*0.5)

  df_minimal <- df_minimal %>%
    left_join(group_offsets, by = "reference") %>%
    mutate(rank_scaled = 0.1 + ((rank_in_group + offset) / max(rank_in_group + offset)) * 1.5)
  
  # Get plot limits and create ECG data
  x_min <- min(df_minimal$hep_start, -150)
  x_max <- max(df_minimal$hep_end)
  
  ecg_data <- create_ecg_data(x_min, x_max)

  ggplot() +
    geom_line(data = ecg_data, aes(x = time, y = voltage),
              color = common_colors$fill_default, linewidth = 1, alpha = 0.3) +
    geom_segment(
      data = df_minimal,
      aes(x = hep_start, xend = hep_end, 
          y = rank_scaled, yend = rank_scaled,
          color = reference),
      linewidth = 0.7
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = common_colors$r_peak_vline) +
    geom_vline(xintercept = t_peak_offset, linetype = "dashed", color = r_t_peak_palette["T-peak"]) +
    labs(
      x = "Time (ms)",
      y = "Individual studies",
      title = "Minimal artifact window",
      subtitle = paste("n =", nrow(df_minimal), "studies"),
      color = "Reference"
    ) +
    scale_color_manual(name = "Reference event",
                       values = r_t_peak_palette) +
    plot_theme_default +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          title = element_text(hjust = 0, size = 9), 
          plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10)),
          legend.position = "top",
          legend.title = element_text(size = 8),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-5, 0, 0, 0),
          axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
}
