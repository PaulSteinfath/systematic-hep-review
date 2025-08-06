plot_rr_intervals <- function(df, group_col = "PMID", 
                              tmin = -150, tmax = 800, tstep = 200) {
  
  # Filter for distinct RR intervals
  rr_df <- df %>%
    filter(!is.na(cfa_minimal_rr)) %>%
    distinct(!!sym(group_col), cfa_minimal_rr)
  multiple_rows_per_group <- any(table(rr_df[[group_col]]) > 1)
  level = if (multiple_rows_per_group) "pipelines" else "studies"
    
  # Early return if no data
  if (nrow(rr_df) == 0) {
    return(no_valid_data_stub("No RR interval data"))
  }

  # Process data only if we have valid entries
  rr_df <- rr_df %>%
    arrange(cfa_minimal_rr) %>%
    mutate(
      rank = seq_len(n()),
      rank_scaled = 0.1 + (rank / n()) * 1.5
    )
  
  # Create ECG data 
  ecg_data <- create_ecg_data(tmin, tmax)
  
  ggplot() +
    geom_line(data = ecg_data, aes(x = time, y = voltage),
              color = common_colors$simulated_ecg, 
              size = 1, 
              alpha = common_colors$simulated_ecg_alpha) +
    geom_segment(
      data = rr_df,
      aes(x = 0, xend = cfa_minimal_rr, y = rank_scaled, yend = rank_scaled),
      color = common_colors$fill_default, 
      linewidth = 0.7
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = common_colors$r_peak_vline) +
    labs(x = "Time (ms)",
         y = paste("Individual", level),
         title = "Minimal RR interval",
         subtitle = paste("n =", nrow(rr_df), level),
        ) +
    scale_x_continuous(breaks = unique(sort(c(seq(0, tmax, by = tstep), tmax)))) +
    theme_classic(base_family = "sans") +
    plot_theme_default +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          title = element_text(hjust = 0, size = 9), 
          plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10)),
          axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
}
