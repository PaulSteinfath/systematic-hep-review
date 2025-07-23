rr_intervals_plot <- function(df) {
  
  rr_df <- df %>%
    distinct(PMID, other_cfa_removal_strategy) %>%
    mutate(rr_match = str_match(tolower(other_cfa_removal_strategy), "rr at least\\s*(\\d+)\\s*ms")) %>%
    filter(!is.na(rr_match[,2])) %>%
    mutate(rr_value = as.numeric(rr_match[,2])) %>%
    filter(!is.na(rr_value), is.finite(rr_value))
    
  # Early return if no data
  if (nrow(rr_df) == 0) {
    return(no_valid_data_stub("No RR interval data"))
  }

  # Process data only if we have valid entries
  rr_df <- rr_df %>%
    arrange(rr_value) %>%
    mutate(
      rank = seq_len(n()),
      rank_scaled = 0.1 + (rank / n()) * 1.5
    )
  
  # Define plot limits 
  x_min <- -150
  x_max <- 750
  
  # Create ECG data 
  ecg_data <- create_ecg_data(x_min, x_max)
  
  ggplot() +
    geom_line(data = ecg_data, aes(x = time, y = voltage),
              color = plot_fill_default_single, size = 1, alpha = 0.3) +
    geom_segment(
      data = rr_df,
      aes(x = 0, xend = rr_value, y = rank_scaled, yend = rank_scaled),
      color = plot_fill_default_single, linewidth = 0.7
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
    labs(x = "Time (ms)",
         y = "",
         title = "Minimum RR Interval",
         subtitle = paste("n =", nrow(rr_df), "studies"),
        ) +
    scale_x_continuous(breaks = unique(sort(c(seq(0, x_max, by = 250), x_max)))) +
    plot_theme_default +
    custom_theme() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          #panel.grid.major.x = element_line(color = "gray90", linetype = "solid"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          title = element_text(hjust = 0, size = 9), 
          plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10)),
          axis.title.x = element_text(hjust = 0.5))
}
