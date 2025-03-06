rr_intervals_plot <- function(df) {
  
  rr_df <- df %>%
    distinct(PMID, other_cfa_removal_strategy, .keep_all = TRUE) %>%
    mutate(rr_match = str_match(tolower(other_cfa_removal_strategy), "rr at least\\s*(\\d+)\\s*ms")) %>%
    filter(!is.na(rr_match[,2])) %>%
    mutate(rr_value = as.numeric(rr_match[,2])) %>%
    arrange(rr_value) %>%
    mutate(rank = row_number()) %>%
    mutate(rank_scaled = 0.1 + (rank / max(rank)) * 1.5)
  
  x_min <- -150
  x_max <- max(rr_df$rr_value) 
  n_rows <- nrow(rr_df)
  
  ecg_data <- data.frame(time = seq(x_min, x_max, length.out = 500))
  ecg_data$voltage <- create_ecg_wave(ecg_data$time)
  ecg_data$voltage <- ecg_data$voltage - mean(ecg_data$voltage)
  
  ggplot() +
    geom_line(data = ecg_data, aes(x = time, y = voltage),
              color = "#696969", size = 1, alpha = 0.3) +
    geom_segment(
      data = rr_df,
      aes(x = 0, xend = rr_value, y = rank_scaled, yend = rank_scaled),
      color = "black", linewidth = 0.7
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
    labs(x = "Time (ms)",
         y = "",
         title = paste("n =", n_rows),
         caption = "Minimum RR Intervals") +
    scale_x_continuous(breaks = unique(sort(c(seq(0, x_max, by = 250), x_max)))) +
    theme_classic(base_family = "sans") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90", linetype = "solid"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0, size = 9),
          plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10)))
}
