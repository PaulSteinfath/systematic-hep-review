minimal_artifact_windows_plot <- function(df) {
  t_peak_offset <- 300
  
  df_minimal <- df %>%
    filter(str_detect(tolower(other_cfa_removal_strategy), "limit analysis to time of minimal artifact")) %>%
    select(PMID, hep_start, hep_end, hep_relative_to) %>%
    distinct(PMID, hep_start, hep_end, hep_relative_to) %>%
    mutate(
      hep_start = case_when(
        tolower(hep_relative_to) == "t-peak" ~ hep_start + t_peak_offset,
        TRUE ~ hep_start
      ),
      hep_end = case_when(
        tolower(hep_relative_to) == "t-peak" ~ hep_end + t_peak_offset,
        TRUE ~ hep_end
      ),
      reference = case_when(
        tolower(hep_relative_to) == "t-peak" ~ "T-peak",
        TRUE ~ "R-peak"
      )
    ) %>%
    mutate(reference = factor(reference, levels = c("T-peak", "R-peak"))) %>%
    arrange(reference, hep_start)
  
  df_minimal <- df_minimal %>%
    group_by(reference) %>%
    mutate(rank_in_group = row_number()) %>%
    ungroup()
  
  group_offsets <- df_minimal %>%
    group_by(reference) %>%
    summarise(group_count = n(), .groups = "drop") %>%
    arrange(reference) %>%
    mutate(offset = lag(cumsum(group_count), default = 0) + (row_number()-1)*0.5)
  
  df_minimal <- df_minimal %>%
    left_join(group_offsets, by = "reference") %>%
    mutate(new_rank = rank_in_group + offset) %>%
    mutate(rank_scaled = 0.1 + (new_rank / max(new_rank)) * 1.5)
  
  x_min <- min(df_minimal$hep_start, -150)
  x_max <- max(df_minimal$hep_end)
  
  break_points <- seq(floor(x_min/100)*100, ceiling(x_max/100)*100, by = 250)
  
  ecg_data <- data.frame(time = seq(x_min, x_max, length.out = 500))
  ecg_data$voltage <- create_ecg_wave(ecg_data$time)
  ecg_data$voltage <- ecg_data$voltage - mean(ecg_data$voltage)
  
  ggplot() +
    geom_line(data = ecg_data, aes(x = time, y = voltage),
              color = "#696969", size = 1, alpha = 0.3) +
    geom_segment(
      data = df_minimal,
      aes(x = hep_start, xend = hep_end, 
          y = rank_scaled, yend = rank_scaled,
          color = reference),
      linewidth = 0.7
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
    geom_vline(xintercept = 300, linetype = "dashed", color = "#E69F00") +
    labs(
      x = "Time (ms)",
      y = "",
      title = paste("n =", nrow(df_minimal)),
      caption = "HEP windows for minimal artifact",
      color = "Reference"
    ) +
    scale_color_manual(values = c("R-peak" = "black", "T-peak" = "#E69F00")) +
    theme_classic(base_family = "sans") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90", linetype = "solid"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0, size = 9),
          plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10)),
          legend.position = "top",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-5, 0, 0, 0))
}
