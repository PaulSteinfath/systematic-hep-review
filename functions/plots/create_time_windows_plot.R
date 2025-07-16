create_time_windows_plot <- function(df, start_var, end_var, reference_var, x_label,
                   reference_values = c("R-peak", "T-peak"),
                   t_peak_offset = 300, x_limits = NULL) {

  # Define colors for the two reference values
  ref_palette <- setNames(r_t_peak_palette[1:2], reference_values)

  # Prepare data based on whether it's for significant effects or other windows
  if (start_var != "merged_start") {
  # Convert and filter for time_of_interest or baseline windows
  df_converted <- df %>%
    filter(!.data[[start_var]] %in% c("none", "unknown"),
       !.data[[end_var]] %in% c("none", "unknown")) %>%
    mutate(
    start_time = as.numeric(.data[[start_var]]),
    end_time   = as.numeric(.data[[end_var]])
    ) %>%
    filter(!is.na(start_time), !is.na(end_time)) %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .data[[reference_var]], .keep_all = TRUE)

  df_group1 <- df_converted %>% filter(.data[[reference_var]] == reference_values[1])
  df_group2 <- df_converted %>% filter(.data[[reference_var]] == reference_values[2]) %>%
    mutate(
    start_time = if(reference_values[2] == "T-peak") start_time + t_peak_offset else start_time,
    end_time   = if(reference_values[2] == "T-peak") end_time + t_peak_offset else end_time
    )
  } else {
    # Convert and filter for significant effects windows
    df_converted <- df %>%
      filter(.data[[reference_var]] %in% reference_values, significant_test == 1) %>%
      mutate(
      hep_start = as.numeric(hep_start),
      hep_end   = as.numeric(hep_end),
      significant_start_ms = as.numeric(significant_start_ms),
      significant_end_ms   = as.numeric(significant_end_ms),
      start_time = if_else(averaging_time == "1", hep_start, significant_start_ms),
      end_time   = if_else(averaging_time == "1", hep_end, significant_end_ms)
      )

  df_group1 <- df_converted %>%
    filter(.data[[reference_var]] == reference_values[1]) %>%
    filter(!is.na(start_time), !is.na(end_time))

  df_group2 <- df_converted %>%
    filter(.data[[reference_var]] == reference_values[2]) %>%
    mutate(
    start_time = if(reference_values[2] == "T-peak") start_time + t_peak_offset else start_time,
    end_time   = if(reference_values[2] == "T-peak") end_time + t_peak_offset else end_time
    ) %>%
    filter(!is.na(start_time), !is.na(end_time))
  }

  # Determine overall time range from data
  all_times <- c(
    if(nrow(df_group1) > 0) c(df_group1$start_time, df_group1$end_time) else numeric(0),
    if(nrow(df_group2) > 0) c(df_group2$start_time, df_group2$end_time) else numeric(0)
  )

  # Handle case with no valid time data
  if (length(all_times) == 0 || all(is.na(all_times))) {
    return(no_valid_data_stub("No valid time window data"))
  }

  x_min <- min(all_times, na.rm = TRUE)
  x_max <- max(all_times, na.rm = TRUE)

  # Define time vector for calculating counts
  x_time <- seq(floor(x_min), ceiling(x_max), by = 1)

  # Calculate counts for both reference groups
  counts1 <- calculate_cumulative_counts(df_group1, x_time)
  counts2 <- calculate_cumulative_counts(df_group2, x_time)

  # Combine into a single dataframe for plotting
  plot_data <- data.frame(
  Time = rep(x_time, 2),
  Count = c(counts1, counts2),
  Reference = factor(rep(reference_values, each = length(x_time)), levels = reference_values)
  )

  # Create the cumulative plot
  combined_plot <- ggplot(plot_data, aes(x = Time, y = Count, fill = Reference, color = Reference)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_vline(xintercept = 0, color = ref_palette[1], alpha = 0.7, linetype = "dashed") +
  geom_vline(xintercept = if(reference_values[2] == "T-peak") t_peak_offset else 0,
         color = ref_palette[2], alpha = 0.7, linetype = "dashed") +
  scale_color_manual(
    values = ref_palette,
    breaks = reference_values
  ) +
  scale_fill_manual(
    values = ref_palette,
    breaks = reference_values
  ) +
  labs(x = x_label, y = "Number of Pipelines") + 
  scale_y_continuous() +
  plot_theme_default +
  custom_theme() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10)
  ) +
  coord_cartesian(xlim = x_limits, expand = FALSE)

  return(combined_plot)
}
