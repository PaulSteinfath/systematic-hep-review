create_time_windows_plot <- function(df, start_var, end_var, reference_var, x_label,
                                     t_peak_offset = 300, x_limits = NULL) { 

  # Prepare data based on whether it's for significant effects or other windows
  if (start_var != "merged_start") {
    # Convert and filter for time_of_interest or baseline windows
    df_converted <- df %>%
      filter(!.data[[start_var]] %in% c("none", "unknown"), 
             !.data[[end_var]] %in% c("none", "unknown")) %>%
      mutate(
        start_time = as.numeric(as.character(.data[[start_var]])),
        end_time   = as.numeric(as.character(.data[[end_var]]))
      ) %>%
      filter(!is.na(start_time), !is.na(end_time)) %>%
      distinct(PMID, .data[[start_var]], .data[[end_var]], .data[[reference_var]], .keep_all = TRUE)

    df_rpeak <- df_converted %>% filter(.data[[reference_var]] == "R-peak")
    df_tpeak <- df_converted %>% filter(.data[[reference_var]] == "T-peak") %>%
      mutate(
        start_time = start_time + t_peak_offset,
        end_time   = end_time + t_peak_offset
      )
  } else {
    # Convert and filter for significant effects windows
    df_converted <- df %>%
      filter(.data[[reference_var]] %in% c("R-peak","T-peak"), significant_test == 1) %>%
      mutate(
        hep_start = as.numeric(as.character(hep_start)),
        hep_end   = as.numeric(as.character(hep_end)),
        significant_start_ms = as.numeric(as.character(significant_start_ms)),
        significant_end_ms   = as.numeric(as.character(significant_end_ms)),
        start_time = if_else(averaging_time == "1", hep_start, significant_start_ms),
        end_time   = if_else(averaging_time == "1", hep_end, significant_end_ms)
      ) 

    df_rpeak <- df_converted %>%
      filter(.data[[reference_var]] == "R-peak")%>%
      filter(!is.na(start_time), !is.na(end_time))

    df_tpeak <- df_converted %>%
      filter(.data[[reference_var]] == "T-peak") %>%
      mutate(
        start_time = start_time + t_peak_offset,
        end_time   = end_time + t_peak_offset
      ) %>%
      filter(!is.na(start_time), !is.na(end_time))
  }

  # Calculate cumulative counts over time intervals
  calculate_cumulative_counts <- function(df_subset, time_vec) { 
    if (nrow(df_subset) == 0) {
      return(rep(0, length(time_vec)))
    }
    counts <- numeric(length(time_vec))
    # Small offset to ensure intervals are checked correctly [start, end)
    precision_offset <- (time_vec[2] - time_vec[1]) / 2 

    for (i in 1:nrow(df_subset)) {
      start_i <- df_subset$start_time[i]
      end_i <- df_subset$end_time[i]
      indices <- which(time_vec >= (start_i - precision_offset) & time_vec < (end_i - precision_offset))
      if (length(indices) > 0) {
        counts[indices] <- counts[indices] + 1
      }
    }
    return(counts) 
  }

  # Determine overall time range from data
  all_times <- c(
      if(nrow(df_rpeak) > 0) c(df_rpeak$start_time, df_rpeak$end_time) else numeric(0),
      if(nrow(df_tpeak) > 0) c(df_tpeak$start_time, df_tpeak$end_time) else numeric(0)
  )

  # Handle case with no valid time data
  if (length(all_times) == 0 || all(is.na(all_times))) {
      return(no_valid_data_stub("No valid time window data"))
  }

  x_min <- min(all_times, na.rm = TRUE)
  x_max <- max(all_times, na.rm = TRUE)

  # Define time vector for calculating counts
   x_time <- seq(floor(x_min), ceiling(x_max), by = 1)

  # Calculate counts for R-peak and T-peak references
  r_counts <- calculate_cumulative_counts(df_rpeak, x_time) 
  t_counts <- calculate_cumulative_counts(df_tpeak, x_time) 

  # Combine into a single dataframe for plotting
  plot_data <- data.frame(
    Time = rep(x_time, 2),
    Count = c(r_counts, t_counts), 
    Reference = factor(rep(c("R-peak", "T-peak"), each = length(x_time)), levels = c("R-peak", "T-peak"))
  )

  # Split data for separate geom_area layers
  plot_data_r_full <- plot_data %>% filter(Reference == "R-peak")
  plot_data_t_full <- plot_data %>% filter(Reference == "T-peak")

  # Create the cumulative plot using geom_area
  combined_plot <- ggplot() + 
    geom_area(data = plot_data_r_full, aes(x = Time, y = Count, fill = "R-peak", color = "R-peak"), alpha = 0.3, position = "identity") +
    geom_area(data = plot_data_t_full, aes(x = Time, y = Count, fill = "T-peak", color = "T-peak"), alpha = 0.3, position = "identity") +
    geom_vline(xintercept = 0, color = r_t_peak_palette["R-peak"], alpha = 0.7, linetype = "dashed") +
    geom_vline(xintercept = t_peak_offset, color = r_t_peak_palette["T-peak"], alpha = 0.7, linetype = "dashed") +
    scale_color_manual(
        name = "Reference", 
        values = r_t_peak_palette, 
        breaks = c("R-peak", "T-peak"), 
        labels = c("R-peak", "T-peak")
    ) +
    scale_fill_manual( 
        name = "Reference", 
        values = r_t_peak_palette, 
        breaks = c("R-peak", "T-peak"), 
        labels = c("R-peak", "T-peak")
    ) +
    labs(x = x_label, y = "Number of Pipelines") + 
    scale_y_continuous() +
    theme_classic() +
    theme(
      legend.position = "none", # Hide legend here via theme for external placement/alignment
      axis.text.y = element_text(size = 8),
      axis.title.y = element_blank(), # Hide y-title here and add back later as grob
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10)
    ) +
    coord_cartesian(xlim = x_limits, expand = FALSE) 

  return(combined_plot)
}
