create_time_windows_plot <- function(df, start_var, end_var, reference_var, x_label,
                                     t_peak_offset = 300, x_limits = NULL,
                                     fill_color = "#696969", legend_label = "Count") {

  # For time of interest and baseline plots:
  if (start_var != "merged_start") {
    # Convert and filter
    df_converted <- df %>%
      filter(!is.na(.data[[start_var]]), !is.na(.data[[end_var]])) %>%
      filter(!.data[[start_var]] %in% c("none", "unknown"), !.data[[end_var]] %in% c("none", "unknown")) %>%
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
    # Convert and filter
    df_converted <- df %>%
      filter(.data[[reference_var]] %in% c("R-peak","T-peak"), significant_test == 1) %>%
      mutate(
        hep_start = as.numeric(as.character(hep_start)),
        hep_end   = as.numeric(as.character(hep_end)),
        significant_start_ms = as.numeric(as.character(significant_start_ms)),
        significant_end_ms   = as.numeric(as.character(significant_end_ms))
      )

    df_rpeak <- df_converted %>%
      filter(.data[[reference_var]] == "R-peak") %>%
      mutate(
        start_time = if_else(averaging_time == "1", hep_start, significant_start_ms),
        end_time   = if_else(averaging_time == "1", hep_end, significant_end_ms)
      ) %>%
      filter(!is.na(start_time), !is.na(end_time))

    df_tpeak <- df_converted %>%
      filter(.data[[reference_var]] == "T-peak") %>%
      mutate(
        start_time = if_else(averaging_time == "1", hep_start, significant_start_ms) + t_peak_offset,
        end_time   = if_else(averaging_time == "1", hep_end, significant_end_ms) + t_peak_offset
      ) %>%
      filter(!is.na(start_time), !is.na(end_time))
  }

  create_density_plot <- function(df_filtered, title = "", x_limits = NULL, remove_axes = FALSE) {
    if (nrow(df_filtered) == 0) {
      return(ggplot() +
        theme_void() +
        annotate("text", x = 0, y = 0, label = "No valid data"))
    }
    
    n_bins <- 160
    time_range <- seq(min(df_filtered$start_time),
      max(df_filtered$end_time),
      length.out = n_bins
    )
    density_matrix <- matrix(0, nrow = nrow(df_filtered), ncol = length(time_range))
    for (i in 1:nrow(df_filtered)) {
      time_indices <- which(time_range >= df_filtered$start_time[i] & time_range <= df_filtered$end_time[i])
      density_matrix[i, time_indices] <- 1
    }
    density_df <- data.frame(
      Time = time_range,
      Density = colSums(density_matrix)
    )

    high_fill <- fill_color
    my_legend <- legend_label
    if (title == "R-peak referenced") {
      high_fill <- "#0072B2" 
      low_fill <- "#E6F3FF"
      my_legend <- "R Peak"
    } else if (title == "T-peak referenced") {
      high_fill <- "#E69F00" 
      low_fill <- "#FFF5E6" 
      my_legend <- "T Peak"
    }

    p <- ggplot() +
      geom_raster(data = density_df, aes(x = Time, y = 1, fill = Density)) +
      scale_fill_gradient(
        low = low_fill,
        high = high_fill,
        name = my_legend,
        guide = guide_colorbar(title.position = "top")
      ) +
      labs(x = x_label, y = "") +
      geom_vline(xintercept = 0, color = "#0072B2", alpha = 0.5, linetype = "dashed") +
      geom_vline(xintercept = t_peak_offset, color = "#E69F00", alpha = 0.5, linetype = "dashed") +
      {
        if (!is.null(x_limits)) coord_cartesian(xlim = x_limits)
      } +
      scale_x_continuous(expand = c(0, 0))

    if (remove_axes) {
      p <- p + theme_void() + theme(legend.position = "none")
    } else {
      p <- p + theme_classic() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.8, "lines")
        )
    }
    return(p)
  }

  # Combine R-peak and T-peak plots
  p_rpeak <- create_density_plot(
    df_rpeak,
    "R-peak referenced",
    x_limits, remove_axes = TRUE
  )
  
  p_tpeak <- create_density_plot(
    df_tpeak,
    "T-peak referenced",
    x_limits, remove_axes = TRUE
  )
  
  combined_plot <- plot_grid(p_rpeak, p_tpeak, ncol = 1, align = "v")
  
  return(combined_plot)
}
