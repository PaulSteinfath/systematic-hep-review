create_time_windows_plot <- function(df, start_var, end_var, reference_var, x_label, t_peak_offset = 300, x_limits = NULL) {
  # Create synthetic ECG wave data
  create_ecg_wave <- function(t) {
    # Simplified ECG wave using gaussian and lorentzian functions
    # R peak at t=0
    r_wave <- 2 * exp(-(t/10)^2)  # Sharp R peak
    q_wave <- -0.2 * exp(-(t+20)^2/100)  # Q wave
    s_wave <- -0.3 * exp(-(t-20)^2/100)  # S wave
    p_wave <- 0.3 * exp(-(t+100)^2/400)  # P wave
    t_wave <- 0.4 * exp(-(t-300)^2/1000)  # T wave at t=300ms
    
    return(p_wave + q_wave + r_wave + s_wave + t_wave)
  }

  # Split data by reference point and process separately
  df_rpeak <- df %>%
    filter(.data[[reference_var]] == "R-peak") %>%
    filter(!is.na(.data[[start_var]]) & !is.na(.data[[end_var]])) %>%
    filter(!.data[[start_var]] %in% c("none", "unknown") & 
           !.data[[end_var]] %in% c("none", "unknown")) %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .keep_all = TRUE) %>%
    mutate(
      start_time = suppressWarnings(as.numeric(as.character(.data[[start_var]]))),
      end_time = suppressWarnings(as.numeric(as.character(.data[[end_var]])))
    ) %>%
    filter(!is.na(start_time) & !is.na(end_time) & 
           is.finite(start_time) & is.finite(end_time))

  df_tpeak <- df %>%
    filter(.data[[reference_var]] == "T-peak") %>%
    filter(!is.na(.data[[start_var]]) & !is.na(.data[[end_var]])) %>%
    filter(!.data[[start_var]] %in% c("none", "unknown") & 
           !.data[[end_var]] %in% c("none", "unknown")) %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .keep_all = TRUE) %>%
    mutate(
      # Add offset to T-peak times to align with R-peak timeline
      start_time = suppressWarnings(as.numeric(as.character(.data[[start_var]]))) + t_peak_offset,
      end_time = suppressWarnings(as.numeric(as.character(.data[[end_var]]))) + t_peak_offset
    ) %>%
    filter(!is.na(start_time) & !is.na(end_time) & 
           is.finite(start_time) & is.finite(end_time))

  create_density_plot <- function(df_filtered, title = "", x_limits = NULL) {
    if (nrow(df_filtered) == 0) {
      return(ggplot() + 
             theme_void() + 
             annotate("text", x = 0, y = 0, label = "No valid data"))
    }

    # Generate ECG wave data
    if (!is.null(x_limits)) {
      t <- seq(x_limits[1], x_limits[2], length.out = 1000)
    } else {
      t <- seq(-200, 800, length.out = 1000)
    }
    ecg <- create_ecg_wave(t)
    ecg_df <- data.frame(Time = t, ECG = ecg)

    # Calculate density
    n_bins <- 160
    time_range <- seq(min(df_filtered$start_time, na.rm = TRUE),
                     max(df_filtered$end_time, na.rm = TRUE),
                     length.out = n_bins)
    
    density_matrix <- matrix(0, nrow = nrow(df_filtered), ncol = length(time_range))
    for (i in 1:nrow(df_filtered)) {
      time_indices <- which(time_range >= df_filtered$start_time[i] & 
                           time_range <= df_filtered$end_time[i])
      density_matrix[i, time_indices] <- 1
    }
    
    density_df <- data.frame(
      Time = time_range,
      Density = colSums(density_matrix)
    )

    # Create density plot with ECG background
    p <- ggplot() +
      # Add ECG wave first as background
      geom_line(data = ecg_df, aes(x = Time, y = ECG * 0.15 + 0.5), 
                color = "grey80", size = 0.5) +
      # Add density plot on top
      geom_raster(data = density_df, aes(x = Time, y = 1, fill = Density)) +
      scale_fill_gradient(
        low = "white", 
        high = "#696969",
        limits = c(0, max(density_df$Density))
      ) +
      # Make fill slightly transparent to see ECG
      scale_alpha(range = c(0.7, 0.9)) +
      labs(x = x_label, y = "", fill = "Count", title = title) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm"),
        legend.position = "none",
        plot.title = element_text(size = 10, hjust = 0)
      ) +
      geom_vline(xintercept = 0, color = "red", alpha = 0.3, linetype = "dashed") +
      # Add T-peak reference line if showing T-peak data
      {if(title == "T-peak referenced") 
        geom_vline(xintercept = t_peak_offset, color = "blue", alpha = 0.3, linetype = "dashed")
      } +
      # Always apply x_limits if provided (for alignment)
      {if(!is.null(x_limits)) 
        coord_cartesian(xlim = x_limits)
      } +
      annotate("text", 
               x = -Inf, y = Inf, 
               label = paste0("n=", nrow(df_filtered)), 
               hjust = -0.1, vjust = 1.5)

    return(p)
  }

  # Create individual plots with provided x_limits
  p1 <- create_density_plot(df_rpeak, "R-peak referenced", x_limits)
  p2 <- create_density_plot(df_tpeak, "T-peak referenced", x_limits)
  
  combined_plot <- plot_grid(
    p1, p2,
    ncol = 1,
    align = "v"
  )

  return(combined_plot)
}
