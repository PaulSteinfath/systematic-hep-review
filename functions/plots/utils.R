no_valid_data_stub <- function(message) {
  ggplot() + 
    theme_void() + 
    annotate("text", x = 0.5, y = 0.5, label = message)
}


clip_values <- function(values, low = 0, high = 1) {
  # Clip values to be in [low, high] range
  pmin(high, pmax(low, values))
}


# Create synthetic ECG data frame for plotting
create_ecg_data <- function(x_min, x_max, n_points = 500) {
  
  # R peak at t=0, T wave at t=300ms
  time <- seq(x_min, x_max, length.out = n_points)
  r_wave <- 1.8 * exp(-(time / 10)^2)        # R peak
  q_wave <- -0.1 * exp(-(time + 20)^2 / 100) # Q wave
  s_wave <- -0.15 * exp(-(time - 20)^2 / 100) # S wave
  p_wave <- 0.15 * exp(-(time + 100)^2 / 400) # P wave
  t_wave <- 0.2 * exp(-(time - 300)^2 / 3000) # T wave
  
  # Combine and create data frame
  data.frame(
    time = time,
    voltage = scale(p_wave + q_wave + r_wave + s_wave + t_wave, scale = FALSE)
  )
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

