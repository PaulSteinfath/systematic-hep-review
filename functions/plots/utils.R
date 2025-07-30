no_valid_data_stub <- function(message) {
  ggplot() + 
    theme_void() + 
    annotate("text", x = 0.5, y = 0.5, label = message)
}


clip_values <- function(values, low = 0, high = 1) {
  # Clip values to be in [low, high] range
  pmin(high, pmax(low, values, na.rm = T), na.rm = T)
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
calculate_cumulative_counts <- function(df, 
                                        time_vec, 
                                        by = "pipeline",
                                        group_col = "PMID") { 
  if (nrow(df) == 0) {
    return(rep(0, length(time_vec)))
  }
  if (!by %in% c("study", "pipeline")) {
    stop("Can only aggregate cumulative windows by study or pipeline")
  }
  
  # Small offset to ensure intervals are checked correctly [start, end)
  precision_offset <- (time_vec[2] - time_vec[1]) / 2 

  # Count each pipeline separately
  if (by == "pipeline") {
    counts <- numeric(length(time_vec))
    
    for (i in 1:nrow(df)) {
      start_i <- df$start_time[i]
      end_i <- df$end_time[i]
      indices <- which(time_vec >= (start_i - precision_offset) & time_vec < (end_i - precision_offset))
      if (length(indices) > 0) {
        counts[indices] <- counts[indices] + 1
      }
    }
    
    return(counts)
  }
  
  # Combine windows from all pipelines within one study
  counts <- numeric(length(time_vec))
  unique_ids <- unique(df[[group_col]])
  for (pmid in unique_ids) {
    df_pmid <- df[df[[group_col]] == pmid, ]
    
    used_by_study <- logical(length(time_vec))
    for (i in 1:nrow(df_pmid)) {
      t_start <- df_pmid$start_time[i]
      t_end <- df_pmid$end_time[i]
      
      used_by_pipeline <- (time_vec >= (t_start - precision_offset)) & 
                          (time_vec < (t_end - precision_offset))
      used_by_study <- used_by_study | used_by_pipeline
    }
    
    indices <- which(used_by_study)
    if (length(indices) > 0) {
      counts[indices] <- counts[indices] + 1
    }
  }
  
  return(counts) 
}


prepare_column_plot_data <- function(df, 
                                     column_order, 
                                     pipeline_steps, 
                                     pipeline_colors) {
  if (!("Step" %in% names(df))) {
    df$Step <- sapply(df$Column, function(var_name) {
      get_pipeline_step(var_name)
    })
  }
  df$Step <- factor(df$Step, levels = names(pipeline_colors))
  
  df$Column <- factor(df$Column, 
                      levels = column_order) 
  levels(df$Column) <- make_readable(levels(df$Column))
  
  df
}
