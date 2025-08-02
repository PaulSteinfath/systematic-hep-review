plot_ecg_controls <- function(df, tilt_labels = FALSE) {
  # Calculate occurrences for each variable from ECG category
  controls_df <- control_counts(df,
                                by = "study",
                                level = "variable") %>%
    filter(category == "ECG and Heartbeat-Related Controls") %>%
    arrange(percentage)
  
  # Fix the column order by sorting
  ordered_columns <- controls_df$variable
  controls_df$variable <- factor(controls_df$variable, 
                                 levels = ordered_columns)
  
  # Adjust labels
  new_labels <- c("ECG" = "ECG",
                  "HEP-ECG Correlation" = "HER-ECG\nCorrelation",
                  "Surrogate Heartbeats" = "Surrogate\nHeartbeats",
                  "RR Interval" = "RR Interval",
                  "Number of Heartbeats" = "Number of\nHeartbeats",
                  "QT Interval" = "QT Interval",
                  "Control interval" = "Control interval",
                  "T-Wave Latency" = "T-Wave Latency")
  levels(controls_df$variable) <- new_labels[levels(controls_df$variable)]
  
  # Plot
  total_count <- unique(controls_df$total)
  p <- bar_panel(controls_df, 
                 value_col = "percentage", 
                 column_col = "variable", 
                 flip = T, 
                 percentages = T,
                 title = "ECG- and heartbeat-related controls",
                 x_lab = "",
                 y_lab = "Proportion of studies") +
    labs(subtitle = paste0("n = ", total_count, " studies"))
  
  # Override the default fill color to match "Other Controls" category
  p <- p + 
    theme(panel.background = element_rect(fill = "white")) +
    guides(fill = "none")
 
  p$layers[[1]]$aes_params$fill <- common_colors$ecg_controls
  
  return(p)
}