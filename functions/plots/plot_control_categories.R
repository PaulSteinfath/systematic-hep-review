plot_control_categories <- function(df, tilt_labels = FALSE) {
  # Calculate occurrences for each category
  category_df <- control_counts(df,
                                by = "study",
                                level = "category") %>%
    arrange(percentage)
  
  # Fix the column order by sorting
  ordered_columns <- category_df$category
  category_df$category <- factor(category_df$category, 
                                 levels = ordered_columns)
  new_labels <- c("ECG and Heartbeat-Related Controls" = "ECG- and Heartbeat-\nRelated Controls",
                  "Heart Rate Variability (HRV) Controls" = "Heart Rate Variability\n(HRV) Controls",
                  "Cardiovascular and Blood Pressure Controls" = "Cardiovascular and\nBlood Pressure Controls",
                  "Respiration" = "Respiration",
                  "Demographic and Psychosocial Controls" = "Demographic and\nPsychosocial Controls",
                  "Physiological and Environmental Controls" = "Physiological and\nEnvironmental Controls",
                  "Task and Experimental Controls" = "Task and\nExperimental Controls",
                  "Other Controls" = "Other Controls")
  
  # Adjust colors to highlight ECG
  category_colors <- c()
  for (cat in unique(category_df$category)) {
    transformed_name <- new_labels[[cat]]
    if (cat == "ECG and Heartbeat-Related Controls") {
      category_colors[transformed_name] <- common_colors$ecg_controls
    } else {
      category_colors[transformed_name] <- common_colors$fill_default
    }
  }
  
  # Adjust labels
  levels(category_df$category) <- new_labels[levels(category_df$category)]
  
  # Plot
  total_count <- unique(category_df$total)
  p <- bar_panel(category_df, 
                 value_col = "percentage", 
                 column_col = "category", 
                 color_col = "category",
                 colors = category_colors,
                 flip = T, 
                 percentages = T,
                 show_legend = F,
                 title = "Control categories",
                 x_lab = "",
                 y_lab = "Proportion of studies") +
    labs(subtitle = paste0("n = ", total_count, " studies"))
  
  return(p)
}
