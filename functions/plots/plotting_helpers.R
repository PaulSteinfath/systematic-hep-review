library(dplyr)
library(ggplot2)

plot_theme_default <- theme_classic(base_family = "sans") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8), 
        axis.title.x = element_text(size = 9, margin = margin(t = 4)),
        axis.title.y = element_text(size = 9))

plot_fill_default_single <- "#696969"

plot_fill_default <- c("#696969","#A9A9A9","#8a8888")

theme_set(plot_theme_default)

column_mapping_readable_default <- c(
  "Number of ECG Electrodes"= "ecg_num_electrodes" ,
  "ECG Lead" = "ecg_lead",
  "ECG Locations" = "ecg_locations",
  "ECG Ground" = "ecg_ground",
  "HEP Window Type" = "hep_window_type",
  "HEP Channels Selected" = "hep_channels_selected",
  "Hypothesis" = "hypothesis",
  "HEP Start (ms)" = "hep_start",
  "HEP End (ms)" = "hep_end",
  "Number of channels" = "channels",
  "Layout" = "layout",
  "Online Reference" = "reference_online",
  "Offline Reference" = "reference_offline",
  "High-Pass Filter (Hz)" = "high_pass",
  "Low-Pass Filter (Hz)" = "low_pass",
  "ICA on Epochs" = "ica_on_epochs",
  "Type of Rejected Components" = "rejected_components",
  "Number of Rejected Cardiac ICs" = "rejected_cardiac_ics",
  "CFA Rejection Approach" = "cfa_rej_approach",
  "CFA Rejection Criteria" = "cfa_rej_criteria",
  "Other CFA Removal Strategy" = "other_cfa_removal_strategy",
  "Other Cleaning Strategy" = "other_cleaning_strategy",
  "Number of Groups" = "groups",
  "Number of Conditions" = "conditions",
  "Number of Trials" = "trials",
  "HEP Relative To" = "hep_relative_to",
  "Baseline Start (ms)" = "baseline_start_ms",
  "Baseline End (ms)" = "baseline_end_ms",
  "HEP Value" = "value",
  "Averaging Across Channels" = "averaging_channels",
  "Averaging Across Timepoints" = "averaging_time",
  "Statistic" = "statistics",
  "Number of Permutations" = "permutations",
  "Significant Test" = "significant_test",
  "Significant Channels" = "significant_channels",
  "Significant Relative To" = "significant_relative_to",
  "Significant Start (ms)" = "significant_start_ms",
  "Significant end (ms)" = "significant_end_ms",
  "Controls" = "controls",
  "Sample Size" = "sample_size",
  "Cluster-Based Permutation" = "clustering",
  "Length (min)" = "length_min", 
  "Modality (EEG/MEG)" = "Modality",
  "EEG Locations" = "eeg_locations"
)

# Add pipeline step categorization - correctly categorizing all variables
pipeline_steps <- list(
  "Acquisition" = c(
    "channels", "ecg_num_electrodes", "ecg_lead", "ecg_locations", 
    "ecg_ground", "reference_online", "length_min", "hep_channels_selected"
  ),
  "Preprocessing" = c(
    "reference_offline", "high_pass", "low_pass", "ICA", 
    "ica_on_epochs", "rejected_components", "rejected_cardiac_ics",
    "cfa_rej_approach", "cfa_rej_criteria", "other_cleaning_strategy", "other_cfa_removal_strategy"
  ),
  "HEP Estimation" = c(
    "hep_relative_to", "hep_start", "hep_end",
    "baseline_start_ms", "baseline_end_ms", "value", 
    "averaging_channels", "averaging_time", "hep_window_type"
  ),
  "Statistics" = c(
    "clustering", "statistics", "permutations", "sample_size", "trials", "conditions", "groups", "hypothesis"
  )
)

# Define colors for pipeline steps
pipeline_colors <- c(
  "Acquisition" = "#4D6B89",      # Slate blue
  "Preprocessing" = "#6A8A82",    # Muted teal
  "HEP Estimation" = "#7D9D85",   # Sage green
  "Statistics" = "#A4B494"        # Light olive
)

# Enhanced function to ensure we always get a valid pipeline step
get_pipeline_step <- function(var) {
  # Return "Other" for any invalid inputs
  if (length(var) == 0 || 
      all(is.null(var)) || 
      all(is.na(var)) || 
      all(var == "")) {
    return("Other")
  }
  
  # Check each pipeline step category
  for (step in names(pipeline_steps)) {
    if (var %in% pipeline_steps[[step]]) {
      return(step)
    }
  }
  
  # If not found directly, try case-insensitive match
  var_lower <- tolower(var)
  for (step in names(pipeline_steps)) {
    for (entry in pipeline_steps[[step]]) {
      if (var_lower == tolower(entry)) {
        return(step)
      }
    }
  }
  
  return("Other")
}

# Map the readable names back to their variable names for pipeline categorization
get_variable_name <- function(readable_name, mapping) {
  var_name <- names(mapping)[which(mapping == readable_name)]
  if (length(var_name) > 0) return(var_name[1])
  return(readable_name)  # If not found, return the original name
}

# Function to get pipeline step for a readable variable name
get_readable_pipeline_step <- function(readable_name, mapping) {
  # Convert the readable name back to variable name
  var_name <- get_variable_name(readable_name, mapping)
  # Then get its pipeline step
  return(get_pipeline_step(var_name))
}

save_plot <- function(p, vis_path, file_name, plot_format = "svg", plot_width = 6, plot_height = 6) {
  full_path <- file.path(vis_path, paste0(file_name, ".", plot_format))
  ggsave(filename = full_path, plot = p, width = plot_width, height = plot_height)
}

apply_column_mapping <- function(names_vector, mapping) {
  if (!is.null(mapping)) {
    sapply(names_vector, function(x) {
      ind <- which(mapping == x)
      if (length(ind) > 0) names(mapping)[ind[1]] else x
    })
  } else {
    names_vector
  }
}

column_barplot <- function(results_df, 
                           x_col, 
                           y_col, 
                           variables,
                           vertical = FALSE,
                           group_var = NULL,
                           align_by_magnitude = TRUE,
                           gap = 0.5,
                           x_lab = NULL,
                           y_lab = NULL,
                           plot_title = NULL,
                           plot_fill = plot_fill_default,
                           plot_theme = plot_theme_default,
                           column_mapping_readable = column_mapping_default,
                           group_bar_pos = "dodge",
                           x_ticks = TRUE) {
  
  # Don't reorder if already a factor
  if (!is.factor(results_df[[x_col]])) {
    results_df[[x_col]] <- factor(results_df[[x_col]], levels = unique(results_df[[x_col]]))
  }
  
  # Always use fill aesthetic if Step column exists
  p <- if ("Step" %in% names(results_df)) {
    ggplot(results_df, aes(x = !!sym(x_col), y = !!sym(y_col), fill = Step)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = plot_fill)
  } else {
    ggplot(results_df, aes(x = !!sym(x_col), y = !!sym(y_col))) +
      geom_bar(stat = "identity", fill = plot_fill[1])
  }
  
  # Add labels and theme
  p <- p + plot_theme +
    labs(x = ifelse(is.null(x_lab), x_col, x_lab),
         y = ifelse(is.null(y_lab), y_col, y_lab),
         title = ifelse(is.null(plot_title), "", plot_title))
  
  if (vertical) {
    p <- p + coord_flip() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      theme(legend.position = c(1, 0), legend.justification = c(1, 0))
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = c(0, 1), legend.justification = c(0, 1))
  }
  
  return(p)
}

create_year_group_columns <- function(df, years) {
  # Check that the 'Year' column exists
  if (!"Year" %in% names(df)) {
    stop("The input data frame must contain a column named 'Year'.")
  }
  
  # Ensure that Year values are numeric (or can be converted)
  year_vals <- as.numeric(df$Year)
  if (any(is.na(year_vals))) {
    warning("Some values in the 'Year' column could not be converted to numeric.")
  }
  
  # For each year threshold, create a new column
  new_columns <- lapply(years, function(threshold) {
    ifelse(year_vals <= threshold,
           paste0(threshold, " and before"),
           paste0("After ", threshold))
  })
  
  names(new_columns) <- paste0("year_group_", years)
  
  return(as.data.frame(new_columns, stringsAsFactors = FALSE))
}
