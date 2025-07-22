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

leads_palette <- c('Lead I' = '#fc8d62', 
                   'Lead II' = '#66c2a5', 
                   'Lead III' = '#8da0cb')

r_t_peak_palette <- c("R-peak" = "#696969", "T-peak" = "#E69F00")

theme_set(plot_theme_default)

column_mapping_readable_default <- c(
  "Number of ECG Electrodes"= "ecg_num_electrodes" ,
  "ECG Lead" = "ecg_lead",
  "ECG Locations" = "ecg_locations",
  "ECG Ground" = "ecg_ground",
  "HER Window Type" = "hep_window_type",
  "HER Channels Selected" = "hep_channels_selected",
  "Hypothesis" = "hypothesis",
  "HER Start (ms)" = "hep_start",
  "HER End (ms)" = "hep_end",
  "Number of channels" = "meeg_num_electrodes",
  "Layout" = "meeg_layout",
  "Online Reference" = "reference_online",
  "Offline Reference" = "reference_offline",
  "High-Pass Filter (Hz)" = "high_pass",
  "Low-Pass Filter (Hz)" = "low_pass",
  "ICA on Epochs" = "ica_on_epochs",
  "ICA" = "ICA",
  "Type of Rejected Components" = "rejected_components",
  "Number of Rejected Cardiac ICs" = "rejected_cardiac_ics",
  "CFA Rejection Approach" = "cfa_rej_approach",
  "CFA Rejection Criteria" = "cfa_rej_criteria",
  "Other CFA Removal Strategy" = "other_cfa_removal_strategy",
  "Other Cleaning Strategy" = "other_cleaning_strategy",
  "Number of Groups" = "groups",
  "Number of Conditions" = "conditions",
  "Number of Trials" = "trials", # Not sure if we need it
  "HER Relative To" = "hep_relative_to",
  "Baseline Start (ms)" = "baseline_start_ms",
  "Baseline End (ms)" = "baseline_end_ms",
  "HER Value" = "value",
  "Averaging Across Channels" = "averaging_channels",
  "Averaging Across Timepoints" = "averaging_time",
  "Statistical Test" = "statistics",
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
  "Modality (EEG/MEG)" = "modality",
  "EEG Locations" = "eeg_locations",
  #"Number of Trials" = "trials_Mean",# Not sure if we need it
  "Resting-state HER" = "rsHEP"
)

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

# Add pipeline step categorization - correctly categorizing all variables
pipeline_steps <- list(
  "Acquisition" = c(
    "meeg_num_electrodes", "ecg_num_electrodes", "ecg_lead", "ecg_locations", 
    "ecg_ground", "reference_online", "modality"
  ),
  "Experiment" = c(
    "groups", "conditions", "length_min", "rsHEP", "sample_size", "trials"
  ),
  "Preprocessing" = c(
    "reference_offline", "high_pass", "low_pass", "ICA", 
    "ica_on_epochs", "rejected_components", "rejected_cardiac_ics",
    "cfa_rej_approach", "cfa_rej_criteria", "other_cleaning_strategy", "other_cfa_removal_strategy"
  ),
  "HER Estimation" = c(
    "hep_relative_to", "hep_start", "hep_end",
    "baseline_start_ms", "baseline_end_ms", "value", "hep_channels_selected",
    "averaging_channels", "averaging_time", "hep_window_type"
  ),
  "Statistics" = c(
    "clustering", "statistics", "permutations", "hypothesis"
  )
)
pipeline_colors <- c(
  "Experiment"     = "#505050",   # Medium-dark grey
  "Acquisition"    = "#6A6A6A",   # Medium grey
  "Preprocessing"  = "#8A8A8A",   # Medium-light grey 
  "HER Estimation" = "#B0B0B0",   # Light grey
  "Statistics"     = "#D0D0D0"    # Lightest grey
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

prepare_column_plot_data <- function(df, 
                                     column_col, 
                                     value_col, 
                                     method_columns, 
                                     column_mapping_readable, 
                                     pipeline_colors = NULL, 
                                     fixed = FALSE) {
  # Apply readable column names
  df[[column_col]] <- apply_column_mapping(df[[column_col]], column_mapping_readable)
  
  # Add Step column if coloring by pipeline group
  if (!is.null(pipeline_colors)) {
    df$Step <- sapply(df[[column_col]], function(readable) {
      var_name <- column_mapping_readable[readable]
      if (is.na(var_name)) var_name <- readable
      get_pipeline_step(var_name)
    })
    df$Step <- factor(df$Step, levels = names(pipeline_colors))
  }
  
  # Set column factor levels
  if (fixed) {
    fixed_order <- apply_column_mapping(method_columns, column_mapping_readable)
    df[[column_col]] <- factor(df[[column_col]], levels = fixed_order)
  } else {
    if (!is.null(pipeline_colors)) {
      df <- df %>%
        dplyr::arrange(match(Step, c("Statistics", "HER Estimation", "Preprocessing", "Acquisition", "Experiment")), dplyr::desc(.data[[value_col]])) %>%
        dplyr::mutate(!!column_col := factor(.data[[column_col]], levels = unique(.data[[column_col]])))
    } else {
      df <- df %>%
        dplyr::arrange(dplyr::desc(.data[[value_col]])) %>%
        dplyr::mutate(!!column_col := factor(.data[[column_col]], levels = unique(.data[[column_col]])))
    }
  }
  
  return(df)
}

custom_theme <- function() {
  theme(
    plot.title = element_text(size = 11), 
    plot.subtitle = element_text(size = 9)
  )
}
