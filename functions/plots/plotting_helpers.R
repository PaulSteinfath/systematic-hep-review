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
  "HEP Window Type" = "hep_window_type",
  "HEP Channels Selected" = "hep_channels_selected",
  "Hypothesis" = "hypothesis",
  "HEP Start (ms)" = "hep_start",
  "HEP End (ms)" = "hep_end",
  "Number of channels" = "meeg_num_electrodes",
  "Layout" = "meeg_layout",
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
  "Modality (EEG/MEG)" = "modality",
  "EEG Locations" = "eeg_locations"
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
  # 1. Build a lookup table from the original variables input.
  # If variables is a list, check whether every element is of length 1.
  if (is.list(variables)) {
    if (all(sapply(variables, length) == 1)) {
      flat_vars <- unlist(variables)
      var_group_ids <- rep(1, length(flat_vars))
      var_group_labels <- rep("Group 1", length(flat_vars))
    } else {
      flat_vars <- unlist(variables)
      var_group_ids <- rep(seq_along(variables), times = sapply(variables, length))
      var_group_labels <- paste0("Group ", var_group_ids)
    }
  } else {
    flat_vars <- variables
    var_group_ids <- rep(1, length(flat_vars))
    var_group_labels <- rep("Group 1", length(flat_vars))
  }
  lookup_df <- data.frame(VarName = flat_vars, 
                          VarGroup = var_group_labels, 
                          OrigOrder = seq_along(flat_vars),
                          stringsAsFactors = FALSE)
  
  lookup_df$VarName <- apply_column_mapping(lookup_df$VarName, column_mapping_readable)
  
  # 2. Merge the lookup table with results_df.
  results_df <- merge(results_df, lookup_df, by.x = x_col, by.y = "VarName", all.x = TRUE)
  
  # 3. Compute ordering and x-axis positions.
  if (is.null(group_var)) {
    # Ungrouped: we use x_col, VarGroup, OrigOrder, and y_col.
    methods_df <- results_df %>%
      select(!!sym(x_col), VarGroup, OrigOrder, !!sym(y_col)) %>%
      distinct()
    if (align_by_magnitude) {
      methods_df <- methods_df %>% 
        group_by(VarGroup) %>% 
        arrange(!!sym(y_col), .by_group = TRUE) %>% 
        ungroup()
    } else {
      methods_df <- methods_df %>% 
        group_by(VarGroup) %>% 
        arrange(OrigOrder, .by_group = TRUE) %>% 
        ungroup()
    }
  } else {
    # Grouped case: use the original lookup table as the ordering base.
    methods_df <- lookup_df
    # Rename the key column to x_col.
    names(methods_df)[names(methods_df) == "VarName"] <- x_col
    if (align_by_magnitude) {
      # Compute the maximum y-value per variable (ignoring group differences)
      ordering_df <- results_df %>%
        group_by(!!sym(x_col)) %>%
        summarise(max_val = max(!!sym(y_col), na.rm = TRUE), .groups = "drop")
      # Merge with the lookup table
      methods_df <- merge(methods_df, ordering_df, by = x_col, all.x = TRUE)
      methods_df <- methods_df %>% 
        group_by(VarGroup) %>% 
        arrange(max_val, .by_group = TRUE) %>% 
        ungroup()
    } else {
      methods_df <- methods_df %>% arrange(OrigOrder)
    }
  }
  
  # Assign sequential positions within each group and add gap offsets.
  methods_df <- methods_df %>%
    group_by(VarGroup) %>%
    mutate(pos_in_group = row_number()) %>%
    ungroup()
  
  group_info <- methods_df %>%
    group_by(VarGroup) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(VarGroup) %>%
    mutate(offset = lag(cumsum(n), default = 0) + (row_number() - 1) * gap)
  
  methods_df <- merge(methods_df, group_info, by = "VarGroup")
  methods_df <- methods_df %>% mutate(xpos = pos_in_group + offset)
  
  # 4. Merge x positions back into results_df.
  results_df <- merge(results_df, methods_df[, c(x_col, "xpos")], by = x_col, all.x = TRUE)
  
  # 5. Build the plot.
  if (is.null(group_var)) {
    # If no grouping, use only the first color.
    p <- ggplot(results_df, aes(x = xpos, y = !!sym(y_col))) +
      geom_bar(stat = "identity", fill = plot_fill[1])
  } else {
    results_df[[group_var]] <- factor(results_df[[group_var]])
    p <- ggplot(results_df, aes(x = xpos, y = !!sym(y_col), fill = !!sym(group_var))) +
      geom_bar(stat = "identity", position = group_bar_pos)
    # If there are fewer groups than colors in the vector, use scale_fill_manual.
    n_groups <- length(unique(results_df[[group_var]]))
    if(n_groups <= length(plot_fill)) {
      p <- p + scale_fill_manual(values = plot_fill, name = NULL)
    } else {
      p <- p + labs(fill = NULL)
    }
  }
  
  # Conditionally control the x-axis tick labels.
  if (x_ticks) {
    p <- p + scale_x_continuous(breaks = methods_df$xpos, 
                                labels = methods_df[[x_col]],
                                expand = expansion(mult = c(0.01, 0.01)))
  } else {
    p <- p + scale_x_continuous(breaks = methods_df$xpos, 
                                labels = NULL,
                                expand = expansion(mult = c(0.01, 0.01)))
  }
  
  
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
