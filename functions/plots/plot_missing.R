missing_columns_default <- list(
                                 c("channels", 
                                 "ecg_num_electrodes", "ecg_lead", 
                                 "ecg_locations", "reference_online", 
                                 "reference_offline", "high_pass", "low_pass", 
                                 "ICA", 
                                 "rejected_components",  
                                 "hep_channels_selected", 
                                 "hep_relative_to", "hep_start", 
                                 "hep_end", 
                                 "value",  "statistics", "permutations", "sample_size"
                                ),
                                c("cfa_rej_approach", "cfa_rej_criteria","baseline_start_ms", "baseline_end_ms"),
                                c("rejected_cardiac_ics", "ecg_ground", "trials", "length_min")
                                )

ica_columns <- c(
  "ica_on_epochs", "rejected_components", 
  "rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria"
)

is_missing <- function(x) {
  x_char <- tolower(as.character(x))
  is.na(x) | x == "" | x_char %in% c("unknown", "na")
}

# This function uses extra conditions (ICA, clustering, Modality, trials) as needed.
calc_missing_for_column <- function(data, col) {
  # 'data' is expected to be a subset corresponding to a single paper.
  missing_values <- is_missing(data[[col]])
  
  # For ica_columns: only count missing if ICA == 1.
  if (col %in% ica_columns) {
    condition <- data$ICA == 1
  } else {
    condition <- rep(TRUE, nrow(data))
  }
  
  # For columns with "perm": only count if clustering == 1.
  if (grepl("perm", col, ignore.case = TRUE)) {
    condition <- condition & (data$clustering == 1)
  }
  
  # For trials: also count as missing if the text starts with "[est" (ignoring case).
  if (tolower(col) == "trials") {
    extra_missing <- grepl("^\\[est", data[[col]], ignore.case = TRUE)
    missing_values <- missing_values | extra_missing
  }
  
  # For reference online/offline: only count if Modality == "EEG".
  if (tolower(col) %in% c("reference online", "reference_online", 
                          "reference offline", "reference_offline")) {
    condition <- condition & (data$Modality == "EEG")
  }
  
  # Return TRUE for rows that are both relevant and missing.
  missing_values & condition
}

# Compute denominator at paper level: count unique PMIDs among rows that are relevant.
compute_denom_papers <- function(data, col) {
  if (col %in% ica_columns) {
    rel <- data$ICA == 1
  } else if (grepl("perm", col, ignore.case = TRUE)) {
    rel <- data$clustering == 1
  } else if (tolower(col) %in% c("reference online", "reference_online",
                                 "reference offline", "reference_offline")) {
    rel <- data$Modality == "EEG"
  } else {
    rel <- rep(TRUE, nrow(data))
  }
  length(unique(data$PMID[rel]))
}

# Compute number of papers missing info for a given column.
compute_missing_papers <- function(data, col) {
  if (col %in% ica_columns) {
    rel <- data$ICA == 1
  } else if (grepl("perm", col, ignore.case = TRUE)) {
    rel <- data$clustering == 1
  } else if (tolower(col) %in% c("reference online", "reference_online",
                                 "reference offline", "reference_offline")) {
    rel <- data$Modality == "EEG"
  } else {
    rel <- rep(TRUE, nrow(data))
  }
  data_rel <- data[rel, , drop = FALSE]
  if(nrow(data_rel) == 0) return(0)
  # Split by paper.
  papers <- split(data_rel, data_rel$PMID)
  # A paper is missing if any row in that paper meets the missing condition.
  missing_indicator <- sapply(papers, function(paper) {
    any(calc_missing_for_column(paper, col))
  })
  sum(missing_indicator)
}

plot_missing <- function(df,
                         columns = missing_columns_default,
                         column_mapping_readable = column_mapping_readable_default,
                         group_var = NULL,
                         vertical = FALSE,
                         percentages = TRUE,
                         plot_fill = plot_fill_default,
                         plot_theme = plot_theme_default,
                         align_by_magnitude = TRUE,
                         gap = 0.5,
                         group_bar_pos = "dodge",
                         show_wordy_title = FALSE,
                         x_lab = "Methodological Choice",
                         x_ticks = TRUE,
                         pipeline_steps = NULL,
                         pipeline_colors = NULL,
                         fixed_order = NULL) {
  # Flatten columns if needed
  if (is.list(columns)) {
    flat_cols <- unlist(columns)
  } else {
    flat_cols <- columns
  }
  
  results_df <- NULL
  
  if (is.null(group_var)) {
    # Ungrouped: compute for entire df.
    for (col in flat_cols) {
      denom <- compute_denom_papers(df, col)
      missing_count <- compute_missing_papers(df, col)
      metric_value <- if (percentages) (missing_count / denom * 100) else missing_count
      temp <- data.frame(Column = col,
                         Missing = missing_count,
                         Metric = metric_value,
                         stringsAsFactors = FALSE)
      results_df <- rbind(results_df, temp)
    }
  } else {
    # Grouped: compute per group.
    if (!(group_var %in% names(df))) {
      stop(paste("Grouping variable", group_var, "not found in data."))
    }
    groups <- unique(df[[group_var]])
    for (col in flat_cols) {
      temp_list <- lapply(groups, function(g) {
        dfg <- df[df[[group_var]] == g, ]
        denom <- compute_denom_papers(dfg, col)
        missing_count <- compute_missing_papers(dfg, col)
        metric_value <- if (percentages) (missing_count / denom * 100) else missing_count
        data.frame(Column = col,
                   Missing = missing_count,
                   total = denom,
                   Metric = metric_value,
                   Group = g,
                   stringsAsFactors = FALSE)
      })
      temp <- do.call(rbind, temp_list)
      results_df <- bind_rows(results_df, temp)
    }
  }
  
  # First apply column mapping
  results_df$Column <- apply_column_mapping(results_df$Column, column_mapping_readable)

  # Add Step column based on the ORIGINAL variable names (before mapping)
  results_df$Step <- sapply(results_df$Column, function(readable) {
    var_name <- column_mapping_readable[readable]  # because 'readable' is the name in the mapping
    if (is.na(var_name)) var_name <- readable      # fallback if not found
    get_pipeline_step(var_name)
  })
  
  # Set Step as factor with pipeline_colors levels
  results_df$Step <- factor(results_df$Step, levels = names(pipeline_colors))

  # Set fixed_order if provided
  if (!is.null(fixed_order)) {
    results_df$Column <- factor(results_df$Column, levels = fixed_order)
  }

  # Define labels
  if (show_wordy_title) {
    my_title <- if (is.null(group_var)) 
      "Missing Information per Column" 
    else 
      "Missing Information per Column by Group"
  } else {
    n_unique_papers <- n_distinct(df$PMID)
    my_title <- paste("n =", n_unique_papers)
  }
  
  y_lab <- if (percentages) {
    "Percentage of Papers with Missing Information"
  } else {
    "Number of Papers with Missing Information"
  }

  # Build plot with proper Step coloring
  p <- ggplot(results_df, aes(x = Column, y = Metric, fill = Step)) +
       geom_bar(stat = "identity") +
       scale_fill_manual(values = pipeline_colors) +
       labs(x = x_lab, y = y_lab, title = my_title) +
       plot_theme

  if (vertical) {
    # Flip plot and remove variable name labels (flipped x-axis), keep metric (y-axis) visible
    p <- p + coord_flip() + scale_x_discrete(labels = NULL)
  } else {
    p <- p + scale_y_continuous(breaks = NULL)
  }

  # Keep legend visible for this plot
  return(p)
}