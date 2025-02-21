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
                         x_ticks = TRUE) {
  
  # Flatten columns if needed.
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
  
  # Apply column mapping.
  results_df$Column <- apply_column_mapping(results_df$Column, column_mapping_readable)
  
  # Define axis labels and title.
  y_lab <- if (percentages) "Percentage of Papers with Missing Information" else "Number of Papers with Missing Information"
  if (show_wordy_title) {
    my_title <- if (is.null(group_var)) 
      "Missing Information per Column" 
    else 
      "Missing Information per Column by Group"
  } else {
    n_unique_papers <- n_distinct(df$PMID)
    my_title <- paste("n =", n_unique_papers)
  }
  
  # Call the generic column_barplot().
  p <- column_barplot(results_df = results_df,
                      x_col = "Column",
                      y_col = "Metric",
                      variables = columns,
                      vertical = vertical,
                      group_var = if (is.null(group_var)) NULL else "Group",
                      align_by_magnitude = align_by_magnitude,
                      gap = gap,
                      x_lab = x_lab,
                      y_lab = y_lab,
                      plot_title = my_title,
                      plot_fill = plot_fill,
                      plot_theme = plot_theme,
                      column_mapping_readable = column_mapping_readable,
                      group_bar_pos = group_bar_pos,
                      x_ticks = x_ticks)
  return(p)
}