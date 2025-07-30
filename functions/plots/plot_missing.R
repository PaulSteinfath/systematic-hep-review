ica_columns <- c(
  "ica_on_epochs", "rejected_components", 
  "rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria"
)

# CFA-specific columns
cfa_columns <- c("rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria")

# Ignore columns that are optional
opt_columns <- c("other_cfa_removal_strategy", "other_cleaning_strategy")


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
    
    # For CFA-specific columns check that CFA is among rejected components
    if (col %in% cfa_columns) {
      cfa_mentioned <- grepl("CFA", data$rejected_components, ignore.case = TRUE)
      condition <- condition & cfa_mentioned
    }
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
    
    # For CFA-specific columns: check that CFA is among rejected components
    if (col %in% cfa_columns) {
      cfa_mentioned <- grepl("CFA", data$rejected_components, ignore.case = TRUE)
      rel <- rel & cfa_mentioned
    }
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

    # For CFA-specific columns: check that CFA is among rejected components
    if (col %in% cfa_columns) {
      cfa_mentioned <- grepl("CFA", data$rejected_components, ignore.case = TRUE)
      rel <- rel & cfa_mentioned
    }
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
                                columns = NULL,
                                percentages = TRUE,
                                column_mapping_readable = column_mapping_readable_default,
                                pipeline_steps = NULL,
                                pipeline_colors = NULL,
                                plot_fill = plot_fill_default_single,
                                show_wordy_title = FALSE,
                                show_title = FALSE,
                                show_legend = FALSE,
                                x_lab = "Methodological Choice",
                                tilt_labels = FALSE,
                                x_ticks = TRUE,
                                y_ticks = TRUE,
                                flip = FALSE,
                                fixed = FALSE) {

  # Set all columns in opt_columns to a non-missing value so they are not counted as missing
  for (col in opt_columns) {
    if (col %in% names(df)) {
      df[[col]][is_missing(df[[col]])] <- "Not applicable"
    }
  }

  if (is.list(columns)) columns <- unlist(columns)

  results_df <- purrr::map_dfr(columns, function(col) {
    denom <- compute_denom_papers(df, col)
    missing <- compute_missing_papers(df, col)
    metric  <- if (percentages) missing / denom else missing
    data.frame(Column = col, Metric = metric, stringsAsFactors = FALSE)
  })

  results_df <- prepare_column_plot_data(results_df, 
                                         column_col = "Column", 
                                         value_col = "Metric", 
                                         method_columns = columns,
                                         column_mapping_readable = column_mapping_readable,
                                         pipeline_colors = pipeline_colors,
                                         fixed = fixed)

  y_lab <- if (percentages) "Proportion of Studies" else "Number of Studies"

  my_title <- if (!show_title) {
    NULL
  } else if (show_wordy_title) {
    "Missing Information"
  } else {
    paste("n =", dplyr::n_distinct(df$PMID), "studies")
  }

  p <- ggplot(results_df, aes(x = Column, y = Metric)) +
    theme_classic(base_family = "sans") +
    labs(title = my_title, x = x_lab, y = y_lab) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(size = 9,
                                 angle = if (tilt_labels) 45 else 0,
                                 hjust = if (tilt_labels) 1 else 0.5),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.title.y = element_text(size = 9)
    ) +
    scale_y_continuous(labels = if (percentages) scales::percent else waiver(),
                       expand = expansion(mult = c(0, .1)))

  if (!is.null(pipeline_colors)) {
    p <- p + geom_bar(aes(fill = Step), stat = "identity", color = "white", linewidth = 0.5) +
      scale_fill_manual(
        name = "Category",
        values = pipeline_colors,
        guide = if (show_legend) "legend" else "none"
      ) +
      theme(legend.position = "right", legend.justification = "center", legend.margin = margin(0, -1, 0, 0))
  } else {
    p <- p + geom_bar(stat = "identity", fill = plot_fill, color = "white", linewidth = 0.5)
  }

  if (!x_ticks) p <- p + theme(axis.text.x = element_blank())
  if (!y_ticks) {
    p <- p +
      theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  }
  if (flip)     p <- p + coord_flip()

  return(p)
}