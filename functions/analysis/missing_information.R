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
    condition <- condition & (data$modality == "EEG")
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
    rel <- data$modality == "EEG"
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
    rel <- data$modality == "EEG"
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


missing_information <- function(df, columns) {
  results_df <- purrr::map_dfr(columns, function(col) {
    denom <- compute_denom_papers(df, col)
    missing <- compute_missing_papers(df, col)
    percentage <- missing / denom
    data.frame(Column = col, 
               count = missing,
               percentage = percentage,
               stringsAsFactors = FALSE)
  })

  for (col in opt_columns) {
    if (col %in% names(df)) {
      results_df$Metric[results_df$Column == col] <- 0
    }
  }
}
