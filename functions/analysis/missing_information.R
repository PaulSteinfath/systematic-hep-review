ica_columns <- c(
  "ica_on_epochs", 
  "rejected_components", 
  "rejected_cardiac_ics", 
  "cfa_rej_approach", 
  "cfa_rej_criteria"
)

# CFA-specific columns
cfa_columns <- c("rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria")

# Ignore columns that are optional
opt_columns <- c("other_cfa_removal_strategy", "other_cleaning_strategy")

# Columns that should be filled only if EEG was used
eeg_columns <- c(
  "reference_online",
  "reference_offline",
  "hep_channels_selected"
)


is_missing <- function(df, col) {
  # Process the default case
  x <- df[[col]]
  x_char <- tolower(as.character(x))
  missing <- is.na(x) | x == "" | x_char %in% c("unknown", "na")
  
  # For trials: also count as missing if the text starts with "[est" (ignoring case).
  if (tolower(col) == "trials") {
    extra_missing <- grepl("^\\[est", df[[col]], ignore.case = TRUE)
    missing <- missing | extra_missing
  }
  
  missing
}


is_relevant <- function(df, col) {
  relevant <- rep(T, nrow(df))
  
  # For ica_columns: only count missing if ICA == 1
  if (col %in% ica_columns) {
    relevant <- relevant & df$ICA == 1
    
    # For CFA-specific columns check that CFA is among rejected components
    if (col %in% cfa_columns) {
      cfa_mentioned <- grepl("CFA", df$rejected_components, ignore.case = TRUE)
      relevant <- relevant & cfa_mentioned
    }
  }
  
  # For columns with "perm": only count if clustering == 1.
  if (grepl("perm", col, ignore.case = TRUE)) {
    relevant <- relevant & (df$clustering == 1)
  }
  
  # For reference/channel columns: only count if modality == "EEG".
  if (tolower(col) %in% eeg_columns) {
    relevant <- relevant & (df$modality == "EEG")
  }
  
  relevant
}


# Compute denominator at paper level: count unique PMIDs among rows that are relevant.
compute_denom_papers <- function(df, col, by = "study") {
  relevant <- is_relevant(df, col)
  
  if (by == "pipeline") {
    sum(relevant)
  } else if (by == "study") {
    length(unique(df$PMID[relevant]))
  } else {
    stop("either by study or by pipeline")
  }
}


decide_missing <- function(missing_pipelines, criterion = "any") {
  if (criterion == "any") {
    # A paper is missing if ANY row in that paper meets the missing condition.
    any(missing_pipelines)
  } else if (criterion == "all") {
    # A paper is missing if ALL rows in that paper meet the missing condition.
    all(missing_pipelines)
  } else {
    stop("bad criterion - any and all are supported")
  }
}


# Compute number of papers missing info for a given column.
compute_missing_papers <- function(df, col, by = "study", criterion = "any") {
  relevant <- is_relevant(df, col)
  if (sum(relevant) == 0) return(0)
  
  missing <- is_missing(df, col) & relevant
  
  if (by == "pipeline") {
    sum(missing)
  } else if (by == "study") {
    # Split by paper, decide according to the criterion
    papers <- split(missing, df$PMID)
    missing_paper <- sapply(papers, criterion)
    sum(missing_paper)
  } else {
    stop("either by study or by pipeline")
  }
}


missing_information <- function(df, columns) {
  results_df <- purrr::map_dfr(columns, function(col) {
    denom <- compute_denom_papers(df, col)
    missing <- compute_missing_papers(df, col)
    percentage <- missing / denom
    data.frame(Column = col, 
               count = missing,
               total = denom,
               percentage = percentage,
               stringsAsFactors = FALSE)
  })

  for (col in opt_columns) {
    if (col %in% names(df)) {
      results_df$count[results_df$Column == col] <- 0
      results_df$percentage[results_df$Column == col] <- 0
    }
  }
  
  results_df
}
