# Prepare table-friendly versions of columns so filters pick the right input type
prepare_filter_data <- function(df) {
  na_tokens <- c("na", "unknown")
  
  binary_map <- list(
    ICA = c("0" = "No", "1" = "Yes"),
    ica_on_epochs = c("0" = "No", "1" = "Yes"),
    setting = c("0" = "Task", "1" = "Resting"),
    preregistration = c("0" = "No", "1" = "Yes"),
    patients = c("0" = "No", "1" = "Yes"),
    new_data = c("0" = "No", "1" = "Yes"),
    clustering = c("0" = "No", "1" = "Yes"),
    significant_test = c("0" = "No", "1" = "Yes"),
    averaging_channels = c("0" = "No", "1" = "Yes"),
    averaging_time = c("0" = "No", "1" = "Yes"),
    clean_noisy_epochs = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_bad_channels = c("FALSE" = "No", "TRUE" = "Yes"),
    has_resting = c("FALSE" = "No", "TRUE" = "Yes"),
    has_task = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_noisy_epochs = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_bad_channels = c("FALSE" = "No", "TRUE" = "Yes"),
    reject_cfa_ics = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_use_minimal_rr = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_use_minimal_artifact_window = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_csd = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_regress = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_pca = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_subtract_rest = c("FALSE" = "No", "TRUE" = "Yes")
  )
  
  numeric_cols <- c(
    "year", "sample_size", "meeg_num_electrodes", "meeg_sfreq_orig", 
    "meeg_sfreq_final", "meg_num_grad", "meg_num_mag", "ecg_num_electrodes",
    "ecg_sfreq_orig", "ecg_sfreq_final", "length_min", "high_pass", "low_pass",
    "groups", "conditions", "hep_start", "hep_end", "ecg_high_pass",
    "ecg_low_pass", "baseline_start_ms", "baseline_end_ms", "permutations",
    "significant_start_ms", "significant_end_ms", "cfa_minimal_rr"
  )
  
  # Numeric columns: replace na tokens, convert to numeric
  for (nm in numeric_cols) {
    vals <- as.character(df[[nm]])
    vals[tolower(vals) %in% na_tokens] <- NA
    df[[nm]] <- as.numeric(vals)
  }
  
  # Binary columns: map 0/1 to No/Yes, convert to factor
  for (nm in names(binary_map)) {
    vals <- as.character(df[[nm]])
    mapped <- binary_map[[nm]][vals]
    mapped[is.na(mapped)] <- vals[is.na(mapped)]  # keep unmapped values as-is
    df[[nm]] <- factor(mapped)
  }
  
  # Convert character columns with few unique values to factor & with many unique values keep as character
  text_cols <- c("title", "authors", "topic", "age_range", "eeg_locations",
                 "ecg_locations", "rejected_components",
                 "cfa_rej_criteria", "other_cleaning_strategy",
                 "hep_eeg_channels_selected", "trials", "trial_estimation",
                 "significant_eeg_channels", "controls", "journal",
                 "journal_full", "paper")
  for (nm in names(df)) {
    if (is.character(df[[nm]]) && !(nm %in% text_cols)) {
      df[[nm]] <- factor(df[[nm]])
    }
  }
  
  df
}


postprocess_included <- function(df) {
	# Move age-related columns to start at position 8 
	age_cols <- c("age_mean", "age_min", "age_max", "age_group")
	remaining <- setdiff(names(df), age_cols)
	new_order <- append(remaining, age_cols, after = 7)
	df <- df[, new_order]

	# Move journal-related columns to the end 
	journal_cols <- c("journal", "journal_full", "paper", "PMID")
	remaining <- setdiff(names(df), journal_cols)
	df <- df[, c(remaining, journal_cols)]

	# Ensure trial-related columns are adjacent, insert after `conditions`
	trial_cols <- c("trials", "trial_estimation", "trials_Mean", "trials_SD", "trials_original")
	remaining <- setdiff(names(df), trial_cols)
	insert_after <- which(remaining == "conditions")
	new_order <- append(remaining, trial_cols, after = insert_after)
	df <- df[, new_order]

	# Drop any unwanted columns
  # remove hep_approach in favor of method_category because its used for plotting. 
	cols_to_remove <- c("hep_approach", "method_numeric", "ecg_event_approach")  
	df <- df[, !(names(df) %in% cols_to_remove)]
	
	return(df)
}
