postprocess_included <- function(df) {
	# Move age-related columns to start at position 8 
	age_cols <- c("age_mean","age_min", "age_max", "age_group")
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
