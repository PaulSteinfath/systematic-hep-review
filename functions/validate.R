validate_row <- function(row, col_names) {
  # Add column names for easier manipulation of the data
  names(row) <- col_names
  row <- as.list(row)
  
  # Only EEG locations for supported layouts should be validated
  if (row$modality != "EEG") {
    return(list())
  }
  
  supported_layouts = names(ch_names)
  if (!(row$meeg_layout %in% supported_layouts)) {
    return(list())
  }
  
  errors <- list()
  
  # Channels selected for HEP analysis should also be present in EEG locations
  used_eeg_locations <- unlist(strsplit(row$eeg_locations, ", "))
  selected_eeg_channels <- unlist(strsplit(row$hep_eeg_channels_selected, ", "))
  significant_unknown <- row$significant_eeg_channels == "unknown"
  significant_eeg_channels <- unlist(strsplit(row$significant_eeg_channels, ", "))
  
  if (!all(selected_eeg_channels %in% used_eeg_locations)) {
    missing <- setdiff(selected_eeg_channels, used_eeg_locations)
    errors <- append(errors, list(list(
      "PMID" = row$PMID,
      "column" = "hep_eeg_channels_selected",
      "error" = "Not mentioned in the EEG locations",
      "failure_case" = paste(missing, collapse = ", ")
    )))
  }
  
  # NOTE: should also be true if the list of significant channels is empty
  all_mentioned <- all(significant_eeg_channels %in% used_eeg_locations)
  if (!significant_unknown & !all_mentioned) {
    missing <- setdiff(significant_eeg_channels, used_eeg_locations)
    errors <- append(errors, list(list(
      "PMID" = row$PMID,
      "column" = "significant_eeg_channels",
      "error" = "Not mentioned in the EEG locations",
      "failure_case" = paste(missing, collapse = ", ")
    )))
  }
  
  errors
}

validate_preprocessed <- function(df) {
  # validate the data after preprocessing, iterating over rows
  errors <- apply(X = df, MARGIN = 1, FUN = validate_row, 
                  col_names = colnames(df), simplify = F)
  errors <- unlist(errors, recursive = F)

  # Control mapping validation 
  control_mapping_errors <- validate_control_mapping(df, control_variable_synonyms)

  # Combine errors
  all_errors <- c(errors, control_mapping_errors) 
  
  # Convert to df
  errors_df <- dplyr::bind_rows(all_errors) 

  if (nrow(errors_df) == 0) { 
    message("validate_preprocessed: no errors")
  } else {
      warning("Found ", nrow(errors_df), " errors in the preprocessed data frame") 
      message(paste0("Number of errors by column:\n", paste(capture.output(table(errors_df$column, useNA = "ifany")), collapse = "\n")))
    }
  
  errors_df 
}

validate_control_mapping <- function(df, control_synonyms_map) {

  errors <- list()
  
  # Get all known mapped terms using the passed parameter
  known_main_terms <- tolower(names(control_synonyms_map))
  known_synonyms <- tolower(unlist(sapply(control_synonyms_map, function(x) x$synonyms, simplify = FALSE)))
  all_known_mapped_terms <- unique(c(known_main_terms, known_synonyms))
  
  for (i in 1:nrow(df)) {
    current_row <- df[i, ]
    pmid <- current_row$PMID
    controls_string <- current_row$controls

    # Skip if controls is NA or empty
    if (is.na(controls_string) || trimws(controls_string) == "") {
      next
    }

    controls_string_lower <- tolower(controls_string)
    
    # Parse the controls string
    row_terms_list <- strsplit(controls_string_lower, ",\\s*|;\\s*|\\s+and\\s+")
    row_terms_flat <- trimws(unlist(row_terms_list))
    
    # Filter out empty strings 
    row_terms_flat <- row_terms_flat[row_terms_flat != ""]
    
    # Find terms in this row that are not in all_known_mapped_terms
    unmapped_in_row <- setdiff(row_terms_flat, all_known_mapped_terms)

    for (term in unmapped_in_row) {
      errors <- append(errors, list(list(
        "PMID" = pmid,
        "column" = "controls",
        "error" = "Unmapped control term",
        "failure_case" = term 
      )))
    }
  }
  
  errors
}
