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
  used_locations <- unlist(strsplit(row$eeg_locations, ", "))
  selected_channels <- unlist(strsplit(row$hep_channels_selected, ", "))
  significant_unknown <- row$significant_channels == "unknown"
  significant_channels <- unlist(strsplit(row$significant_channels, ", "))
  
  if (!all(selected_channels %in% used_locations)) {
    missing <- setdiff(selected_channels, used_locations)
    errors <- append(errors, list(list(
      "PMID" = row$PMID,
      "column" = "hep_channels_selected",
      "error" = "Not mentioned in the EEG locations",
      "failure_case" = paste(missing, collapse = ", ")
    )))
  }
  
  # NOTE: should also be true if the list of significant channels is empty
  all_mentioned <- all(significant_channels %in% used_locations)
  if (!significant_unknown & !all_mentioned) {
    missing <- setdiff(significant_channels, used_locations)
    errors <- append(errors, list(list(
      "PMID" = row$PMID,
      "column" = "significant_channels",
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
  errors <- as.data.frame(do.call(rbind, errors))
  
  if (nrow(errors) == 0) {
    message("validate_preprocessed: no errors")
  }
  
  warning("Found ", nrow(errors), " errors in the preprocessed data frame")
  message(paste0("Number of errors by column:\n", table(errors$column)))
  
  errors
}
