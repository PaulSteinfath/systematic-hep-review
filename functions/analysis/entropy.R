# Helper function to compute Shannon entropy from a frequency table.
calc_entropy <- function(freq_table) {
  p <- freq_table / sum(freq_table)
  H <- -sum(p * log2(p))
  return(H)
}

# Helper function for categorical (or character) variables.
entropy_categorical <- function(x, norm = TRUE) {
  freq <- table(x)
  H <- calc_entropy(freq)
  if (norm) {
    H_max <- log2(length(freq))
    return(if (H_max == 0) 0 else H / H_max)
  } else {
    return(H)
  }
}

# Helper function for numeric variables.
# It checks for low unique counts (and converts to character if so)
# Otherwise, it discretizes the variable using quantile-based binning.
entropy_numeric <- function(x, unique_threshold = 10, num_bins = 10, norm = TRUE) {
  # If there are few unique values, treat as categorical.
  if (length(unique(x)) <= unique_threshold) {
    return(entropy_categorical(as.character(x), norm = norm))
  } else {
    effective_bins <- num_bins
    repeat {
      breaks <- quantile(x, probs = seq(0, 1, length.out = effective_bins + 1), na.rm = TRUE)
      # Remove duplicate breakpoints.
      breaks <- unique(breaks)
      if (length(breaks) > 1 || effective_bins < 2) break
      effective_bins <- effective_bins - 1
    }
    
    # If we could not get at least 2 unique breaks, fall back to categorical treatment.
    if (length(breaks) < 2 || effective_bins < 2) {
      return(entropy_categorical(as.character(x), norm = norm))
    } else {
      # Discretize x into bins.
      binned <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
      return(entropy_categorical(binned, norm = norm))
    }
  }
}

reduce_by_pmid <- function(x, pmid) {
  # Remove NA values first.
  valid <- !is.na(x)
  x <- x[valid]
  pmid <- pmid[valid]
  # Remove duplicates where both PMID and x are identical.
  unique_rows <- !duplicated(data.frame(PMID = pmid, Value = x, stringsAsFactors = FALSE))
  return(x[unique_rows])
}

# Main function: computes entropy for each column in method_columns.
# Default behavior discretizes numeric variables and returns normalized entropy.
# Setting all_char = TRUE forces all columns to be treated as categorical.
compute_entropy <- function(data, method_columns, 
                            num_bins = 10, 
                            unique_threshold = 10, 
                            all_char = FALSE, 
                            norm = TRUE, 
                            drop_paper_duplicates = TRUE) {
  result <- list()
  pmid <- data[["PMID"]]
  
  for (col in method_columns) {
    x <- data[[col]]
    
    # Only use EEG studies for reference columns
    pmid <- data$PMID
    if (tolower(col) %in% c("reference_online", "reference_offline")) {
      x <- x[data$modality == "EEG"]
      pmid <- data$PMID[data$modality == "EEG"]
    }
    
    if (drop_paper_duplicates){
      x <- reduce_by_pmid(x, pmid)
    }
    
    # Remove NA values.
    x <- x[!is.na(x)]
    
    # If no values remain, set result as NA.
    if (length(x) == 0) {
      warning("No values remain after preprocessing for ", col)
      result[[col]] <- NA
      next
    }
    
    # If all_char is TRUE, convert everything to character.
    if (all_char || is.factor(x) || is.character(x)) {
      # Convert to character and process comma-separated tokens
      x <- as.character(x)
      tokens <- process_char_vector(x)
      if (length(tokens) == 0) {
        result[[col]] <- NA
      } else {
        result[[col]] <- entropy_categorical(tokens, norm = norm)
      }
    } else if (is.numeric(x)) {
      result[[col]] <- entropy_numeric(x, unique_threshold = unique_threshold, 
                                       num_bins = num_bins, norm = norm)
    } else {
      warning(paste("Column", col, "has unsupported type; skipping."))
      result[[col]] <- NA
    }
  }
  
  # Return the results as a data frame.
  res_df <- data.frame(Column = names(result),
                       Entropy = unlist(result),
                       stringsAsFactors = FALSE)
  
  return(res_df)
}
