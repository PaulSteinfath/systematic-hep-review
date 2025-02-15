entropy_columns_default <- c(
  "sample_size",   
  "channels", 
  "length_min", "ecg_num_electrodes", "ecg_lead", 
  "ecg_locations", "ecg_ground", "reference_online", 
  "reference_offline", "high_pass", "low_pass", 
  "ICA", "ica_on_epochs", "rejected_components", 
  "rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria", 
  "hep_channels_selected", 
  "hep_relative_to", "hep_start", 
  "hep_end", "baseline_start_ms", "baseline_end_ms", 
  "value", "averaging_channels", 
  "averaging_time", "clustering"
)

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

process_char_vector <- function(x) {
  # Remove elements that are empty or "unknown" (after trimming and converting to lower case)
  x_clean <- x[!(trimws(tolower(x)) %in% c("", "unknown"))]
  if (length(x_clean) == 0) return(character(0))
  
  # Split each remaining element by comma
  tokens_list <- strsplit(x_clean, ",")
  # Trim spaces from each token and combine into one vector
  tokens <- unlist(lapply(tokens_list, function(vec) trimws(vec)))
  # Remove any tokens that are empty or "unknown"
  tokens <- tokens[!(tokens == "" | tolower(tokens) == "unknown")]
  return(tokens)
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
    
    if (drop_paper_duplicates){
      x <- reduce_by_pmid(x, pmid)
    }
    
    # Remove NA values.
    x <- x[!is.na(x)]
    
    # If no values remain, set result as NA.
    if (length(x) == 0) {
      result[[col]] <- NA
      next
    }

    # If all_char is TRUE, convert everything to character.
    if (all_char) {
      x <- as.character(x)
      tokens <- process_char_vector(x)
      if (length(tokens) == 0) {
        result[[col]] <- NA
      } else {
        result[[col]] <- entropy_categorical(tokens, norm = norm)
      }
      next
    }
    
    # Process by type:
    if (is.factor(x) || is.character(x)) {
      # Convert to character and process comma-separated tokens.
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

plot_entropy <- function(df, 
                         method_columns = entropy_columns_default, 
                         column_mapping_readable = column_mapping_readable_default, 
                         group_var = NULL,
                         vertical = FALSE, 
                         num_bins = 10, 
                         unique_threshold = 10, 
                         all_char = FALSE, 
                         norm = TRUE, 
                         plot_fill = plot_fill_default,
                         plot_theme = plot_theme_default,
                         drop_paper_duplicates = TRUE,
                         gap = 0.5, 
                         group_bar_pos = "dodge",
                         show_wordy_title = FALSE) {
  
  # Flatten method_columns if needed.
  if (is.list(method_columns)) {
    flat_methods <- unlist(method_columns)
  } else {
    flat_methods <- method_columns
  }
  
  # Compute entropy results.
  if (is.null(group_var)) {
    entropy_results <- compute_entropy(
      data = df, 
      method_columns = flat_methods, 
      num_bins = num_bins, 
      unique_threshold = unique_threshold, 
      all_char = all_char, 
      norm = norm,
      drop_paper_duplicates = drop_paper_duplicates
    )
  } else {
    if (!(group_var %in% names(df))) {
      stop(paste("Grouping variable", group_var, "not found in data."))
    }
    groups <- unique(df[[group_var]])
    results_list <- lapply(groups, function(g) {
      dfg <- df[df[[group_var]] == g, ]
      entropy_res <- compute_entropy(
        data = dfg, 
        method_columns = flat_methods, 
        num_bins = num_bins, 
        unique_threshold = unique_threshold, 
        all_char = all_char, 
        norm = norm,
        drop_paper_duplicates = drop_paper_duplicates
      )
      entropy_res$Group <- g
      return(entropy_res)
    })
    entropy_results <- do.call(rbind, results_list)
    entropy_results$Group <- factor(entropy_results$Group)
  }
  
  entropy_results$Column <- apply_column_mapping(entropy_results$Column, column_mapping_readable)
  
  if (show_wordy_title){
    my_title <- if (is.null(group_var)) 
      "Entropy of Methodological Choices" 
    else 
      "Entropy of Methodological Choices by Group"
  } else{
    n_unique_papers <- n_distinct(df$PMID)
    my_title <- paste("n =", n_unique_papers)
  }

  p <- column_barplot(results_df = entropy_results, 
                      x_col = "Column", 
                      y_col = "Entropy",
                      variables = method_columns,
                      vertical = vertical,
                      group_var = if (is.null(group_var)) NULL else "Group",
                      align_by_magnitude = TRUE,
                      gap = gap,
                      x_lab = "Methodological Choice",
                      y_lab = "Entropy",
                      plot_title = my_title,
                      plot_fill = plot_fill,
                      plot_theme = plot_theme,
                      column_mapping_readable = column_mapping_readable,
                      group_bar_pos = group_bar_pos)
  return(p)
}
