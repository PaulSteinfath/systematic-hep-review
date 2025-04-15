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
                         pipeline_steps,
                         pipeline_colors,
                         column_mapping_readable = column_mapping_readable_default,
                         plot_theme = plot_theme_default,
                         align_by_magnitude = FALSE,
                         gap = 0.5,
                         x_lab = "Variable",
                         y_lab = "Entropy",
                         plot_title = "Entropy by Variable",
                         x_ticks = TRUE,
                         vertical = FALSE) {
  df_entropy <- compute_entropy(df, method_columns = method_columns,
                               all_char = FALSE, norm = TRUE,
                               drop_paper_duplicates = TRUE)
  colnames(df_entropy) <- c("Column", "Entropy")

  # Assign each variable a pipeline step
  df_entropy$Step <- sapply(df_entropy$Column, get_pipeline_step)
  
  # Factor levels for pipeline steps
  step_order <- names(pipeline_steps)
  df_entropy$Step <- factor(df_entropy$Step, levels = step_order)

  # Sort by step, then within step by ascending Entropy
  df_entropy <- df_entropy %>%
    arrange(Step, Entropy)

  # Apply column mapping
  df_entropy$Column <- apply_column_mapping(df_entropy$Column, column_mapping_readable)

  # Convert Column to factor with new sorted order
  df_entropy$Column <- factor(df_entropy$Column, levels = df_entropy$Column)

  # Build the plot
  p <- ggplot(df_entropy, aes(x = Column, y = Entropy, fill = Step)) +
       geom_bar(stat = "identity") +
       scale_fill_manual(values = pipeline_colors) +
       labs(x = x_lab, y = y_lab, title = plot_title) +
       plot_theme

  # Option: remove the legend from the entropy plot.
  p <- p + theme(legend.position = "none")

  # Control x-axis
  if (!x_ticks) {
    p <- p + theme(axis.text.x = element_blank())
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Flip if vertical
  if (vertical) {
    p <- p + coord_flip()
  }

  return(p)
}
