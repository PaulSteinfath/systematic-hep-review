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
  pmid <- data$PMID
  
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

plot_entropy <- function(entropy_df,
                                column_mapping_readable = column_mapping_readable_default,
                                pipeline_steps = NULL,
                                pipeline_colors = NULL,
                                x_lab = "Methodological Choice",
                                y_lab = "Entropy",
                                plot_fill = plot_fill_default_single,
                                show_wordy_title = FALSE,
                                show_title = FALSE,
                                show_legend = FALSE,
                                tilt_labels = FALSE,
                                x_ticks = TRUE,
                                y_ticks = TRUE,
                                flip = FALSE,
                                fixed = FALSE) {
  
  # Apply readable mapping to Column names
  entropy_df$Column <- apply_column_mapping(entropy_df$Column, column_mapping_readable)
  
  # For provided entropy_df, preserve the existing order (it's already sorted in figures.R)
  if (fixed) {
    entropy_df$Column <- factor(entropy_df$Column, levels = unique(entropy_df$Column))
  }
  
  my_title <- if (!show_title) {
    NULL
  } else if (show_wordy_title) {
    "Entropy" 
  } else {
    paste("n =", dplyr::n_distinct(df$PMID), "studies")
  }
  
  p <- ggplot(entropy_df, aes(x = Column, y = Entropy)) +
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
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  if (!is.null(pipeline_colors)) {
    p <- p + geom_bar(aes(fill = Step), stat = "identity", color = "white", linewidth = 0.5) +
      scale_fill_manual(values = pipeline_colors, guide = if (show_legend) "legend" else "none") +
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