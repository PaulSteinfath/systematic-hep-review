hist_panel <- function(df, col, group_col = 'PMID', title = NULL, discrete = F,
                       drop.na = T, force.numeric = F, allowed = NULL,
                       x.label = NULL, use.log10 = F, use.log2 = F,
                       fill_as_aesthetic = F,
                       modality_filter = NULL, binwidth = NULL, bins = NULL, tilt_labels = F,
                       use_proportion = TRUE, y_limits = NULL, custom_labels = NULL,
                       preserve_order = FALSE, decreasing = TRUE) { 
  
  # Filter for EEG modality if specified
  if (!is.null(modality_filter)) {
    df <- df %>% filter(modality == modality_filter)
  }
  
  # Get unique (group_col, col) combinations to avoid overestimating the weight
  # of papers with multiple rows
  df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
  multiple_rows_per_group <- any(table(df_distinct[[group_col]]) > 1)
  
  # Check if there are multiple rows per paper for the specified column
  if (multiple_rows_per_group) {
    warning(paste("Warning: Multiple rows detected per", group_col, "for column", col))
  }
  
  if (force.numeric) {
    # Place before drop_na so that all failed conversions (NAs) are removed
    df_distinct[[col]] <- as.numeric(df_distinct[[col]]) 
  }
  if (drop.na) {
    df_distinct <- drop_na(df_distinct, !!sym(col))
  }
  if (!is.null(allowed)) {
    df_distinct[[col]] <- tolower(df_distinct[[col]])
    names(allowed) <- lapply(names(allowed), tolower)
    df_distinct <- filter(df_distinct, is.element(!!sym(col), names(allowed)))
    df_distinct[[col]] <- allowed[df_distinct[[col]]]
  }
  
  # Plot the histogram
  if (discrete) {
    
    # Print unique values while replacing new lines with spaces
    unique_values <- paste(unique(df_distinct[[col]]), collapse = ", ")
    unique_values <- gsub("\n", " ", unique_values)
    message(paste(col, unique_values, sep = ": "))

    # Get counts and calculate proportions
    counts_df <- df_distinct %>%
      count(!!sym(col)) %>%
      arrange(desc(n)) %>%  # Sort by frequency
      mutate(prop = n / sum(n))  # Calculate proportions

    # Apply custom labels if provided
    if (!is.null(custom_labels)) {
      counts_df[[col]] <- factor(counts_df[[col]], 
                                 levels = names(custom_labels),
                                 labels = custom_labels)
    }
    
    # Determine x-axis mapping based on preserve_order
    if (preserve_order && is.factor(df_distinct[[col]])) {
      p <- ggplot(counts_df, aes(x = !!sym(col), 
                                 y = if(use_proportion) prop else n))
    } else {
      # Default - reorder by frequency
      p <- ggplot(counts_df, aes(x = reorder(!!sym(col), n, decreasing = decreasing), 
                                 y = if(use_proportion) prop else n))
    }
    
    if (fill_as_aesthetic) {
      p <- p +
        geom_bar(aes(fill = !!sym(col)), stat = "identity", 
                 color = 'white', linewidth = 0.5)
    } else {
      p <- p + 
        geom_bar(stat = "identity", fill = common_colors$fill_default, 
                 color = 'white', linewidth = 0.5)
    }
    
    p <- p + theme_classic(base_family = "sans")
  } else {
    p <- ggplot(df_distinct, aes(x = !!sym(col))) +
      geom_histogram(aes(
        y = if (use_proportion) 
          after_stat(count / sum(count))
        else 
          after_stat(count)
        ),
        fill = common_colors$fill_default, 
        color = 'white', 
        linewidth = 0.5, 
        binwidth = binwidth, 
        bins = bins) +
      theme_classic(base_family = "sans")
  }
  
  # Get label type once for both title and y-axis
  n_total <- nrow(df_distinct)
  label_type <- if (multiple_rows_per_group) "pipelines" else "studies"
  
  # Set a custom title if provided
  datapoint_count <- paste("n =", n_total, tolower(label_type))
  if (!is.null(title)) {
    p <- p +
      labs(title = title, subtitle = datapoint_count)
  } else {
    p <- p +
      labs(title = datapoint_count)
  }
  
  p <- p +
    plot_theme_default +
    theme(
      axis.text.x = element_text(angle = if (tilt_labels) 45 else 0, 
                                 hjust = if (tilt_labels) 1 else 0.5),
    )
  
  if (!is.null(x.label)) {
    p <- p + xlab(x.label)
  } else {
    p <- p + theme(axis.title.x = element_blank())
  }
  
  # Add percent signs to labels, remove empty space below the bars, apply y-axis
  # limits if provided
  p <- p + scale_y_continuous(labels = if (use_proportion) scales::percent else waiver(),
                              expand = expansion(mult = c(0, .1)),
                              limits = y_limits)
  
  # Update y-axis label to use the same label_type
  if (use_proportion) {
    p <- p + ylab(paste("Proportion of", label_type))
  } else {
    p <- p + ylab(paste("Number of", label_type))
  }
  
  if (use.log10) {
    p <- p + scale_x_log10()
  }
  
  if (use.log2) {
    p <- p + scale_x_continuous(transform = 'log2')
  }
  
   return(p)
}
