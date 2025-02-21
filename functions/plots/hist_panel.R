hist_panel <- function(df, col, group_col = 'PMID', discrete = F,
                       drop.na = T, force.numeric = F, allowed = NULL,
                       x.label = NULL, use.log10 = F, use.log2 = F, 
                       modality_filter = NULL, binwidth = NULL, tilt_labels = F,
                       use_proportion = TRUE, y_limits = NULL, custom_labels = NULL) {  # Added custom_labels
  
  # Filter for EEG modality if specified
  if (!is.null(modality_filter)) {
    df <- df %>% filter(Modality == modality_filter)
  }
  
  # Get unique (group_col, col) combinations to avoid overestimating the weight
  # of papers with multiple rows
  df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
  
  # Check if there are multiple rows per paper for the specified column
  if(nrow(df) != nrow(df_distinct)) {
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
    
    #print unique values
    message(paste(col, paste(unique(df_distinct[[col]]), collapse = ", "), sep = ": "))

    # Get counts and calculate proportions
    counts_df <- df_distinct %>%
      count(!!sym(col)) %>%
      arrange(desc(n)) %>%  # Sort by frequency
      mutate(prop = n / sum(n))  # Calculate proportions
    
    # Apply custom labels if provided
    if (!is.null(custom_labels)) {
      counts_df[[col]] <- factor(counts_df[[col]], 
                                levels = seq_along(custom_labels) - 1,
                                labels = custom_labels)
    }
    
    p <- ggplot(counts_df, aes(x = reorder(!!sym(col), n, decreasing = TRUE), 
                              y = if(use_proportion) prop else n)) +
      geom_bar(stat = "identity", fill = '#696969', color = 'white', linewidth = 0.5) +
      theme_classic(base_family = "sans")
      
    if(use_proportion) {
      p <- p + scale_y_continuous(labels = scales::percent,
                                expand = expansion(mult = c(0, .1)),
                                limits = y_limits)  # Apply y-axis limits if provided
    }
  } else {
    p <- ggplot(df_distinct, aes(x = !!sym(col))) +
      geom_histogram(fill = '#696969', color = 'white', linewidth = 0.5, binwidth = binwidth) +
      theme_classic(base_family = "sans")
  }
  
  # Get label type once for both title and y-axis
  n_total <- nrow(distinct(df, !!sym(group_col)))
  label_type <- if(nrow(df) == n_total) "Studies" else "Pipelines"
  
  p <- p +
    labs(title = paste("n =", n_total, tolower(label_type))) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(size = 8, 
                                 angle = if (tilt_labels) 45 else 0, 
                                 hjust = if (tilt_labels) 1 else 0.5),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, 
                                  margin = margin(t = 4)),  # Adjust this value to move label closer
      axis.title.y = element_text(size = 9)
    )
  
  if (!is.null(x.label)) {
    p <- p + xlab(x.label)
  }
  
  # Update y-axis label to use the same label_type
  if (discrete && use_proportion) {
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
