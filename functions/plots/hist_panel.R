hist_panel <- function(df, col, group_col = 'PMID', discrete = F,
                       drop.na = T, force.numeric = F, allowed = NULL,
                       x.label = NULL, use.log10 = F, use.log2 = F, 
                       modality_filter = NULL, binwidth = NULL, tilt_labels = F,
                       use_proportion = TRUE, y_limits = NULL, custom_labels = NULL,
                       grouping_var = NULL, plot_fill = plot_fill_default) {
  
  # Filter for EEG modality if specified
  if (!is.null(modality_filter)) {
    df <- df %>% filter(Modality == modality_filter)
  }
  
  # Use distinct rows based on group_col and col; if a grouping variable is provided include it.
  if(!is.null(grouping_var)){
    df_distinct <- distinct(df, !!sym(group_col), !!sym(col), !!sym(grouping_var))
  } else {
    df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
  }
  
  # Warn if there are multiple rows per paper for the specified column
  if(nrow(df) != nrow(df_distinct)) {
    warning(paste("Warning: Multiple rows detected per", group_col, "for column", col))
  }
  
  if (force.numeric) {
    # Convert to numeric (this may introduce NAs which are dropped below)
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
  
  # Plotting
  if (discrete) {
    
    # Print unique values for col (and grouping_var if provided)
    message(paste(col, paste(unique(df_distinct[[col]]), collapse = ", "), sep = ": "))
    if (!is.null(grouping_var)) {
      message(paste(grouping_var, paste(unique(df_distinct[[grouping_var]]), collapse = ", "), sep = ": "))
    }
    
    # Compute counts – if grouping_var is provided, count by both variables
    if (!is.null(grouping_var)) {
      counts_df <- df_distinct %>%
        count(!!sym(col), !!sym(grouping_var)) %>%
        arrange(desc(n)) %>%
        group_by(!!sym(col)) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()
    } else {
      counts_df <- df_distinct %>%
        count(!!sym(col)) %>%
        arrange(desc(n)) %>%
        mutate(prop = n / sum(n))
    }
    
    # Apply custom labels to the target variable if provided
    if (!is.null(custom_labels)) {
      counts_df[[col]] <- factor(counts_df[[col]], 
                                 levels = seq_along(custom_labels) - 1,
                                 labels = custom_labels)
    }
    
    # Create the bar plot
    if (!is.null(grouping_var)) {
      p <- ggplot(counts_df, aes(x = factor(!!sym(col)), 
                                 y = if(use_proportion) prop else n, 
                                 fill = !!sym(grouping_var))) +
        geom_bar(stat = "identity", position = "dodge", color = 'white', linewidth = 0.5) +
        scale_fill_manual(values = plot_fill, name = "")
    } else {
      p <- ggplot(counts_df, aes(x = reorder(!!sym(col), n, decreasing = TRUE), 
                                 y = if(use_proportion) prop else n)) +
        geom_bar(stat = "identity", fill = plot_fill[1], color = 'white', linewidth = 0.5) 
    }
    
    if(use_proportion) {
      p <- p + scale_y_continuous(labels = scales::percent,
                                  expand = expansion(mult = c(0, .1)),
                                  limits = y_limits)
    }
    
  } else {
    # Continuous variable: Plot a histogram
    if (!is.null(grouping_var)) {
      p <- ggplot(df_distinct, aes(x = !!sym(col), fill = !!sym(grouping_var))) +
        geom_histogram(color = 'white', linewidth = 0.5, binwidth = binwidth, position = "dodge") +
        scale_fill_manual(values = plot_fill_default, name = "")
    } else {
      p <- ggplot(df_distinct, aes(x = !!sym(col))) +
        geom_histogram(fill = plot_fill_default[1], color = 'white', linewidth = 0.5, binwidth = binwidth) 
    }
  }
  
  # Determine total number of distinct studies (or pipelines)
  n_total <- nrow(df_distinct)
  label_type <- if(nrow(distinct(df, !!sym(group_col))) == n_total) "Studies" else "Pipelines"
  
  p <- p +
    labs(title = paste("n =", n_total, tolower(label_type))) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(size = 8, 
                                 angle = if (tilt_labels) 45 else 0, 
                                 hjust = if (tilt_labels) 1 else 0.5),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.title.y = element_text(size = 9)
    )
  
  if (!is.null(x.label)) {
    p <- p + xlab(x.label)
  }
  
  if (discrete && use_proportion) { 
    p <- p + ylab(paste("Proportion of", label_type))
  } else {
    p <- p + ylab(paste("Number of", label_type))
  }
  
  if (use.log10) {
    p <- p + scale_x_log10()
  }
  
  if (use.log2) {
    p <- p + scale_x_continuous(trans = "log2")
  }
  
  return(p)
}
