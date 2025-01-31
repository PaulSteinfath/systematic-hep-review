hist_panel <- function(df, col, group_col = 'PMID', discrete = F,
                       drop.na = T, force.numeric = F, allowed = NULL,
                       x.label = NULL, use.log10 = F, use.log2 = F, modality_filter = NULL, binwidth = NULL, tilt_labels = F) {
  
  # Filter for EEG modality if specified
  if (!is.null(modality_filter)) {
    df <- df %>% filter(Modality == modality_filter)
  }
  
  # Get unique (group_col, col) combinations to avoid overestimating the weight
  # of papers with multiple rows
  df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
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
    message(paste(col, unique(df_distinct[[col]])))
    
    p <- ggplot(df_distinct, aes(x = !!sym(col))) +
      geom_bar(fill = '#696969', color = 'white', linewidth = 0.5) +
      theme_classic(base_family = "sans")
  } else {
    p <- ggplot(df_distinct, aes(x = !!sym(col))) +
      geom_histogram(fill = '#696969', color = 'white', linewidth = 0.5, binwidth = binwidth) +
      theme_classic(base_family = "sans")
  }
  
  p <- p +
    labs(title = paste('n =', nrow(distinct(df_distinct, !!sym(group_col))))) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(
      title = element_text(size = 8),
      axis.text.x = element_text(angle = if (tilt_labels) 45 else 0, hjust = if (tilt_labels) 1 else 0.5),
      axis.title.x = element_text(margin = margin(t = 1))  # Adjust this value to move label closer
    )
  
  
  if (!is.null(x.label)) {
    p <- p + xlab(x.label)
  }
  
  if (use.log10) {
    p <- p + scale_x_log10()
  }
  
  if (use.log2) {
    p <- p + scale_x_continuous(transform = 'log2')
  }
  
   return(p)
}
