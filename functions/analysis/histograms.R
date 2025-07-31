hist_discrete <- function(df, 
                          col, 
                          group_col = 'PMID', 
                          allowed = NULL, 
                          modality_filter = NULL) {
  # Filter for EEG modality if specified
  if (!is.null(modality_filter)) {
    df <- df %>% filter(modality == modality_filter)
  }
  
  # Get unique (group_col, col) combinations to avoid overestimating the weight
  # of papers with multiple rows
  df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
  
  # Restrict allowed values
  if (!is.null(allowed)) {
    df_distinct[[col]] <- tolower(df_distinct[[col]])
    names(allowed) <- lapply(names(allowed), tolower)
    df_distinct <- filter(df_distinct, is.element(!!sym(col), names(allowed)))
    df_distinct[[col]] <- allowed[df_distinct[[col]]]
  }
  
  multiple_rows_per_group <- any(table(df_distinct[[group_col]]) > 1)
  level <- if (multiple_rows_per_group) "pipelines" else "studies"
  total_count <- nrow(df_distinct)
  
  df_counts <- df_distinct %>% 
    count(!!sym(col)) %>%
    arrange(desc(n)) %>%
    mutate(
      total = total_count,
      percentage = round(n / total_count * 100, 1),
      level = level
    )
  
  df_counts
}


hist_continuous <- function(df,
                            col,
                            group_col = "PMID",
                            binwidth = NULL,
                            force.numeric = F,
                            modality_filter = NULL) {
  p <- hist_panel(df, col, force.numeric = force.numeric,
                  binwidth = binwidth, modality_filter = modality_filter)
  pg <- ggplot_build(p)
  
  # Use provided data to determine studies / pipelines
  df_plot <- p$data
  multiple_rows_per_group <- any(table(df_plot[[group_col]]) > 1)
  level <- if (multiple_rows_per_group) "pipelines" else "studies"
  total_count <- nrow(df_plot)
  
  # Use processed data to extract the histogram
  hist_df <- pg$data[[1]] %>%
    mutate({{col}} := x,
           total = total_count,
           level = level,
           percentage = round(count / total * 100, 1)) %>%
    select({{col}}, count, total, percentage, level)
  
  hist_df
}