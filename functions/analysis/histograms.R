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
  df_distinct <- df
  level <- "pipelines"
  if (!is.null(group_col)) {
    df_distinct <- distinct(df, !!sym(group_col), !!sym(col))
    multiple_rows_per_group <- any(table(df_distinct[[group_col]]) > 1)
    if (!multiple_rows_per_group) {
      level <- "studies"
    }
  }
  
  # Restrict allowed values
  if (!is.null(allowed)) {
    df_distinct[[col]] <- tolower(df_distinct[[col]])
    names(allowed) <- lapply(names(allowed), tolower)
    df_distinct <- filter(df_distinct, is.element(!!sym(col), names(allowed)))
    df_distinct[[col]] <- allowed[df_distinct[[col]]]
  }
  
  total_count <- nrow(df_distinct)
  
  df_counts <- df_distinct %>% 
    count(!!sym(col)) %>%
    arrange(desc(n)) %>%
    mutate(
      total = total_count,
      percentage = n / total_count * 100,
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
           percentage = count / total * 100) %>%
    select({{col}}, count, total, percentage, level)
  
  hist_df
}


# For 0/1 columns, get the count and percentage of studies with 1
# At least one pipeline with 1 per study is needed
get_usage_studies <- function(df, cols, group_col = "PMID") {
  usage <- data.frame()
  
  for (col in cols) {
    is_using <- by(df[[col]], df[[group_col]], \(x) any(x), simplify = T)
    
    col_usage <- data.frame(
      column = col,
      count = sum(is_using),
      total = length(is_using),
      level = "studies"
    )
    col_usage$percentage = col_usage$count / col_usage$total * 100
    
    usage <- bind_rows(usage, col_usage)
  }
  
  usage
}


# For 0/1 columns, get the count and percentage of pipelines with 1
# Optionally, with filtering to only consider distinct pipelines 
get_usage_pipelines <- function(df, cols, group_col = "PMID", distinct = F) {
  usage <- data.frame()
  
  for (col in cols) {
    is_using <- df[[col]]
    if (distinct) {
      df_distinct <- df %>%
        distinct(!!sym(group_col), !!sym(col))
      is_using <- df_distinct[[col]]
    }
    
    col_usage <- data.frame(
      column = col,
      count = sum(is_using),
      total = length(is_using),
      level = "pipelines"
    )
    col_usage$percentage = col_usage$count / col_usage$total * 100
    
    usage <- bind_rows(usage, col_usage)
  }
  
  usage
}