cfa_criteria_counts <- function(df, mapping = NULL, group_col = "PMID") {
  df_filt <- df %>%
    filter(reject_cfa_ics) %>%
    mutate(cfa_rej_criteria = tolower(cfa_rej_criteria)) %>%
    distinct(PMID, cfa_rej_criteria) %>%
    mutate(pipeline_id = row_number())
  
  total_count <- nrow(df_filt)
  multiple_rows_per_group <- any(table(df_filt[[group_col]]) > 1)
  level <- if (multiple_rows_per_group) "pipelines" else "studies"
  
  criteria_expanded <- df_filt %>%
    separate_rows(cfa_rej_criteria, sep = ", ") %>%
    mutate(cfa_rej_criteria = str_trim(cfa_rej_criteria, side = "both"))
  if (!is.null(mapping)) {
    criteria_expanded <- criteria_expanded %>%
      mutate(
        cfa_rej_criteria = case_when(
          tolower(cfa_rej_criteria) %in% names(mapping) ~ mapping[tolower(cfa_rej_criteria)],
          TRUE ~ NA
        )
      )
    
    if (any(is.na(criteria_expanded$cfa_rej_criteria))) {
      message("cfa_criteria: some criteria are missing from the provided mapping, dropping")
      criteria_expanded <- criteria_expanded %>%
        filter(!is.na(cfa_rej_criteria))
    }
  } 
  
  criteria_expanded <- criteria_expanded %>%
    distinct(pipeline_id, cfa_rej_criteria, .keep_all = TRUE)
  
  counts <- criteria_expanded %>%
    group_by(cfa_rej_criteria) %>%
    summarise(count = n_distinct(pipeline_id)) %>%
    arrange(desc(count)) %>%
    mutate(total = total_count,
           level = level,
           percentage = count / total_count)
  
  counts
}