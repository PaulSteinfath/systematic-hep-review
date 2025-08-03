rejections_per_component_type <- function(df) {
  # Filter for ICA=1 and get distinct PMIDs with their rejected components
  df_ica <- df %>%
    filter(ICA == 1) %>%
    distinct(PMID, rejected_components) %>%
    filter(rejected_components != "", rejected_components != "unknown") %>%
    mutate(pipeline_id = row_number())  
  
  # Count pipelines before splitting
  n_rows <- nrow(df_ica)
  multiple_rows_per_paper <- any(table(df_ica$PMID) > 1)
  level <- if (multiple_rows_per_paper) "pipelines" else "studies"
  
  # Split the lists of component types
  df_long <- df_ica %>%
    separate_rows(rejected_components, sep = ", ") %>%
    mutate(
      rejected_components = trimws(rejected_components),
      rejected_components = tolower(rejected_components),
      # Map to standardized names
      rejected_components = allowed$ica_component_types[rejected_components] 
    )
  
  # Count the occurrences of each type
  counts_df <- df_long %>%
    group_by(rejected_components) %>%
    summarise(count = n_distinct(pipeline_id)) %>%
    arrange(desc(count)) %>%
    mutate(total = n_rows,
           percentage = count / n_rows,
           level = level)
  
  counts_df
}