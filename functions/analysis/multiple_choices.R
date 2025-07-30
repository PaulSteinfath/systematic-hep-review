multiple_choices <- function(df, variables) {
  total_studies <- dplyr::n_distinct(df$PMID)
  
  results_df <- purrr::map_dfr(variables, function(var) {
    df %>%
      dplyr::group_by(PMID) %>%
      dplyr::summarise(unique_choices = dplyr::n_distinct(.data[[var]]), .groups = "drop") %>%
      dplyr::summarise(count = sum(unique_choices > 1)) %>%
      dplyr::mutate(Column = var,
                    total = total_studies,
                    percentage = count / total_studies)
  })
  
  results_df
}