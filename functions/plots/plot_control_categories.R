plot_control_categories <- function(df, tilt_labels = TRUE) {
  ## 0. Look-ups 
  control_variable_synonyms <- get_control_variable_mappings()
  
  ## 1. Pre-processing 
  df_unique    <- dplyr::distinct(df, PMID, controls)
  df_controls  <- ifelse(is.na(df_unique$controls), "", df_unique$controls)
  
  # accumulate patterns per category
  category_patterns <- list()
  for (nm in names(control_variable_synonyms)) {
    meta <- control_variable_synonyms[[nm]]
    category_patterns[[meta$category]] <-
      c(category_patterns[[meta$category]], nm, meta$synonyms)
  }
  categories <- names(category_patterns)
  
  # TRUE/FALSE presence matrix
  category_presence <- matrix(
    FALSE,
    nrow = nrow(df_unique),
    ncol = length(categories),
    dimnames = list(NULL, categories)
  )
  for (cat in categories) {
    pat <- paste0("\\b(", paste(unique(category_patterns[[cat]]), collapse = "|"), ")\\b")
    category_presence[, cat] <- stringr::str_detect(
      tolower(df_controls),
      tolower(pat)
    )
  }
  
  ## 2. Long data frame for hist_panel() 
  long_df <- category_presence %>%
    as.data.frame() %>%
    dplyr::mutate(PMID = df_unique$PMID) %>%
    tidyr::pivot_longer(
      cols      = categories,
      names_to  = "category",
      values_to = "present"
    ) %>%
    dplyr::filter(present) %>%          # keep only matches
    dplyr::select(PMID, category)
  
  ## 3. Plot  – one uniform colour, no fill aesthetic
  p <- hist_panel(
    long_df,
    col               = "category",
    group_col         = "PMID",
    discrete          = TRUE,
    use_proportion    = TRUE,
    x.label           = "Control category",
    fill_as_aesthetic = FALSE,  
    tilt_labels       = tilt_labels
  ) + coord_flip()
  
  return(p)
}
