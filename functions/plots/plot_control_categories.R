plot_control_categories <- function(df, tilt_labels = FALSE) {
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
  
  ## 3. Plot – with fill aesthetic to allow individual colors per category
  allowed_mapping <- c("ECG and Heartbeat-Related Controls" = "ECG and Heartbeat-\nRelated Controls",
                      "Heart Rate Variability (HRV) Controls" = "Heart Rate Variability\n(HRV) Controls",
                      "Cardiovascular and Blood Pressure Controls" = "Cardiovascular and\nBlood Pressure Controls",
                      "Respiration" = "Respiration",
                      "Demographic and Psychosocial Controls" = "Demographic and\nPsychosocial Controls",
                      "Physiological and Environmental Controls" = "Physiological and\nEnvironmental Controls",
                      "Task and Experimental Controls" = "Task and\nExperimental Controls",
                      "Other Controls" = "Other Controls")
  
  p <- hist_panel(
    long_df,
    col               = "category",
    group_col         = "PMID",
    discrete          = TRUE,
    use_proportion    = TRUE,
    x.label           = "",
    title           = "Control Categories",
    fill_as_aesthetic = TRUE,  
    tilt_labels       = tilt_labels,
    allowed           = allowed_mapping,
    decreasing         = FALSE
  ) + coord_flip()
  
  category_colors <- c()
  for (cat in unique(long_df$category)) {
    transformed_name <- allowed_mapping[cat]
    if (cat == "ECG and Heartbeat-Related Controls") {
      category_colors[transformed_name] <- "#647499ff"
    } else {
      category_colors[transformed_name] <- "#696969"
    }
  }


  p <- p + scale_fill_manual(values = category_colors, guide = "none")
  
  return(p)
}
