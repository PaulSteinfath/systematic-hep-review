plot_ecg_controls <- function(df, tilt_labels = TRUE) {
  ## 0. Look-ups
  control_variable_synonyms <- get_control_variable_mappings()
  target_cat <- "ECG and Heartbeat-Related Controls"
  
  ## 1. Identify all variables that belong to the target category 
  ecg_vars <- names(control_variable_synonyms)[
    vapply(
      control_variable_synonyms,
      function(meta) identical(meta$category, target_cat),
      logical(1)
    )
  ]
  if (length(ecg_vars) == 0)
    stop("No variables mapped to the category ‘", target_cat, "’.")
  
  ## 2. Build a (word-boundary) regex for *each* variable
  make_pat <- function(var) {
    terms <- c(
      var,                                   # the variable name (list key)
      control_variable_synonyms[[var]]$synonyms %||% character(0)
    )
    paste0("\\b(", paste(unique(terms), collapse = "|"), ")\\b")
  }
  var_patterns <- setNames(lapply(ecg_vars, make_pat), ecg_vars)
  
  ## 3. Pre-process the data 
  df_unique   <- dplyr::distinct(df, PMID, controls)
  df_controls <- ifelse(is.na(df_unique$controls), "", df_unique$controls)
  
  ## 4. TRUE/FALSE presence matrix at *variable* level
  presence_vars <- sapply(
    var_patterns,
    function(pat) stringr::str_detect(tolower(df_controls), tolower(pat))
  )
  
  ## 5. Long data frame for hist_panel() 
  long_df <- presence_vars |>
    as.data.frame() |>
    dplyr::mutate(PMID = df_unique$PMID) |>
    tidyr::pivot_longer(
      cols      = ecg_vars,
      names_to  = "variable",
      values_to = "present"
    ) |>
    dplyr::filter(present) |>
    dplyr::select(PMID, variable)
  
  ## 6. Plot 
  p <- hist_panel(
    long_df,
    col               = "variable",    
    group_col         = "PMID",
    discrete          = TRUE,
    use_proportion    = TRUE,
    x.label           = "ECG and Heartbeat-Related Controls",
    fill_as_aesthetic = FALSE,       
    tilt_labels       = tilt_labels,
    allowed           = c("ECG" = "ECG",
                         "HEP-ECG Correlation" = "HEP-ECG\nCorrelation",
                         "Surrogate Heartbeats" = "Surrogate\nHeartbeats",
                         "RR Interval" = "RR Interval",
                         "Number of Heartbeats" = "Number of\nHeartbeats",
                         "QT Interval" = "QT Interval",
                         "Control interval" = "Control interval",
                         "T-Wave Latency" = "T-Wave Latency")
  ) + coord_flip()
  
  # Override the default fill color to match "Other Controls" category
  p <- p + 
    theme(panel.background = element_rect(fill = "white")) +
    guides(fill = "none")
 
  p$layers[[1]]$aes_params$fill <- "#647499ff"
  
  return(p)
}