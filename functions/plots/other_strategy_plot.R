other_strategy_plot <- function(df) {
  df_other <- df %>%
    select(PMID, other_cfa_removal_strategy) %>%
    distinct(PMID, other_cfa_removal_strategy, .keep_all = TRUE) %>%
    filter(
      other_cfa_removal_strategy != "",
      other_cfa_removal_strategy != "limit analysis to time of minimal artifact",
      other_cfa_removal_strategy != "unknown",
      !str_detect(tolower(other_cfa_removal_strategy), "^rr at least")
    )
  
  df_other <- df_other %>%
    mutate(merged_strategy = case_when(
      tolower(other_cfa_removal_strategy) %in% c(
        "correct for cfa using the signal from the tip of the nose",
        "regress ecg out",
        "subtracting the cardiac signal artifact (skin-conducted ekg signal to the scalp) from the eeg signal",
        "scaled ecg subtracted from eeg",
        "subtract average ecg",
        "ecg subtraction"
      ) ~ "subtract / regress\nECG from EEG",
      other_cfa_removal_strategy == "Weitkunat, R., & Schandry, R. (1995)." ~ "Weitkunat, R., &\n Schandry, R.\n (1995)",
      tolower(other_cfa_removal_strategy) == "subtract subject average resting state heps from task heps" ~ "subtract\nrsHEP from\ntask HEP",
      str_detect(tolower(other_cfa_removal_strategy), "pca|hep") ~ "PCA on\nHEP",     
      TRUE ~ other_cfa_removal_strategy
    ))
  
  hist_panel(df_other,
             col = "merged_strategy",
             group_col = "PMID",
             discrete = TRUE,
             use_proportion = FALSE, 
             x.label = "Other CFA strategies") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = function(x) seq(0, ceiling(max(x)), by = 1),
                       limits = c(0, NA))
}
