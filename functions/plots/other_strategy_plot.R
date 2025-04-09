other_strategy_plot <- function(df) {
  df_other <- df %>%
    distinct(PMID, other_cfa_removal_strategy) %>%
    filter(
      other_cfa_removal_strategy != "",
      other_cfa_removal_strategy != "unknown"
    ) %>%
    mutate(merged_strategy = case_when(
      other_cfa_removal_strategy == "limit analysis to time of minimal artifact" ~ 
        "minimal artifact\nwindow",
      str_detect(tolower(other_cfa_removal_strategy), "^rr at least") ~ 
        "RR interval\nconstraints",
      tolower(other_cfa_removal_strategy) %in% c(
        "correct for cfa using the signal from the tip of the nose",
        "regress ecg out",
        "subtracting the cardiac signal artifact (skin-conducted ekg signal to the scalp) from the eeg signal",
        "scaled ecg subtracted from eeg",
        "subtract average ecg",
        "ecg subtraction",
        "weitkunat, r., & schandry, r. (1995)."
      ) ~ "subtract / regress\nECG from EEG",
      tolower(other_cfa_removal_strategy) == "subtract subject average resting state heps from task heps" ~ 
        "subtract\nrsHEP from\ntask HEP",
      str_detect(tolower(other_cfa_removal_strategy), "pca|hep") ~ "PCA on\nHEP",    
      tolower(other_cfa_removal_strategy) == "hjorth" ~ "CSD",
      TRUE ~ other_cfa_removal_strategy
    ))
  
  hist_panel(df_other,
             col = "merged_strategy",
             group_col = "PMID",
             discrete = TRUE,
             use_proportion = FALSE, 
             x.label = "Other strategies for the removal of CFA") +
    scale_y_continuous(expand = c(0, 0))
}
