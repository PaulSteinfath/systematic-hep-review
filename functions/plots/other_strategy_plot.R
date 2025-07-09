other_strategy_plot <- function(df) {
  df_other <- df %>%
    distinct(PMID, other_cfa_removal_strategy) %>%
    filter(
      other_cfa_removal_strategy != "",
      other_cfa_removal_strategy != "unknown"
    ) %>%
    separate_rows(other_cfa_removal_strategy, sep = ",\\s*") %>%
    mutate(other_cfa_removal_strategy = str_trim(other_cfa_removal_strategy)) %>%
    filter(other_cfa_removal_strategy != "") %>%
    mutate(merged_strategy = case_when(
      str_detect(tolower(other_cfa_removal_strategy), "^rr at least") ~ 
        "RR interval\nconstraints",
      tolower(other_cfa_removal_strategy) == "csd" ~ "CSD",
      other_cfa_removal_strategy == "limit analysis to time of minimal artifact" ~ 
        "minimal artifact\nwindow",
      tolower(other_cfa_removal_strategy) %in% c(
        "subtract/regress ecg from eeg"
      ) ~ "subtract/regress\nECG from EEG",
      tolower(other_cfa_removal_strategy) == "pca on heps" ~ "PCA on\nHEP",
      tolower(other_cfa_removal_strategy) == "subtract rshep from taskhep" ~ 
        "subtract rsHEP\nfrom task HEP",
      TRUE ~ other_cfa_removal_strategy
    ))
  
  hist_panel(df_other,
             col = "merged_strategy",
             group_col = "PMID",
             discrete = TRUE,
             use_proportion = TRUE, 
             x.label = "Other strategies for the removal of CFA")
}
