library(dplyr)
library(testit)


# Paths
data_path <- file.path(getwd(), 'data')
codebook_raw_path <- file.path(data_path, 'Codebook.csv')
manual_raw_path <- file.path(data_path, 'HEP - Manual.csv')
pubmed_raw_path <- file.path(data_path, 'HEP - Pubmed Results.csv')
codebook_save_path <- file.path(data_path, 'HER_methods_review_codebook.csv')
manual_save_path <- file.path(data_path, 'HER_methods_review_manual.csv')
pubmed_save_path <- file.path(data_path, 'HER_methods_review_pubmed.csv')


# Mapping for renaming: "new column name" = "old column name"
# The order of old column names corresponds to the order of columns in the table
column_mapping <- c(
  # Pubmed columns are not changed
  "preregistration" = "Preregistration",
  "topic" = "Topic",
  "sample_size" = "Sample.size",
  "setting" = "rsHEP",
  "modality" = "Modality",
  "meeg_num_electrodes" = "X.Channels",
  "meeg_layout" = "Layout",
  "eeg_locations" = "EEG.Locations",
  "length_min" = "Length..min.",
  "ecg_num_electrodes" = "X.ECG.electrodes",
  "ecg_description" = "ECG.Description",
  "ecg_lead" = "ECG.Lead",
  "ecg_locations" = "ECG.Locations",
  "ecg_ground" = "ECG.Ground",
  "reference_online" = "Reference..online.",
  "reference_offline" = "Reference..offline.",
  "high_pass" = "High.pass",
  "low_pass" = "Low.pass",
  "ica" = "ICA",
  "ica_on_epochs" = "ICA.on.epochs",
  "rejected_components" = "Rejected.components",
  "num_rejected_cardiac_ics" = "X..rejected.cardiac.ICs",
  "cfa_rej_approach" = "CFA.Rej..Approach",
  "cfa_rej_criteria" = "CFA.Rej..Criteria",
  "other_cfa_removal_strategy" = "Other.CFA.removal.strategy",
  "other_cleaning_strategy" = "Other.cleaning.strategy",
  "her_channels_selected" = "Channels.selected",
  "num_groups" = "X.Groups",
  "num_conditions" = "X.Conditions",
  "num_trials" = "X.Trials...SD.",
  "her_window_type" = "HEP...Window.Type",
  "her_relative_to" = "HEP...Relative.to",
  "her_start_ms" = "HEP...Start..ms.",
  "her_end_ms" = "HEP...End..ms.",
  "baseline_start_ms" = "Baseline...Start..ms.",
  "baseline_end_ms" = "Baseline...End..ms.",
  "hypothesis" = "Hypothesis",
  "value" = "Value",
  "averaging_channels" = "Averaging..channels.",
  "averaging_time" = "Averaging..time.",
  "statistics" = "Statistical.test",
  "clustering" = "Cluster.based.Permutation",
  "permutations" = "X.Permutations",
  "multiple_comparisons" = "Multiple.Comparisons",
  "significant_test" = "Significant.test",
  "significant_channels" = "Significant...Channels",
  "significant_relative_to" = "Significant...Relative.to",
  "significant_start_ms" = "Significant...Start..ms.",
  "significant_end_ms" = "Significant...End..ms.",
  "controls" = "Controls",
  "trial_estimation" = "Trial.Estimation",
  "other_notes" = "Other.notes..unclassified.",
  "motivation" = "Motivation"
)
inverse_mapping <- setNames(names(column_mapping), column_mapping)

columns_to_drop <- c(
  "topic",
  "ecg_description", 
  "multiple_comparisons",
  "other_notes", 
  "motivation", 
  "Analyst",
  "Link"
)

screening_columns <- c(
  "source", "PMID", "DOI", "Analyst", "Include",
  "Comment", "Title", "Authors", "Citation", "Year"
)


cleanup_data <- function(df) {
  # Rename columns to make the names more usable in scripts
  df <- rename(df, all_of(column_mapping))
  
  # Remove unused columns
  df <- select(df, !all_of(columns_to_drop))
  
  df
}


cleanup_codebook <- function(df) {
  # Update column names according to the mapping
  old_names <- make.names(df$column)
  new_names <- unlist(lapply(old_names, \(x) case_when(
    x %in% names(inverse_mapping) ~ inverse_mapping[x],
    .default = x)))
  df$column <- new_names
  
  df
}

preprocess_internal <- function() {
  df_pubmed <- read.csv(pubmed_raw_path, skip = 1)
  df_pubmed <- cleanup_data(df_pubmed)
  
  df_manual <- read.csv(manual_raw_path, skip = 1)
  df_manual <- cleanup_data(df_manual)
  
  df_codebook <- read.csv(codebook_raw_path, skip = 1)
  df_codebook <- cleanup_codebook(df_codebook)
  
  assert("column names in the codebook and table match",
         identical(sort(names(df_pubmed)), sort(df_codebook$column)))
  assert("column names in the codebook and table match",
         identical(sort(names(df_manual)), sort(df_codebook$column)))
  
  write.csv(df_pubmed, pubmed_save_path, row.names = F)
  write.csv(df_manual, manual_save_path, row.names = F)
  write.csv(df_codebook, codebook_save_path, row.names = F)
}


preprocess_internal()