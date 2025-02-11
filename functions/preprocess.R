library(dplyr) # %>%, rename

# Columns that describe the screening
screening_columns <- c(
  "source", "PMID", "DOI", "Analyst", "Include",
  "Comment", "Title", "Authors", "Citation", "Year"
)

# Mapping for renaming: "new column name" = "old column name"
column_mapping <- c(
  "ecg_num_electrodes" = "X.ECG.electrodes",
  "ecg_description" = "ECG.Description",
  "ecg_lead" = "ECG.Lead",
  "ecg_locations" = "ECG.Locations",
  "ecg_ground" = "ECG.Ground",
  "hep_window_type" = "HEP...Window.Type",
  "hep_channels_selected" = "Channels.selected",
  "stats_hypothesis" = "Hypothesis",
  "stats_permutation" = "Cluster.based.Permutation",
  "hep_start" = "HEP...Start..ms.",
  "hep_end" = "HEP...End..ms.",
  "eeg_locations" = "EEG.Locations",
  "channels" = "X.Channels",
  "layout" = "Layout",
  "reference_online" = "Reference..online.",
  "reference_offline" = "Reference..offline.",
  "high_pass" = "High.pass",
  "low_pass" = "Low.pass",
  "ica_on_epochs" = "ICA.on.epochs",
  "rejected_components" = "Rejected.components",
  "rejected_cardiac_ics" = "X..rejected.cardiac.ICs",
  "cfa_rej_approach" = "CFA.Rej..Approach",
  "cfa_rej_criteria" = "CFA.Rej..Criteria",
  "other_cfa_removal_strategy" = "Other.CFA.removal.strategy",
  "other_cleaning_strategy" = "Other.cleaning.strategy",
  "groups" = "X.Groups",
  "conditions" = "X.Conditions",
  "trials_sd" = "X.Trials...SD.",
  "hep_relative_to" = "HEP...Relative.to",
  "baseline_start_ms" = "Baseline...Start..ms.",
  "baseline_end_ms" = "Baseline...End..ms.",
  "value" = "Value",
  "averaging_channels" = "Averaging..channels.",
  "averaging_time" = "Averaging..time.",
  "statistics" = "Statistics",
  "permutations" = "X.Permutations",
  "multiple_comparisons" = "Multiple.Comparisons",
  "significant_test" = "Significant.test",
  "significant_channels" = "Significant...Channels",
  "significant_relative_to" = "Significant...Relative.to",
  "significant_start_ms" = "Significant...Start..ms.",
  "significant_end_ms" = "Significant...End..ms.",
  "controls" = "Controls",
  "other_notes" = "Other.notes..unclassified.",
  "motivation" = "Motivation",
  "sample_size" = "Sample.size"
 

  # NOTE: add other columns here
)


load_data <- function(pubmed.path, manual.path) {
  # Load the data
  df_pubmed <- read.csv(pubmed.path, skip = 1)
  df_manual <- read.csv(manual.path, skip = 1)

  # Combine the dataframe but keep the information about the source
  df_pubmed$source <- "pubmed"
  df_manual$source <- "manual"
  df_full <- rbind(df_pubmed, df_manual)

  df_full
}


preprocess_screening <- function(df_screening) {
  df_screening %>%
    mutate(Comment = recode(Comment,
      "Consider (v2)" = "Consider",
      "Different Species" = "Not related to HEP",
      "Not Related" = "Not related to HEP",
      "Intracranial Recordings" = "Intracranial recordings",
      "Conference Abstract" = "Conference abstract"
    ))
}


# Clean cardiac IC rejection data
clean_cardiac_ics <- function(x) {
  # Return NA for NULL, NA, or empty strings
  if (is.null(x) || is.na(x) || x == "") return(NA_real_)
  
  # Handle mean ± SD format (e.g., "4.78+-1.13")
  if (grepl("\\+-", x)) {
    mean_val <- as.numeric(sub("\\+-.*$", "", x))
    return(mean_val)
  }
  
  # Handle ranges (e.g., "0-3", "1–3", "2–4")
  if (grepl("-|–", x)) {
    range_vals <- strsplit(x, "-|–")[[1]]
    return(mean(as.numeric(range_vals)))
  }
  
  # Handle single numbers
  if (grepl("^\\d+(\\.\\d+)?$", x)) {
    return(as.numeric(x))
  }
  
  # If the value did not match any format, warn and return NA_real_
  warning(paste("Could not parse cardiac IC value:", x))
  return(NA_real_)
}


preprocess <- function(df_full) {
  # Rename the columns
  df_full <- rename(df_full, all_of(column_mapping))

  # Extract and return two dataframes
  # 1. Screening - all information about screening (include/comment) that is
  # required to generate the PRISMA diagram, keep only one row per paper
  df_screening <- df_full[, screening_columns] %>%
    filter(!is.na(Include)) %>%
    preprocess_screening()

  # 2. The main dataframe that contains only the rows for included papers
  included_pmids <- df_full %>%
    filter(Include == 1) %>%
    pull(PMID)

  df_included <- df_full %>%
    filter(PMID %in% included_pmids)

  # NOTE: Apply additional preprocessing steps to df_included here using
  # something along the lines of:
  #
  # df_included <- preprocess_ecg(df_included)

  # 4. adjust data types
  df_included$rsHEP <- as.factor(df_included$rsHEP)
  df_included$ICA <- as.factor(df_included$ICA)
  df_included$hep_start <- as.numeric(df_included$hep_start)
  df_included$hep_end <- as.numeric(df_included$hep_end)
  df_included$high_pass <- as.numeric(df_included$high_pass)
  df_included$low_pass <- as.numeric(df_included$low_pass)
  df_included$channels <- as.numeric(df_included$channels)

  #5 transform included IC data
  df_included$rejected_cardiac_ics <- sapply(df_included$rejected_cardiac_ics, clean_cardiac_ics)

  list(df_screening, df_included)
}
