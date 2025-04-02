library(dplyr) 
library(stringr)

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
  "hypothesis" = "Hypothesis",
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
  "trials" = "X.Trials...SD.",
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
  "sample_size" = "Sample.size",
  "clustering" = "Cluster.based.Permutation",
  "length_min" = "Length..min."
)

columns_to_drop <- c("Other.notes..unclassified.", "Motivation", "DOI", "Link", "Analyst", "Include", "Comment", "Citation", "ECG.Description", "Multiple.Comparisons")
convert_to_numeric <- c("Year", "sample_size", "channels", "length_min", "ecg_num_electrodes", "high_pass", "low_pass", "groups", "conditions", "hep_start", "hep_end", 
                        "baseline_start_ms", "baseline_end_ms", "permutations", "significant_start_ms", "significant_end_ms")
convert_to_factors <- c("rsHEP", "Modality", "ICA", "ica_on_epochs", "hep_relative_to", "averaging_channels", "averaging_time", "clustering", "significant_test", 
                        "significant_relative_to")

load_data <- function(pubmed.path, manual.path) {
  # Load the data
  df_pubmed <- read.csv(pubmed.path, skip = 1)
  df_manual <- read.csv(manual.path, skip = 1)
  
  df_manual <- df_manual %>%
    filter(!is.na(PMID) & PMID != "")
  
  # Combine the dataframe but keep the information about the source
  df_pubmed$source <- "pubmed"
  df_manual$source <- "manual"
  df_full <- rbind(df_pubmed, df_manual)
  
  return(df_full)
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


preprocess_ecg <- function(df) {
  df %>%
    mutate(ecg_lead = recode(ecg_lead,
                             "none" = "None",
                             "Single-channel" = "Single\nchannel",
                             "Multiple leads" = "Multiple\nleads",
                             "Multiple leads (including lead I)" = "Multiple\nleads",
                             "Multiple leads (including lead II)" = "Multiple\nleads",
                             "Multiple leads (including leads I, II, III)" = "Multiple\nleads",
                             "unknown" = "N/M"))
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
    # Return mean of range
    return(mean(as.numeric(range_vals)))
  }
  
  # Handle single numbers
  if (grepl("^\\d+(\\.\\d+)?$", x)) {
    return(as.numeric(x))
  }
  
  return(NA_real_)
}

create_author_column <- function(data) {
  paper_vector <- data %>%
    mutate(
      First_Author_Surname = word(Authors, 1, sep = " "),
      Paper = case_when(
        str_detect(Authors, ",") ~ paste0(First_Author_Surname, " et al. (", Year),
        TRUE ~ paste0(First_Author_Surname, " (", Year)
      )
    ) %>%
    group_by(Paper) %>%
    mutate(
      Paper = if (n_distinct(PMID) > 1) {
        paste0(Paper, letters[match(PMID, unique(PMID))], ")")
      } else paste0(Paper, ")")
    ) %>%
    ungroup() %>%
    pull(Paper) # Extract the column as a vector
  
  return(paper_vector)
}

adjust_data_type <- function(df, adjust_numeric = c(), adjust_factor = c()) {
  
  valid_numeric <- intersect(names(df), adjust_numeric)
  valid_factor <- intersect(names(df), adjust_factor)
  
  if (length(valid_numeric) > 0) {
    df <- df %>%
      mutate(across(all_of(valid_numeric), as.numeric))
  }
  
  if (length(valid_factor) > 0) {
    df <- df %>%
      mutate(across(all_of(valid_factor), as.factor))
  }
  
  return(df)
}

preprocess <- function(df_full, output_screening = T, drop_cols = T, adjust_data_types = T) {
  
  if (output_screening){
    # Extract and return two dataframes
    # Screening - all information about screening (include/comment) that is
    # required to generate the PRISMA diagram, keep only one row per paper
    df_screening <- df_full[, screening_columns] %>%
      filter(!is.na(Include)) %>%
      preprocess_screening()
  }

  # The main dataframe that contains only the rows for included papers
  included_pmids <- df_full %>%
    filter(Include == 1) %>%
    pull(PMID)
  
  df_included <- df_full %>%
    filter(PMID %in% included_pmids)
  
  if (drop_cols){
    df_included <- df_included %>%
      mutate(across(all_of(columns_to_drop), ~ NULL))
  }
  
  # Rename the columns
  valid_mapping <- column_mapping[column_mapping %in% names(df_included)]
  df_included <- rename(df_included, all_of(valid_mapping))
  
  if (adjust_data_types){
    df_included <- adjust_data_type(df_included, convert_to_numeric, convert_to_factors)
  }
  
  df_included <- preprocess_ecg(df_included)
  
  # transform included IC data
  df_included$rejected_cardiac_ics <- sapply(df_included$rejected_cardiac_ics, clean_cardiac_ics)
  
  # add Paper column (readable unique identifier)
  df_included$paper <- create_author_column(df_included)
  
  if (output_screening){
    list(df_screening, df_included)
  } else {
    return(df_included)
  }
}
