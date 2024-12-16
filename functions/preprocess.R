library(dplyr)       # %>%, rename

# Columns that describe the screening
screening_columns <- c("Source", "PMID", "DOI", "Analyst", "Include", 
                       "Comment", "Title", "Authors", "Citation", "Year")

# Mapping for renaming: "new column name" = "old column name"
column_mapping = c(
  "ecg_num_electrodes" = "X.ECG.electrodes",
  "ecg_description" = "ECG.Description",
  "ecg_lead" = "ECG.Lead",
  "ecg_location" = "ECG.Locations",
  "ecg_ground" = "ECG.Ground",
  "hep_window_type" = "HEP...Window.Type",
  "hep_channels_selected" = "Channels.selected",
  "stats_hypothesis" = "Hypothesis",
  "stats_permutation" = "Cluster.based.Permutation"
  # NOTE: add other columns here
)


load_data <- function(pubmed.path, manual.path) {
  # Load the data
  df_pubmed <- read.csv(pubmed.path, skip = 1)
  df_manual <- read.csv(manual.path, skip = 1)
  
  # Combine the dataframe but keep the information about the source
  df_pubmed$Source <- "pubmed"
  df_manual$Source <- "manual"
  df_full <- rbind(df_pubmed, df_manual)
  
  df_full
}


preprocess <- function(df_full) {
  # Rename the columns
  df_full <- rename(df_full, all_of(column_mapping))
  
  # Extract and return two dataframes
  # 1. Screening - all information about screening (include/comment) that is 
  # required to generate the PRISMA diagram
  df_screening <- df_full[,screening_columns]
  
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
  
  list(df_screening, df_included)
}
