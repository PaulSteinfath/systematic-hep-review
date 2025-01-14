library(dplyr)       # %>%, rename

# Columns that describe the screening
screening_columns <- c("Source", "PMID", "DOI", "Analyst", "Include", 
                       "Comment", "Title", "Authors", "Citation", "Year")

# Mapping for renaming: "new column name" = "old column name"
column_mapping = c(
  "modality" = "Modality",
  "eeg_num_electrodes" = "X.Channels",
  "eeg_layout" = "Layout",
  "eeg_locations" = "EEG.Locations",
  "ecg_num_electrodes" = "X.ECG.electrodes",
  "ecg_description" = "ECG.Description",
  "ecg_lead" = "ECG.Lead",
  "ecg_locations" = "ECG.Locations",
  "ecg_ground" = "ECG.Ground",
  "hep_window_type" = "HEP...Window.Type",
  "hep_channels_selected" = "Channels.selected",
  "averaging_channels" = "Averaging..channels.",
  "averaging_time" = "Averaging..time.",
  "stats_hypothesis" = "Hypothesis",
  "stats_permutation" = "Cluster.based.Permutation",
  "significant_channels" = "Significant...Channels"
  # NOTE: add other columns here
)

# Channel names that are tracked for different layouts
# Source: https://www.fieldtriptoolbox.org/template/layout and the corresponding
# layout files from the FieldTrip GitHub repository
# NOTE: channels outside of the head contour (e.g., F9/F10) were removed to make 
# the visualization more compact
ch_names <- list(
  standard19 = c("Fp1", "Fp2", 
                 "F7", "F3", "Fz", "F4", "F8", 
                 "T3", "C3", "Cz", "C4", "T4", 
                 "T5", "P3", "Pz", "P4", "T6", 
                 "O1", "O2"),
  standard61 = c("Fp1", "Fpz", "Fp2", 
                 "AF7", "AF3", "AFz", "AF4", "AF8", 
                 "F7", "F5", "F3", "F1", "Fz", "F2", "F4", "F6", "F8", 
                 "FT7", "FC5", "FC3", "FC1", "FCz", "FC2", "FC4", "FC6", "FT8", 
                 "T7", "C5", "C3", "C1", "Cz", "C2", "C4", "C6", "T8", 
                 "TP7", "CP5", "CP3", "CP1", "CPz", "CP2", "CP4", "CP6", "TP8", 
                 "P7", "P5", "P3", "P1", "Pz", "P2", "P4", "P6", "P8", 
                 "PO7", "PO3", "POz", "PO4", "PO8", 
                 "O1", "Oz", "O2"),
  biosemi128 = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", 
                 "A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", 
                 "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", 
                 "A25", "A26", "A27", "A28", "A29", "A30", "A31", "A32", 
                 "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", 
                 "B9", "B10", "B11", "B12", "B13", "B14", "B15", "B16", 
                 "B17", "B18", "B19", "B20", "B21", "B22", "B23", "B24", 
                 "B25", "B26", "B27", "B28", "B29", "B30", "B31", "B32",
                 "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", 
                 "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", 
                 "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", 
                 "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32", 
                 "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", 
                 "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", 
                 "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", 
                 "D25", "D26", "D27", "D28", "D29", "D30", "D31", "D32")
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


preprocess_channels <- function(df) {
  # Resolve EEG locations if standard were used
  df <- df %>%
    mutate(eeg_layout = recode(eeg_layout,
                               "10-10" = "standard61",
                               "10-20" = "standard19"))
  df$eeg_locations[df$eeg_locations == "standard"] <- sapply(df$eeg_layout[df$eeg_locations == "standard"], 
                                                             \(x) paste(ch_names[[x]], collapse = ", "))
  
  # If all channels were selected, copy from EEG Locations
  all_channels_selected <- df$hep_channels_selected == "All"
  df$hep_channels_selected[all_channels_selected] <- df$eeg_locations[all_channels_selected]
  
  # Allow All except, copy from EEG locations and remove unnecessary
  
  # NOTE: C4 might mean different things in different layouts
  # Resolve layouts: GSN-HydroCel-129, biosemi128
  
  df
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
  df_included <- preprocess_channels(df_included)
  
  list(df_screening, df_included)
}
