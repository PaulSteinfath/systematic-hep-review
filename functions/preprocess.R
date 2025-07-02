# Columns that describe the screening
screening_columns <- c(
  "source", "PMID", "DOI", "Include",
  "Comment", "Title", "Authors", "Citation", "Year"
)

convert_to_numeric <- c("Year", "sample_size", "meeg_num_electrodes", "length_min", "high_pass", "low_pass", "groups", "conditions", "hep_start", "hep_end", 
                        "baseline_start_ms", "baseline_end_ms", "permutations", "significant_start_ms", "significant_end_ms")
convert_to_factors <- c("rsHEP", "Modality", "ICA", "ica_on_epochs", "hep_relative_to", "averaging_channels", "averaging_time", "clustering", "significant_test", 
                        "significant_relative_to")

load_data <- function(pubmed.path, manual.path) {
  # Load the data
  df_pubmed <- read.csv(pubmed.path)
  df_manual <- read.csv(manual.path)
  
  # Combine the dataframe but keep the information about the source
  df_pubmed$source <- "pubmed"
  df_manual$source <- "manual"
  df_full <- rbind(df_pubmed, df_manual)
  
  return(df_full)
}


resolve_all_except <- function(row) {
  # Expected input: list(all_locs, except_locs)
  all_locs <- unlist(strsplit(row[[1]], ", "))

  except_locs <- gsub("All except ", "", row[[2]])
  except_locs <- unlist(strsplit(except_locs, ", "))
  
  kept_cols <- setdiff(all_locs, except_locs)
  paste(kept_cols, collapse = ", ")
}


preprocess_channels <- function(df) {
  # Fill in standard EEG locations if the whole layout was used
  use_layout <- df$eeg_locations == "layout"
  df$eeg_locations[use_layout] <- sapply(df$meeg_layout[use_layout], 
                                         \(x) paste(ch_names[[x]], collapse = ", "))
  
  # If all channels were selected, copy from EEG Locations
  all_selected <- df$hep_channels_selected == "All"
  df$hep_channels_selected[all_selected] <- df$eeg_locations[all_selected]
  
  # Resolve All except XX, XX
  all_except <- grepl("All except", df$hep_channels_selected)
  df$hep_channels_selected[all_except] <- 
    apply(X = df[all_except, c("eeg_locations", "hep_channels_selected")], 
          MARGIN = 1, FUN = resolve_all_except)
  
  df
}


preprocess_ecg <- function(df) {
  df %>%
    mutate(ecg_lead = recode(ecg_lead,
                             "none" = "None",
                             "Multiple leads" = "Multiple\nleads",
                             "Multiple leads (lead II)" = "Lead II",
                             "unknown" = "N/M",
                             "Unclassified" = "N/C"))
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
      filter(!is.na(Include))
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
  
  if (adjust_data_types){
    df_included <- adjust_data_type(df_included, convert_to_numeric, convert_to_factors)
  }
  
  df_included <- df_included %>%
    preprocess_ecg() %>%
    preprocess_channels()
  
  # Transform included IC data
  df_included$rejected_cardiac_ics <- sapply(df_included$rejected_cardiac_ics, clean_cardiac_ics)
  
  # Add paper column (readable unique identifier)
  df_included$paper <- create_author_column(df_included)

  # Add columns averaging / clustering & baseline
  df_included <- df_included %>%
    mutate(
      method_category = case_when(
        averaging_time == "1" & clustering == "0" ~ "Averaging",
        clustering == "1" & averaging_time == "0" ~ "Clustering",
        TRUE ~ "Other"
      ),
      method_numeric = ifelse(method_category == "Averaging", 1, 0),
      baseline_defined = case_when(
        !is.na(baseline_start_ms) & !is.na(baseline_end_ms) ~ 1,
        TRUE ~ 0
      )
    )
  
  if (output_screening){
    list(df_screening, df_included)
  } else {
    return(df_included)
  }
}
