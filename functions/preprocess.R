# Columns that describe the screening
screening_columns <- c(
  "source", "PMID", "DOI", "title", "authors", "citation", "year", "include", "comment"
)

columns_to_drop <- c("DOI", "include", "comment", "citation")
convert_to_numeric <- c("year", "sample_size", "meeg_num_electrodes", 
                        "length_min", "high_pass", "low_pass", "groups", 
                        "conditions", "hep_start", "hep_end", 
                        "ecg_high_pass", "ecg_low_pass",
                        "baseline_start_ms", "baseline_end_ms", "permutations", 
                        "significant_start_ms", "significant_end_ms")
convert_to_factors <- c("setting", "modality", "ICA", "ica_on_epochs", 
                        "hep_relative_to", "averaging_channels", "averaging_time", 
                        "clustering", "significant_test", "significant_relative_to")

ref_mapping <- c(
  "Common average" = "CAR",
  "Linked mastoids" = "LinkM",
  "Left mastoid" = "LM",
  "Linked earlobes" = "LinkE",
  "Cz" = "Cz",
  "Fz" = "Fz",
  "FCz" = "FCz",
  "Fpz" = "Fpz",
  "CMS" = "CMS",
  "Nose" = "Nose",
  "Laplacian reference" = "LAP",
  "REST" = "REST",
  "Other" = "Other",
  "na" = "N/A",
  "unknown" = "N/M"
)
online_ref_categories <- c(
  "Common average", 
  "Linked mastoids", "Left mastoid", 
  "Cz", "Fz", "FCz", "Fpz", 
  "CMS", "Nose", "Linked earlobes", "Other", "unknown"
)
offline_ref_categories <- c(
  "Common average", "Linked mastoids", "Linked earlobes",
  "Laplacian reference", "REST", "Cz", "unknown", "Other"
)


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


preprocess_cfa_removal <- function(df) {
  df$reject_cfa_ics <- grepl("cfa", df$rejected_components, ignore.case = T)
  
  df$cfa_minimal_rr <- as.numeric(
    str_match(tolower(df$other_cfa_removal_strategy), 
              "rr at least\\s*(\\d+)\\s*ms")[, 2]
  )
  df$cfa_use_minimal_rr <- !is.na(df$cfa_minimal_rr)
  
  df$cfa_use_minimal_artifact_window <- str_detect(tolower(df$other_cfa_removal_strategy), 
                                                   "limit analysis to time of minimal artifact")
  df$cfa_csd <- str_detect(tolower(df$other_cfa_removal_strategy), "csd")
  df$cfa_regress <- str_detect(tolower(df$other_cfa_removal_strategy), 
                               "subtract/regress ecg from eeg")
  df$cfa_pca <- str_detect(tolower(df$other_cfa_removal_strategy), 
                           "pca on hep")
  df$cfa_subtract_rest <- str_detect(tolower(df$other_cfa_removal_strategy),
                                     "subtract rshep from taskhep")
  
  df
}


preprocess_cleaning <- function(df) {
  other_cleaning <- str_split(df$other_cleaning_strategy, ', ')
  other_cleaning <- lapply(other_cleaning, \(x) tolower(trimws(x)))
  
  for (approach in c('noisy epochs', 'bad channels')) {
    use_approach <- sapply(other_cleaning, \(x) approach %in% x)
    df[[paste0("clean_", str_replace(approach, ' ', '_'))]] <- use_approach
  }
  
  df
}


preprocess_channels <- function(df) {
  # Fill in standard EEG locations if the whole layout was used
  use_layout <- df$eeg_locations == "layout"
  df$eeg_locations[use_layout] <- sapply(df$meeg_layout[use_layout], 
                                         \(x) paste(ch_names[[x]], collapse = ", "))
  
  # If all channels were selected, copy from EEG Locations
  all_selected <- df$hep_eeg_channels_selected == "All"
  df$hep_eeg_channels_selected[all_selected] <- df$eeg_locations[all_selected]
  
  # Resolve All except XX, XX
  all_except <- grepl("All except", df$hep_eeg_channels_selected)
  df$hep_eeg_channels_selected[all_except] <- 
    apply(X = df[all_except, c("eeg_locations", "hep_eeg_channels_selected")], 
          MARGIN = 1, FUN = resolve_all_except)
  
  df
}


preprocess_studies <- function(df) {
  df_category <- df %>%
    group_by(PMID) %>%
    summarise(
      has_resting = any(setting == 1),
      has_task = any(setting == 0),
      .groups = "drop"
    ) %>%
    mutate(
      study_category = case_when(
        has_resting & has_task ~ "Both",
        has_resting & !has_task ~ "Rest", 
        !has_resting & has_task ~ "Task",
        TRUE ~ "Other"
      )
    )
  assert("All studies perform either task-based or resting-state analysis",
         nrow(df_category[df_category$study_category == "Other",]) == 0)
  
  df_category$study_category <- factor(df_category$study_category,
                                       levels = c("Task", "Rest", "Both"))
  
  df <- merge(df, df_category, by = "PMID", sort = F)
  df
}

preprocess_reference <- function(df) {
  df %>%
    mutate(
      # online
      reference_online = tolower(reference_online),
      reference_online = case_when(
        reference_online %in% tolower(online_ref_categories) ~ reference_online,
        TRUE ~ "Other"
      ),
      # offline
      reference_offline = tolower(reference_offline),
      reference_offline = case_when(
        reference_offline %in% tolower(offline_ref_categories) ~ reference_offline,
        TRUE ~ "Other"
      )
    )
}

preprocess_ecg <- function(df) {
  df %>%
    mutate(ecg_lead = recode(ecg_lead,
                             "none" = "None",
                             "Multiple leads" = "Multiple\nleads",
                             "Multiple leads (Lead II)" = "Lead II",
                             "Unclassified" = "N/C",
                             "unknown" = "N/M")) %>%
    # NOTE: sym4 wavelet also serves as a data-independent template of a 
    # cardiac cycle, merging into one category
    mutate(ecg_event_method = recode(ecg_event_method,
                                     "sym4" = "Template matching"))
}


preprocess_hep_significant <- function(df) {
  # Reference event
  df_hep_reference <- df %>%
    group_by(PMID) %>%
    summarise(
      has_rpeak = any(hep_relative_to == "R-peak"),
      has_tpeak = any(hep_relative_to == "T-peak"),
      .groups = "drop"
    ) %>%
    mutate(
      reference_category = case_when(
        has_rpeak & has_tpeak ~ "Both",
        has_rpeak & !has_tpeak ~ "R-peak",
        !has_rpeak & has_tpeak ~ "T-peak",
        TRUE ~ "Other"
      )
    )
  assert("Expected R-/T-peak or both", 
         sum(df_hep_reference$reference_category == "Other") == 0)
  df_hep_reference <- df_hep_reference %>%
    mutate(reference_category = factor(reference_category, 
                                       levels = c("R-peak", "T-peak", "Both"))) %>%
    select(PMID, reference_category)
  df <- safe_merge(df, df_hep_reference, by = "PMID", sort = F)
  
  # Baseline correction
  baseline_correction <- df %>%
    distinct(PMID, baseline_defined, .keep_all = TRUE) %>%
    group_by(PMID) %>%
    summarise(
      has_yes = any(baseline_defined == "Yes"),
      has_no = any(baseline_defined == "No"),
      .groups = "drop"
    ) %>%
    mutate(
      baseline_category = case_when(
        has_yes & has_no ~ "Both",
        has_yes & !has_no ~ "Yes",
        !has_yes & has_no ~ "No",
        TRUE ~ "Unknown"
      )
    )
  baseline_correction <- baseline_correction %>%
    mutate(baseline_category = factor(baseline_category, 
                                      levels = c("Yes", "No", "Both", "Unknown"))) %>%
    select(PMID, baseline_category)
  df <- safe_merge(df, baseline_correction, by = "PMID", sort = F)
  
  # Analysis approach: averaging vs. clustering
  hep_determination_analysis <- df %>%
    group_by(PMID) %>%
    summarise(
      has_averaging = any(method_category == "Averaging"),
      has_clustering = any(method_category == "Clustering"),
      .groups = "drop"
    ) %>%
    mutate(
      determination_category = case_when(
        has_averaging & has_clustering ~ "Both",
        has_averaging & !has_clustering ~ "Averaging",
        !has_averaging & has_clustering ~ "Clustering",
        TRUE ~ "None"
      )
    )
  hep_determination_analysis <- hep_determination_analysis %>%
    mutate(determination_category = factor(determination_category, 
                                           levels = c("Averaging", "Clustering", "Both", "None"))) %>%
    select(PMID, determination_category)
  df <- safe_merge(df, hep_determination_analysis, by = "PMID", sort = F)
  
  df$hep_approach <- factor(case_when(
    (df$averaging_time == 1) & (df$clustering == 0) ~ "Averaging", 
    (df$clustering == 1) & (df$averaging_time == 0) ~ "Clustering"
  ))
  
  df <- df %>%
    mutate(
      significant_eeg_channels = if_else(
        df$hep_approach == "Averaging" & df$significant_test == 1, 
        df$hep_eeg_channels_selected,
        df$significant_eeg_channels,
        missing = df$significant_eeg_channels
      )
    )
  
  # Window type
  df_hep_window_type <- df %>%
    group_by(PMID) %>%
    summarise(
      has_primary = any(hep_window_type == "Primary"),
      has_secondary = any(hep_window_type == "Secondary"),
      .groups = "drop"
    ) %>%
    mutate(
      window_type_category = case_when(
        has_primary & has_secondary ~ "Both",
        has_primary & !has_secondary ~ "Primary",
        !has_primary & has_secondary ~ "Secondary",
        TRUE ~ "Other"
      )
    )
  assert("Expected at least primary or secondary", 
         sum(df_hep_window_type$window_type_category == "Other") == 0)
  df_hep_window_type <- df_hep_window_type %>%
    mutate(window_type_category = factor(window_type_category, 
                                         levels = c("Primary", "Secondary", "Both"))) %>%
    select(PMID, window_type_category)
  df <- safe_merge(df, df_hep_window_type, by = "PMID", sort = F)
  
  df
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
      First_Author_Surname = word(authors, 1, sep = " "),
      Paper = case_when(
        str_detect(authors, ",") ~ paste0(First_Author_Surname, " et al. (", year),
        TRUE ~ paste0(First_Author_Surname, " (", year)
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

process_trial_column <- function(df, col) {
  # Capture the name of the column as a string
  col_name <- deparse(substitute(col))
  new_mean_col <- paste0(col_name, "_Mean")
  new_sd_col <- paste0(col_name, "_SD")
  new_original_col <- paste0(col_name, "_original")
  
  # Process the column and return a tibble with three new columns
  out <- df %>%
    # Ensure the target column is character
    mutate({{col}} := as.character({{col}})) %>%
    transmute(
      !!new_mean_col := case_when(
        str_detect({{col}}, "\\[est\\d+\\]") ~ as.numeric(str_extract({{col}}, "\\d+")),
        str_detect({{col}}, "\\d+\\+-\\d+") ~ as.numeric(str_split_fixed({{col}}, "\\+\\-", 2)[, 1]),
        str_detect({{col}}, "^\\d+$") ~ as.numeric({{col}}),
        is.na({{col}}) | {{col}} == "" ~ NA_real_,
        TRUE ~ NA_real_
      ),
      !!new_sd_col := case_when(
        str_detect({{col}}, "\\d+\\+-\\d+") ~ as.numeric(str_split_fixed({{col}}, "\\+\\-", 2)[, 2]),
        TRUE ~ NA_real_
      ),
      !!new_original_col := case_when(
        str_detect({{col}}, "\\[est\\d+\\]") ~ NA_real_,  # Set estimated values to NA
        str_detect({{col}}, "\\d+\\+-\\d+") ~ as.numeric(str_split_fixed({{col}}, "\\+\\-", 2)[, 1]),  # Extract mean from SD entries
        str_detect({{col}}, "^\\d+$") ~ as.numeric({{col}}),
        is.na({{col}}) | {{col}} == "" ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  return(out)
}

preprocess <- function(df_full, output_screening = T, drop_cols = T, adjust_data_types = T) {
  
  if (output_screening){
    # Extract and return two dataframes
    # Screening - all information about screening (include/comment) that is
    # required to generate the PRISMA diagram, keep only one row per paper
    df_screening <- df_full[, screening_columns] %>%
      filter(!is.na(include))
  }

  # The main dataframe that contains only the rows for included papers
  included_pmids <- df_full %>%
    filter(include == 1) %>%
    pull(PMID)
  
  df_included <- df_full %>%
    filter(PMID %in% included_pmids)
  
  # Add journal columns
  df_included <- add_journal_column(df_included)
  
  if (drop_cols){
    df_included <- df_included %>%
      mutate(across(all_of(columns_to_drop), ~ NULL))
  }
  
  # Create baseline_defined 
  df_included <- df_included %>%
    mutate(
      baseline_defined = case_when(
        baseline_start_ms == "unknown" | baseline_end_ms == "unknown" ~ "Unknown",
        baseline_start_ms == "none" | baseline_end_ms == "none" ~ "No",
        !is.na(baseline_start_ms) & !is.na(baseline_end_ms) ~ "Yes",
        TRUE ~ "No"
      )
    )
  
  # Add columns averaging / clustering
  df_included <- df_included %>%
    mutate(
      method_category = case_when(
        averaging_time == "1" & clustering == "0" ~ "Averaging",
        clustering == "1" & averaging_time == "0" ~ "Clustering",
        TRUE ~ "Other"
      ),
      method_numeric = ifelse(method_category == "Averaging", 1, 0)
    )
  
  if (adjust_data_types){
    df_included <- adjust_data_type(df_included, convert_to_numeric, convert_to_factors)
  }
  
  # NOTE: apply steps one by one to get adequate messages in case of errors,
  # chaining with %>% mixes error messages from all calls
  df_included <- preprocess_studies(df_included)
  df_included <- preprocess_ecg(df_included)
  df_included <- preprocess_cleaning(df_included)
  df_included <- preprocess_cfa_removal(df_included)
  df_included <- preprocess_channels(df_included)
  df_included <- preprocess_hep_significant(df_included)
  
  # transform included IC data
  df_included$rejected_cardiac_ics <- sapply(df_included$rejected_cardiac_ics, clean_cardiac_ics)
  
  # add Paper column (readable unique identifier)
  df_included$paper <- create_author_column(df_included)
  
  # process trials column
  new_trial_cols <- process_trial_column(df_included, trials)
  df_included <- bind_cols(df_included, new_trial_cols)
  df_included$trials_Mean <- as.numeric(df_included$trials_Mean)
  df_included$trials_original <- as.numeric(df_included$trials_original)

  # clean up cases where additional tests follow ANOVA
  df_included$statistics <- case_when(
    str_detect(df_included$statistics, regex("anova", ignore_case = TRUE)) ~ "ANOVA", 
    TRUE ~ df_included$statistics
  )

  if (output_screening){
    list(df_screening, df_included)
  } else {
    return(df_included)
  }
}
