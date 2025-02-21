create_ica_rej <- function(df) {
  # Filter for ICA=1 and get distinct PMIDs with their rejected components
  df_ica <- df %>%
    filter(ICA == 1) %>%
    distinct(PMID, rejected_components)

  # Clean rejected_components: replace newlines with a comma, collapse multiple commas, and trim spaces/commas
  df_ica$rejected_components <- sapply(df_ica$rejected_components, function(x) {
    x <- gsub("[\r\n]+", ",", x)    # replace any newline characters with comma
    x <- gsub(",+", ",", x)         # collapse multiple commas
    x <- gsub("^,|,$", "", x)        # remove leading and trailing commas
    comps <- unlist(strsplit(x, ","))
    comps <- unique(trimws(comps))
    comps <- comps[comps != ""]
    paste(comps, collapse = ",")
  })

  # Count pipelines before splitting
  pipeline_n <- nrow(df_ica)

  df_long <- df_ica %>%
    separate_rows(rejected_components, sep = ",") %>%
    mutate(
      rejected_components = trimws(rejected_components),
      rejected_components = tolower(rejected_components)
    )

  # Create a mapping of common variations to standardized names
  component_mapping <- c(
    "eye movements" = "Ocular",
    "blinks" = "Ocular",
    "muscle" = "Muscle",
    "cfa" = "CFA",
    "channel noise" = "Channel\nnoise",
    "line noise" = "Line\nnoise",
    "other" = "Other"
  )

  df_long <- df_long %>%
    mutate(
      rejected_components = component_mapping[rejected_components] %>% coalesce(rejected_components)
    )
  
  # Remove duplicate rows and filter out "unknown" components
  df_long_dedup <- df_long %>% 
    distinct(PMID, rejected_components, .keep_all = TRUE) %>%
    filter(tolower(rejected_components) != "unknown")  # Exclude "unknown"
  
  # Use hist_panel to generate the histogram plot
  hist_panel(
    df_long_dedup,
    col = "rejected_components",
    group_col = "PMID",
    discrete = TRUE,
    x.label = "Types of rejected ICA components",
    tilt_labels = FALSE,
    use_proportion = TRUE
  )
}
