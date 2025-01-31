create_ica_rej <- function(df) {
  # Filter for ICA=1 and get distinct PMIDs with their rejected components
  df_ica <- df %>%
    filter(ICA == 1) %>%
    distinct(PMID, rejected_components)

  # Replace newline characters with commas
  df_ica$rejected_components <- gsub("\n", ",", df_ica$rejected_components)

  # Remove multiple commas and trim whitespace and commas
  df_ica$rejected_components <- df_ica$rejected_components %>%
    gsub(",+", ",", .) %>%
    gsub("^,+|,+$", "", .) %>%
    trimws()

  df_long <- df_ica %>%
    separate_rows(rejected_components, sep = ",") %>%
    mutate(
      rejected_components = trimws(rejected_components),
      rejected_components = tolower(rejected_components)
    )

  # Create a mapping of common variations to standardized names
  component_mapping <- c(
    "eye movements" = "Eye Movements",
    "blinks" = "Blinks",
    "muscle" = "Muscle",
    "cfa" = "CFA",
    "channel noise" = "Channel Noise",
    "line noise" = "Line Noise",
    "other" = "Other"
  )

  # Apply the mapping and create plot
  df_long %>%
    mutate(
      rejected_components = component_mapping[rejected_components] %>%
        coalesce(rejected_components)
    ) %>%
    group_by(rejected_components) %>%
    summarise(Frequency = n()) %>%
    arrange(desc(Frequency)) %>%
    filter(!is.na(rejected_components), rejected_components != "") %>%
    ggplot(aes(x = reorder(rejected_components, Frequency), y = Frequency)) +
    geom_bar(stat = "identity", fill = "#696969") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(
      # title = "Frequency of Rejected ICA Components",
      # subtitle = paste("n=", nrow(df_ica), " pipelines"),
      y = "Number of Pipelines",
      x = ""
      #x = paste("Rejected ICA Component types ", "n=", nrow(df_ica), " pipelines"), 
    ) +
    theme_classic(base_family = "sans") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.title = element_text(size = 10)
    )
}
