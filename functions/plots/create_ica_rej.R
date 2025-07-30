create_ica_rej <- function(df) {
  # Filter for ICA=1 and get distinct PMIDs with their rejected components
  df_ica <- df %>%
    filter(ICA == 1) %>%
    distinct(PMID, rejected_components) %>%
    filter(rejected_components != "", rejected_components != "unknown") %>%
    mutate(pipeline_id = row_number())  



  # Clean rejected_components: replace newlines with a comma, collapse multiple commas, and trim spaces/commas
  df_ica$rejected_components <- sapply(df_ica$rejected_components, function(x) {
    x <- gsub("[\r\n]+", ",", x)    
    x <- gsub(",+", ",", x)         
    x <- gsub("^,|,$", "", x)       
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
      rejected_components = component_mapping[rejected_components] %>% coalesce(rejected_components) # Map to standardized names
    ) %>% filter(rejected_components != "")

  counts_df <- df_long %>%
    group_by(rejected_components) %>%
    summarise(count = n_distinct(pipeline_id)) %>%
    arrange(desc(count)) %>%
    mutate(prop = count / pipeline_n)


  p <- ggplot(counts_df, aes(x = reorder(rejected_components, count, decreasing = TRUE),
                             y = prop)) +
    geom_bar(stat = "identity", fill = "#696969", color = "white", linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .1))) +
    labs(
      x="",
      y = "Proportion of pipelines",
      title = "Types of Rejected ICA Components",
      subtitle = paste("n =", pipeline_n, "pipelines")
    ) + plot_theme_default 

  return(p)
}
