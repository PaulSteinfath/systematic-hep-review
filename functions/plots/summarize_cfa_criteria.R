summarize_cfa_criteria <- function(df) {
  
  category_mapping <- c(
    "time course" = "Time Course",
    "topography" = "Topography",
    "power spectrum" = "Power Spectrum",
    "phase consistency" = "Phase\nConsistency",
    "iclabel" = "Algorithm",
    "corrmap" = "Algorithm",
    "correlation" = "Correlation",
    "sasica" = "Algorithm"
  )
  
  # pipelines with cfa_rej_criteria
  df_filt <- df %>%
    mutate(cfa_rej_criteria = tolower(cfa_rej_criteria)) %>%
    filter(cfa_rej_criteria != "", cfa_rej_criteria != "unknown") %>%
    distinct(PMID, cfa_rej_criteria) %>%
    mutate(pipeline_id = row_number())  
  
  total_pipelines <- nrow(df_filt)
  
  # expand and process the criteria
  criteria_expanded <- df_filt %>%
    separate_rows(cfa_rej_criteria, sep = ",") %>%
    mutate(
      cfa_rej_criteria = str_replace_all(cfa_rej_criteria, "[\r\n]", " "),
      cfa_rej_criteria = str_trim(cfa_rej_criteria, side = "both"),
      cfa_rej_criteria = case_when(
        cfa_rej_criteria %in% names(category_mapping) ~ category_mapping[cfa_rej_criteria],
        TRUE ~ cfa_rej_criteria
      )
    ) %>%
    filter(cfa_rej_criteria != "", cfa_rej_criteria != "unknown") %>%
    distinct(pipeline_id, cfa_rej_criteria, .keep_all = TRUE)

  # Create main plot data using pipeline counts
  main_counts <- criteria_expanded %>%
    group_by(cfa_rej_criteria) %>%
    summarise(
      count = n_distinct(pipeline_id),
      n_studies = n_distinct(PMID)
    ) %>%
    arrange(desc(count)) %>%
    mutate(prop = count / total_pipelines)

  # Create main plot
  main_plot <- ggplot(main_counts, 
    aes(x = reorder(cfa_rej_criteria, count, decreasing = TRUE), y = prop)) +
    geom_bar(stat = "identity", fill = "#696969", color = "white", linewidth = 0.5) +
    plot_theme_default +
    custom_theme() +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .1))) +
    labs(
      x = "",
      y = "Proportion of pipelines",
      title = "CFA Rejection Criteria",
      subtitle = paste("n =", total_pipelines, "pipelines")
    ) +
    theme(
      title = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.title.y = element_text(size = 9)
    )

  # Create algorithm inset data using pipeline IDs
  algo_data <- criteria_expanded %>%
    filter(cfa_rej_criteria == "Algorithm") %>%
    left_join(
      df_filt %>% 
        separate_rows(cfa_rej_criteria, sep = ",") %>%
        mutate(cfa_rej_criteria = str_trim(cfa_rej_criteria)) %>%
        filter(cfa_rej_criteria %in% c("iclabel", "corrmap", "sasica")),
      by = c("PMID", "pipeline_id")
    ) %>%
    filter(!is.na(cfa_rej_criteria.y)) %>%
    rename(algorithm = cfa_rej_criteria.y)

  # Exit immediately if not enough info for algo_plot
  if (nrow(algo_data) == 0) {
    return(main_plot)
  }

  # Calculate algorithm proportions
  algo_counts <- algo_data %>%
    group_by(algorithm) %>%
    summarise(count = n_distinct(pipeline_id)) %>%
    mutate(prop = count / total_pipelines)

  # Create algorithm inset plot
  algo_plot <- ggplot(algo_counts, 
    aes(x = algorithm, y = count)) +
    geom_bar(stat = "identity", fill = "#696969", color = "black", width = 0.7) +
    labs(x = "Algorithm", y = "Count") +
    scale_x_discrete(labels = function(x) sapply(x, function(xi) {
      case_match(
        xi,
        "corrmap" ~ "CORRMAP",
        "iclabel" ~ "ICLabel",
        "sasica" ~ "SASICA",
        .default = xi
      )
    })) +
    scale_y_continuous(expand = c(0, 0)) +
    plot_theme_default +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.title.x = element_text(margin = margin(t = 2, b = 2)),
      axis.title.y = element_text(margin = margin(r = 2, l = 2)),
      plot.background = element_rect(fill = alpha("white", 0), color = NA),
      panel.border = element_rect(color = "black", fill = NA)
    )

  # Combine plots
  combined_plot <- ggdraw() +
    draw_plot(main_plot) +
    draw_plot(algo_plot, x = 0.65, y = 0.45, width = 0.3, height = 0.5)

  return(combined_plot)
}
