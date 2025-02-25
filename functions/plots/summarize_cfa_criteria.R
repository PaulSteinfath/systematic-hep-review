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
  
  criteria_expanded <- df %>%
    separate_rows(cfa_rej_criteria, sep = ",") %>%
    mutate(
      cfa_rej_criteria = tolower(str_trim(cfa_rej_criteria, side = "both")),
      cfa_rej_criteria = case_when(
        cfa_rej_criteria %in% names(category_mapping) ~ category_mapping[cfa_rej_criteria],
        TRUE ~ cfa_rej_criteria
      )
    ) %>%
    filter(cfa_rej_criteria != "", cfa_rej_criteria != "unknown") %>%
    distinct(PMID, cfa_rej_criteria, .keep_all = TRUE)
  
  main_plot <- hist_panel(criteria_expanded,
                 col = "cfa_rej_criteria",
                 group_col = "PMID",
                 discrete = TRUE,
                 x.label = "CFA rejection criteria")
  
  algo_data <- df %>%
    separate_rows(cfa_rej_criteria, sep = ",") %>%
    mutate(cfa_rej_criteria = tolower(str_trim(cfa_rej_criteria, side = "both"))) %>%
    filter(cfa_rej_criteria %in% c("iclabel", "corrmap", "sasica")) %>%
    distinct(PMID, cfa_rej_criteria, .keep_all = TRUE)
  
  algo_plot <- ggplot(algo_data, aes(x = cfa_rej_criteria)) +
      geom_bar(fill = "#696969", color = "black", width = 0.7) +
      labs(x = "Algorithm", y = "Count") +
      scale_x_discrete(labels = function(x) sapply(x, function(xi) {
        if(xi == "corrmap") "Corrmap" else if(xi == "iclabel") "IClabel" else if(xi == "sasica") "SASICA" else xi
      })) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal(base_size = 9) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.title.x = element_text(margin = margin(t = 2, b = 2)),
            axis.title.y = element_text(margin = margin(r = 2, l = 2)),
            plot.background = element_rect(fill = "white", color = NA),
            panel.border = element_rect(color = "black", fill = NA))
  
  combined_plot <- ggdraw() +
      draw_plot(main_plot) +
      draw_plot(algo_plot, x = 0.65, y = 0.65, width = 0.3, height = 0.3)
  
  return(combined_plot)
}
