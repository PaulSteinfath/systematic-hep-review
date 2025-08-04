figure_cfa_removal <- function(df, save_path = NULL, ext = "svg") {
  # Panel A: is ICA applied to continuous or epoched data?
  ica_usage_plot <- hist_panel(
    df %>% 
      distinct(PMID, ICA, ica_on_epochs) %>%
      filter(ICA == 1),
    col = "ica_on_epochs",
    title = "ICA usage",
    discrete = TRUE,
    custom_labels = c("0" = "Continuous", "1" = "Epoched")
  )
  
  # Panels B and C: approach and criteria for rejecting CFA-related ICs
  cfa_approach_plot <- hist_panel(df %>% 
                                    filter(reject_cfa_ics), 
                                  "cfa_rej_approach", 
                                  title = "Approach for rejecting CFA ICs", 
                                  discrete = T,
                                  allowed = allowed$cfa_approach)
  cfa_criteria_plot <- plot_cfa_criteria(df)
  cfa_algorithm_plot <- plot_cfa_algorithm(df)
  
  # Panel D: number of rejected CFA-related ICs
  cardiac_ics_plot <- hist_panel(
    df %>%
      distinct(PMID, rejected_cardiac_ics) %>%
      filter(!is.na(rejected_cardiac_ics)), 
    "rejected_cardiac_ics",
    x.label = "Number of components",
    title = "Number of rejected CFA ICs",
    discrete = FALSE,
    binwidth = 0.5
  )
  
  # Panels E-G: other strategies for removing CFA
  other_strategies_plot <- plot_other_cfa_strategy(df)
  rr_plot <- plot_rr_intervals(df)
  minimal_artifact_plot <- plot_minimal_artifact_windows(df, 
                                                         t_peak_offset = params$t_peak_offset)
  
  # Combine subplots into rows
  top_row <- plot_grid(
    ica_usage_plot,
    cfa_approach_plot,
    cfa_criteria_plot,
    nrow = 1, labels = c("A", "B", "C"),
    align = "hv",
    rel_widths = c(0.7, 1.1, 1.9)
  )
  
  middle_row <- plot_grid(
    cardiac_ics_plot,
    other_strategies_plot,
    nrow = 1, labels = c("D", "E"),
    align = "hv", rel_widths = c(0.3, 0.7)
  )
  
  bottom_row <- plot_grid(
    NULL,
    rr_plot,
    NULL,
    minimal_artifact_plot,
    nrow = 1, 
    labels = c("F", "", "G", ""),
    rel_widths = c(0.05, 1, 0.05, 1),
    align = "h",
    axis = "lr"
  )
  
  combined <- plot_grid(top_row, 
                        middle_row, 
                        bottom_row, 
                        ncol = 1,
                        align = "hv",
                        axis = "lr",
                        rel_heights = c(1, 1, 1.25))
  
  # Add algorithm as an inset plot
  fig <- ggdraw() +
    draw_plot(combined) +
    draw_plot(cfa_algorithm_plot, 
              x = 0.775, y = 0.85, 
              width = 0.225, height = 0.15)
  
  
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, paste0("fig5_cfa_removal.", ext)),
      plot = fig,
      width = 10,
      height = 9,
      units = "in",
      dpi = 300,
      device = ext,
      bg = "white"
    )
  }
  
  return(fig)
}