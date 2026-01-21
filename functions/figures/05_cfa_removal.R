figure_cfa_removal <- function(df, save_path = NULL, ext = "svg") {
  # Panel A: is ICA applied to continuous or epoched data?
  ica_usage_plot <- hist_panel(
    df %>% 
      distinct(PMID, ICA, ica_on_epochs, authors) %>%
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
    cfa_approach_plot,
    cfa_criteria_plot,
    nrow = 1, labels = c("A", "B"),
    align = "hv",
    rel_widths = c(0.35, 0.65)
  )
  
  middle_row <- plot_grid(
    cardiac_ics_plot,
    other_strategies_plot,
    nrow = 1, labels = c("C", "D"),
    align = "hv", rel_widths = c(0.35, 0.65)
  )
  
  bottom_row <- plot_grid(
    NULL,
    rr_plot,
    NULL,
    minimal_artifact_plot,
    nrow = 1, 
    labels = c("E", "", "F", ""),
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
                        rel_heights = c(1, 1, 1.4))
  
  # Add algorithm as an inset plot
  fig <- ggdraw() +
    draw_plot(combined) +
    draw_plot(cfa_algorithm_plot, 
              x = 0.725, y = 0.85, 
              width = 0.275, height = 0.15)
  
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.95,  # height / width
                save_path,
                filename = "fig5_cfa_removal",
                ext = ext)
  }
  
  return(fig)
}