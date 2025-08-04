figure_cfa_removal <- function(df, save_path, ext = "svg") {
  # Panel A: is ICA applied to continuous or epoched data?
  ica_usage_plot <- hist_panel(
    df %>% 
      distinct(PMID, ICA, ica_on_epochs) %>%
      filter(ICA == 1),
    col = "ica_on_epochs",
    title = "ICA Usage",
    discrete = TRUE,
    custom_labels = c("0" = "Continuous", "1" = "Epoched")
  )
  
  cfa_criteria_plot <- summarize_cfa_criteria(df)
  
  # Panel C: number of rejected CFA-related ICs
  cardiac_ics_plot <- hist_panel(
    df %>%
      distinct(PMID, rejected_cardiac_ics) %>%
      filter(!is.na(rejected_cardiac_ics)), 
    "rejected_cardiac_ics",
    x.label = "",
    title = "Number of rejected CFA-related ICs",
    discrete = FALSE,
    binwidth = 1
  )
  
  other_strategies_plot <- plot_other_cfa_strategy(df)
  rr_plot <- plot_rr_intervals(df)
  minimal_artifact_plot <- plot_minimal_artifact_windows(df, 
                                                         t_peak_offset = params$t_peak_offset)
  
  # Combine subplots into rows
  top_row <- plot_grid(
    ica_usage_plot,
    cfa_criteria_plot,
    ncol = 2, labels = c("A", "B"),
    align = "v", axis = "b",
    rel_widths = c(0.25, 0.75),
    label_x = c(0, 0.07),  
    label_y = c(1, 1)
  )
  
  middle_row <- plot_grid(
    cardiac_ics_plot,
    other_strategies_plot,
    ncol = 2, labels = c("C", "D"),
    align = "hv", rel_widths = c(0.4, 0.6)
  )
  
  bottom_row <- plot_grid(
    rr_plot,
    minimal_artifact_plot,
    ncol = 2, labels = c("E", "F"),
    align = "hv"
  )
  
  fig <- plot_grid(top_row, 
                   middle_row, 
                   bottom_row, 
                   ncol = 1,
                   align = "h",
                   axis = "l")
  
  ggsave(
    filename = file.path(save_path, paste0("cfa_removal_plot.", ext)),
    plot = fig,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}