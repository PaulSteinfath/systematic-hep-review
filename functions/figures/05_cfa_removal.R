figure_cfa_removal <- function(df, save_path, ext = "svg") {
  
  ica_usage_plot <- epoch_continuous_ica(df)
  cfa_criteria_plot <- summarize_cfa_criteria(df)
  cardiac_ics_plot <- rejected_cardiac_ics(df)
  other_strategies_plot <- other_strategy_plot(df)
  rr_plot <- rr_intervals_plot(df)
  minimal_artifact_plot <- minimal_artifact_windows_plot(df, t_peak_offset = 300)
  
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
  
  fig <- plot_grid(top_row, middle_row, bottom_row, ncol = 1)
  
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