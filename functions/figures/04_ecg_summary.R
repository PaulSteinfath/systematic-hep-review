figure_ecg_summary <- function(df, save_path, ext = 'png') {
  # Map "unknown" to 9 so that it isn't lost during conversion to numeric and
  # is positioned nicely
  df <- df %>%
    mutate(ecg_num_electrodes = replace(ecg_num_electrodes,
                                        ecg_num_electrodes == "unknown", 9))
  p_ecg_num_electrodes <- hist_panel(df, "ecg_num_electrodes", 
                                     force.numeric = T, binwidth = 1,
                                     x.label = "Number of ECG electrodes",
                                     title = "Number of ECG electrodes") +
    scale_x_continuous(breaks = seq(0, 9),
                       labels = c(seq(0, 8), "N/M"))
  p_ecg_leads <- hist_panel(df, "ecg_lead", fill_as_aesthetic = T,
                            discrete = T, title = "ECG lead") +
    scale_fill_manual(values = leads_palette,
                      na.value = plot_fill_default_single,
                      guide = "none") 
  p_ecg_locations <- plot_ecg_locations(df, leads_palette)
  fig_AB = plot_grid(
    p_ecg_num_electrodes,
    p_ecg_leads,
    ncol = 1,
    labels = c("A", "B")
  )
  
  fig <- plot_grid(
    fig_AB,
    p_ecg_locations,
    nrow = 1,
    rel_widths = c(1, 1.5),
    labels = c("", "C")
  )
  
  ggsave(
    filename = file.path(save_path, paste0("fig4_ecg_summary.", ext)),
    plot = fig,
    width = 10,
    height = 4,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
