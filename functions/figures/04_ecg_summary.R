figure_ecg_summary <- function(df, save_path = NULL, ext = 'png') {
  # Map "unknown" to 9 so that it isn't lost during conversion to numeric and
  # is positioned nicely
  df <- df %>%
    mutate(ecg_num_electrodes = replace(ecg_num_electrodes,
                                        ecg_num_electrodes == "unknown", 9))
  
  # ECG acquisition
  p_ecg_acquisition <- ggdraw() +
    draw_label("ECG acquisition parameters", x = 0.5, y = 0.5, fontface = "bold")
  p_ecg_num_electrodes <- hist_panel(df, "ecg_num_electrodes", 
                                     force.numeric = T, binwidth = 1,
                                     x.label = "Number of ECG electrodes",
                                     title = "Number of ECG electrodes") +
    scale_x_continuous(breaks = seq(0, 9),
                       labels = c(seq(0, 8), "N/M"))
  p_ecg_leads <- hist_panel(df, "ecg_lead", fill_as_aesthetic = T,
                            discrete = T, title = "ECG lead", mark_offset = 0.08) +
    scale_fill_manual(values = leads_palette,
                      na.value = common_colors$fill_default,
                      guide = "none") 
  p_ecg_locations <- plot_ecg_locations(df, leads_palette)
  
  # ECG preprocessing
  p_ecg_preproc <- ggdraw() +
    draw_label("ECG preprocessing parameters", x = 0.5, y = 0.5, fontface = "bold")
  c(p_filter_ind, p_filter_density) %<-% plot_segments(
    df,
    start_var = "ecg_high_pass",
    end_var = "ecg_low_pass",
    x_scale = "log",
    custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
    x_label = "Filter cutoff (Hz)",
    rel_heights = c(1, 0.2, 0.01, 0.2, 0.05),
    labels = c('', '', '', '', ''),
    combined = F
  )
  
  df$ecg_event_method <- merge_into_other(df, "PMID", "ecg_event_method",
                                          params$ecg_method_min_papers)
  p_method <- hist_panel(
    df %>%
      filter(ecg_event_method != "unknown"),
    "ecg_event_method",
    title = "Algorithm for ECG peak detection",
    custom_labels = c(
      "Template matching" = "Template\nmatching",
      "PanTompkins1985" = "Pan &\nTompkins (1985)",
      "Niazy2005" = "Niazy et al.\n(2005)",
      "deCarvalho2002" = "de Carvalho\net al. (2002)",
      "Derivative sign" = "Derivative\nsign",
      "Librow" = "Librow",
      "Other" = "Other"
    ),
    discrete = T,
    mark_offset = 0.07
  )
  
  df$ecg_event_toolbox <- merge_into_other(df, "PMID", "ecg_event_toolbox",
                                           params$ecg_toolbox_min_papers)
  p_toolbox <- hist_panel(
    df %>%
      filter(!(ecg_event_toolbox %in% c("none", "unknown"))),
    "ecg_event_toolbox",
    title = "Software for ECG peak detection",
    custom_labels = c(
      "HEPLAB" = "HEPLAB",
      "Peakfinder" = "Peakfinder",
      "FMRIB" = "FMRIB",
      "BrainVision Analyzer" = "BrainVision\nAnalyzer",
      "Kubios" = "Kubios",
      "WinCPRS" = "WinCPRS",
      "Librow" = "Librow",
      "Neurokit2" = "Neurokit2",
      "Other" = "Other"
    ),
    discrete = T,
    mark_offset = 0.07
  )
  
  fig_AB = plot_grid(
    p_ecg_num_electrodes,
    p_ecg_leads,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.5, 0.5),
    labels = c("A", "B")
  )
  
  fig_ABC <- plot_grid(
    fig_AB,
    NULL, 
    p_ecg_locations,
    nrow = 1,
    labels = c('', 'C', ''),
    rel_widths = c(1, 0.15, 1.25),
    align = 'v',
    axis = 'l'
  )
  
  fig_D = plot_grid(
    p_filter_ind,
    p_filter_density,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.65, 0.35)
  )
  
  fig_EF <- plot_grid(
    p_method, 
    NULL,
    p_toolbox, 
    ncol = 1,
    rel_heights = c(0.485, 0.03, 0.485),
    labels = c("E", "", "F"),
    align = 'v',
    axis = 'l'
  )
  
  fig_DEF <- plot_grid(
    NULL,
    fig_D,
    NULL, 
    fig_EF,
    nrow = 1,
    rel_widths = c(0.075, 0.9, 0.05, 1.7),
    labels = c("D", "", "", ""),
    align = 'v',
    axis = 'l'
  )
  
  fig <- plot_grid(
    p_ecg_acquisition,
    fig_ABC,
    NULL,
    p_ecg_preproc,
    fig_DEF,
    ncol = 1,
    rel_heights = c(0.1, 0.9, 0.025, 0.1, 1.025),
    align = 'v',
    axis = 'l'
  )
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 1.1,  # height / width
                save_path,
                filename = "fig4_ecg_summary",
                ext = ext)
  }
  
  return(fig)
}
