figure_ecg_summary <- function(df, save_path = NULL, ext = 'png') {
  # Map "unknown" to 9 so that it isn't lost during conversion to numeric and
  # is positioned nicely
  df <- df %>%
    mutate(ecg_num_electrodes = replace(ecg_num_electrodes,
                                        ecg_num_electrodes == "unknown", 9))
  
  # ECG acquisition
  p_ecg_num_electrodes <- hist_panel(df, "ecg_num_electrodes", 
                                     force.numeric = T, binwidth = 1,
                                     x.label = "Number of ECG electrodes",
                                     title = "Number of ECG electrodes") +
    scale_x_continuous(breaks = seq(0, 9),
                       labels = c(seq(0, 8), "N/M"))
  p_ecg_leads <- hist_panel(df, "ecg_lead", fill_as_aesthetic = T,
                            discrete = T, title = "ECG lead") +
    scale_fill_manual(values = leads_palette,
                      na.value = common_colors$fill_default,
                      guide = "none") 
  p_ecg_locations <- plot_ecg_locations(df, leads_palette)
  
  # ECG preprocessing
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
  
  methods_to_display <- df %>% 
    distinct(PMID, ecg_event_method) %>% 
    group_by(ecg_event_method) %>% 
    summarize(count = n()) %>% 
    filter(ecg_event_method != "unknown", 
           count >= params$ecg_method_min_papers) %>%
    pull(ecg_event_method)
  p_method <- hist_panel(
    df %>%
      filter(ecg_event_method %in% methods_to_display),
    "ecg_event_method",
    title = "Method for detecting ECG events (R-/T-peak)",
    custom_labels = c(
      "Template matching" = "Template\nmatching",
      "PanTompkins1985" = "Pan &\nTompkins (1985)",
      "Niazy2005" = "Niazy et al.\n(2005)",
      "deCarvalho2002" = "de Carvalho\net al. (2002)",
      "Derivative sign" = "Derivative\nsign",
      "Librow" = "Librow"
    ),
    discrete = T
  )
  
  toolboxes_to_display <- df %>% 
    distinct(PMID, ecg_event_toolbox) %>% 
    group_by(ecg_event_toolbox) %>% 
    summarize(count = n()) %>% 
    filter(!(ecg_event_toolbox %in% c("none", "unknown")), 
           count >= params$ecg_toolbox_min_papers) %>%
    pull(ecg_event_toolbox)
  p_toolbox <- hist_panel(
    df %>%
      filter(ecg_event_toolbox %in% toolboxes_to_display),
    "ecg_event_toolbox",
    title = "Toolbox / function for detecting ECG events",
    custom_labels = c(
      "HEPLAB" = "HEPLAB",
      "Peakfinder" = "Peakfinder",
      "FMRIB" = "FMRIB",
      "BrainVision Analyzer" = "BrainVision\nAnalyzer",
      "Kubios" = "Kubios",
      "WinCPRS" = "WinCPRS",
      "Librow" = "Librow",
      "Neurokit2" = "Neurokit2"
    ),
    discrete = T
  )
  
  fig_ABD = plot_grid(
    p_ecg_num_electrodes,
    p_ecg_leads,
    NULL,
    p_filter_ind,
    p_filter_density,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.45, 0.45, 0.05, 0.65, 0.35),
    labels = c("A", "B", "", "D", "")
  )
  
  fig_CEF <- plot_grid(
    p_ecg_locations,
    NULL, 
    p_method, 
    NULL,
    p_toolbox, 
    ncol = 1,
    rel_heights = c(0.9, 0.05, 0.485, 0.03, 0.485),
    labels = c("C", "", "E", "", "F"),
    align = 'v',
    axis = 'l'
  )
  
  fig <- plot_grid(
    fig_ABD,
    NULL, 
    fig_CEF,
    nrow = 1,
    rel_widths = c(1, 0.05, 1.5),
    align = 'v',
    axis = 'l'
  )
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 1.05,  # height / width
                save_path,
                filename = "fig4_ecg_summary",
                ext = ext)
  }
  
  return(fig)
}
