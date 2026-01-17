figure_ecg_summary <- function(df, save_path = NULL, ext = 'png') {
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
                      na.value = common_colors$fill_default,
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
    NULL, 
    p_ecg_locations,
    nrow = 1,
    rel_widths = c(1, 0.1, 1.4),
    labels = c("", "C", ""),
    label_x = c(0, -0.1, 0)
  )
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.5,  # height / width
                save_path,
                filename = "fig4_ecg_summary",
                ext = ext)
  }
  
  return(fig)
}


# ecg preprocessing
# df_included$ecg_highpass <- as.numeric(df_included$ecg_highpass)
# df_included$ecg_lowpass <- as.numeric(df_included$ecg_lowpass)
# 
# df_included$ecg_detect_method <- case_when(
#   df_included$ecg_detect_method == "PanTompkins1985, modified" ~ "PanTompkins85",
#   df_included$ecg_detect_method == "PanTompkins1985" ~ "PanTompkins85",
#   df_included$ecg_detect_method == "PanTompkins" ~ "PanTompkins85",
#   df_included$ecg_detect_method == "template matching" ~ "Template matching",
#   df_included$ecg_detect_method == "sym4 wavelet" ~ "Template matching",
#   df_included$ecg_detect_method == "Template matching (Vehkaoja et al., 2013)" ~ "Template matching",
#   .default = df_included$ecg_detect_method
# )
# 
# df_included$ecg_detect_toolbox <- case_when(
#   df_included$ecg_detect_toolbox == "Neurokit2" ~ "Neurokit",
#   df_included$ecg_detect_toolbox == "AcqKnowledge, Kubios" ~ "AcqKnowledge",
#   .default = df_included$ecg_detect_toolbox
# )
# 
# p_filter <- plot_segments(
#   df_included, 
#   start_var = "ecg_highpass", 
#   end_var = "ecg_lowpass", 
#   x_scale = "log", 
#   custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
#   x_label = "Filter cutoff (Hz)",
#   rel_heights = c(1, 0.2, 0, 0, 0),
#   labels = c('', '', '', '', '')
# )
# 
# p_approach <- hist_panel(df_included, "ecg_detect_approach", title = "Approach", discrete = T)
# p_method <- hist_panel(
#   df_included[df_included$ecg_detect_method != "unknown",], 
#   "ecg_detect_method", 
#   title = "Detection method",
#   allowed = c(
#     "PanTompkins85" = "Pan &\nTompkins",
#     "Template matching" = "Template\nmatching"
#   ),
#   discrete = T
# )
# p_toolbox <- hist_panel(
#   df_included[!(df_included$ecg_detect_toolbox %in% c("none", "unknown")),], 
#   "ecg_detect_toolbox", 
#   title = "Toolbox / function for detecting ECG events",
#   allowed = c(
#     "HEPLAB" = "HEPLAB", 
#     "FMRIB" = "FMRIB", 
#     "Peakfinder" = "Peakfinder", 
#     "Neurokit" = "Neurokit", 
#     "Kubios" = "Kubios"
#   ),
#   discrete = T
# )
# 
# p1 <- plot_grid(p_approach, p_method, nrow = 1, rel_widths = c(1.5, 1))
# p2 <- plot_grid(p1, p_toolbox, ncol = 1)
# p3 <- plot_grid(p_filter, p2, nrow = 1, rel_widths = c(1, 1.75))
# p3
