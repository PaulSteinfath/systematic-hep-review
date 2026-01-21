figure_sampling_frequency <- function(df, save_path, ext = 'png') {
  # NOTE: using log-scale in all panels
  
  p_meeg_title <- ggdraw() +
    draw_label("M/EEG sampling frequency", fontface = "bold")
  
  df$meeg_sfreq_orig_log <- log10(df$meeg_sfreq_orig)
  p_meeg_orig <- hist_panel(df, "meeg_sfreq_orig_log", discrete = F, 
                            title = "During the recording", 
                            x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  df$meeg_sfreq_final_log <- log10(df$meeg_sfreq_final)
  p_meeg_final <- hist_panel(df, "meeg_sfreq_final_log", discrete = F, 
                             title = "In the offline analysis", 
                             x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  p_ecg_title <- ggdraw() +
    draw_label("ECG sampling frequency", fontface = "bold")
  
  df$ecg_sfreq_orig_log <- log10(df$ecg_sfreq_orig)
  p_ecg_orig <- hist_panel(df, "ecg_sfreq_orig_log", discrete = F, 
                           title = "During the recording", 
                           x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  df$ecg_sfreq_final_log <- log10(df$ecg_sfreq_final)
  p_ecg_final <- hist_panel(df, "ecg_sfreq_final_log", discrete = F, 
                            title = "In the offline analysis", 
                            x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  
  p_meeg_sfreq <- plot_grid(
    p_meeg_orig, p_meeg_final,
    ncol = 2,
    labels = c('A', 'B')
  )
  p_ecg_sfreq <- plot_grid(
    p_ecg_orig, p_ecg_final,
    ncol = 2,
    labels = c('C', 'D')
  )
  
  fig <- plot_grid(
    p_meeg_title,
    p_meeg_sfreq,
    NULL,
    p_ecg_title,
    p_ecg_sfreq,
    ncol = 1,
    rel_heights = c(0.2, 1, 0.1, 0.2, 1)
  )
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.7,  # height / width
                save_path,
                filename = "figS3_sampling_frequency",
                ext = ext)
  }
  
  return(fig)
}