figure_sampling_frequency <- function() {
  # sfreq
  df_included$meeg_sfreq_final <- case_when(
    df_included$meeg_sfreq_final == "unknown" ~ df_included$meeg_sfreq_orig,
    .default = df_included$meeg_sfreq_final
  )
  df_included$ecg_sfreq_orig <- case_when(
    df_included$ecg_sfreq_orig == "unknown" ~ df_included$ecg_sfreq_orig,
    .default = df_included$ecg_sfreq_orig
  )
  df_included$ecg_sfreq_final <- case_when(
    df_included$ecg_sfreq_final == "unknown" ~ df_included$ecg_sfreq_orig,
    .default = df_included$ecg_sfreq_final
  )
  
  df_included$meeg_sfreq_orig <- as.numeric(df_included$meeg_sfreq_orig)
  df_included$meeg_sfreq_final <- as.numeric(df_included$meeg_sfreq_final)
  
  df_included$ecg_sfreq_orig <- as.numeric(df_included$ecg_sfreq_orig)
  df_included$ecg_sfreq_final <- as.numeric(df_included$ecg_sfreq_final)
  
  df_included$meeg_sfreq_orig_log <- log10(df_included$meeg_sfreq_orig)
  p_meeg_orig <- hist_panel(df_included, "meeg_sfreq_orig_log", discrete = F, 
                            title = "M/EEG sampling frequency (original)", 
                            x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  df_included$meeg_sfreq_final_log <- log10(df_included$meeg_sfreq_final)
  p_meeg_final <- hist_panel(df_included, "meeg_sfreq_final_log", discrete = F, 
                             title = "M/EEG sampling frequency (final)", 
                             x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  df_included$ecg_sfreq_orig_log <- log10(df_included$ecg_sfreq_orig)
  p_ecg_orig <- hist_panel(df_included, "ecg_sfreq_orig_log", discrete = F, 
                           title = "ECG sampling frequency (original)", 
                           x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  df_included$ecg_sfreq_final_log <- log10(df_included$ecg_sfreq_final)
  p_ecg_final <- hist_panel(df_included, "ecg_sfreq_final_log", discrete = F, 
                            title = "ECG sampling frequency (final)", 
                            x.label = "Sampling frequency", bins = 12) + 
    scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)), 
                       labels = \(x) round(10^(x), 0)) +
    expand_limits(x = c(2, 4))
  
  
  p_sfreq <- plot_grid(
    p_meeg_orig, p_meeg_final,
    p_ecg_orig, p_ecg_final,
    ncol = 2
  )
}