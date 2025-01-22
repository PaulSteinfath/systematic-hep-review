library(cowplot)
library(tidyr)  # Add this for drop_na function

# Import plotting functions
source(file.path(func_path, "plots", "combined_cutoffs_plot.r"))
source(file.path(func_path, "plots", "hist_panel.r"))

# Create filter cutoff plots
create_filter_plots <- function(df) {
  filter_plot <- create_combined_plot(
    df = df,
    start_var = "high_pass",
    end_var = "low_pass",
    x_scale = "log",
    custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
    x_label = "Filter Cutoff (Hz)",
    y_label = "Individual Studies",
    font_family = "sans"
  )
}

# Create overview histogram panel plot
create_overview_panel <- function(df) {

  # Define major reference categories in lowercase
  major_categories <- tolower(c("Cz", "Nose", "Linked earlobes", "Linked mastoids", 
                              "FCz", "Common average", "Fpz", "CMS", "CMS and DRL", 
                              "unknown", "Laplacian reference"))

  df <- df %>%
    mutate(
      reference_online = ifelse(
        tolower(reference_online) %in% major_categories,
        reference_online,
        "Other"
      ),
      reference_online = ifelse(reference_online == "Other", 
                              "Other", 
                              tolower(reference_online))
    )

  df <- df %>%
    mutate(
      reference_offline = ifelse(
        tolower(reference_offline) %in% major_categories,
        reference_offline, 
        "Other"            
      ),
      reference_offline = ifelse(reference_offline == "Other", 
                               "Other", 
                               tolower(reference_offline))
    )

  # plotting function
  p <- plot_grid(
    hist_panel(df, 'Year', binwidth = 1),
    hist_panel(df, 'sample_size', x.label = 'Sample size', use.log10 = TRUE),
    hist_panel(df, 'rsHEP', x.label = 'Resting-state HEP', discrete = TRUE),
    hist_panel(df, 'Modality', discrete = TRUE, 
              allowed = c('EEG' = 'EEG', 'MEG' = 'MEG')),
    hist_panel(df, 'ecg_lead', discrete = TRUE, x.label = 'ECG Lead', 
              tilt_labels = TRUE,
              allowed = c(
                'Lead I' = 'Lead I',
                'Lead II' = 'Lead II', 
                'Lead III' = 'Lead III',
                'Multiple Leads' = 'Multiple Leads',
                'Multiple Leads (including Lead I)' = 'Multiple Leads',
                'Multiple Leads (including Lead I, II, III)' = 'Multiple Leads',
                'Multiple Leads (including Lead II)' = 'Multiple Leads',
                'single-channel' = 'Single Channel',
                'none' = 'None',
                'unknown' = 'Unknown'
              )),
    hist_panel(df, 'ecg_num_electrodes', x.label = 'Number of ECG channels',
              discrete = TRUE, drop.na = FALSE),
    hist_panel(df, 'channels', x.label = 'Number of EEG channels', 
              force.numeric = TRUE, use.log2 = TRUE, 
              modality_filter = 'EEG'),
    hist_panel(df, 'reference_online', x.label = 'Reference (online)', 
              discrete = TRUE, tilt_labels = TRUE, 
              modality_filter = 'EEG',
              allowed = c('Common average' = 'CAR', 'Linked mastoids' = 'LM',
                        'Cz' = 'Cz', 'FCz' = 'FCz', 'Fpz' = 'Fpz',
                        'CMS' = 'CMS', 'CMS and DRL' = 'CMS', 
                        'Nose' = 'Nose', 'Linked earlobes' = 'LE', 
                        'Other' = 'other', 'unknown' = 'unknown')),
    hist_panel(df, 'reference_offline', x.label = 'Reference (offline)', 
              discrete = TRUE, tilt_labels = TRUE, 
              modality_filter = 'EEG',
              allowed = c('Common average' = 'CAR', 'Linked mastoids' = 'LM',
                        'Linked earlobes' = 'LE', 'REST' = 'REST', 
                        'Cz' = 'Cz', 'Laplacian reference' = 'LAP', 
                        'unknown' = 'unknown', 'other' = 'other')),
    hist_panel(df, 'ICA', discrete = TRUE),
    hist_panel(df, 'cfa_rej_approach', x.label = 'CFA Rejection Approach', 
              discrete = TRUE,
              allowed = c("Manual" = "Manual", "Automatic" = "Auto",
                        "Semi-automatic" = "Semi", "Unknown" = "Unknown")),
    hist_panel(df, 'hep_relative_to', discrete = TRUE, 
              x.label = 'Relative to',
              allowed = c('R-peak' = 'R-peak', 'T-peak' = 'T-peak')),
    ncol = 3, nrow = 4, align = 'hv', axis = 'l'
  )
  return(p)
}

# Generate all figures
make_figures <- function(df, save_path) {
  filter_plot <- create_filter_plots(df)
  overview_plot <- create_overview_panel(df)
  
  # Save plots with consistent parameters
  ggsave(
    filename = file.path(save_path, "combined_hp_lp_cutoffs_density.svg"),
    plot = filter_plot,
    width = 4,
    height = 5.2,
    units = "in",
    dpi = 300,
    device = "svg"
  )
  
  ggsave(
    filename = file.path(save_path, "overview_plots.svg"),
    plot = overview_plot,
    width = 12,
    height = 12,
    units = "in",
    dpi = 300,
    device = "svg"
  )
}