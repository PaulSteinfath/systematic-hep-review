figure_meeg_acq_prep <- function(df, save_path, ext = 'png') {

  # Create individual histogtams for online / offline references
  ref_online <- hist_panel(df, "reference_online",
                           title = "Reference (online)",
                           discrete = TRUE, tilt_labels = F,
                           modality_filter = "EEG",
                           allowed = ref_mapping[online_ref_categories]
  )
  
  ref_offline <- hist_panel(df, "reference_offline",
                            title = "Reference (offline)",
                            discrete = TRUE, tilt_labels = F,
                            modality_filter = "EEG",
                            allowed = ref_mapping[offline_ref_categories]
  )
  
  # Create plots for filtering cutoffs, ICA rejection, and ICA usage
  filter_plot <- plot_segments(
    df = df,
    start_var = "high_pass",
    end_var = "low_pass",
    x_scale = "log",
    custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
    x_label = "Filter cutoff (Hz)",
    y_label = "Individual studies",
    show_legend = TRUE 
  )
  
  ica_rej_plot <- plot_rejected_components(df)
  ica_simple_plot <- hist_panel(df, "ICA", 
                                x.label = "",
                                title = "ICA usage",
                                discrete = T,
                                custom_labels = c("0" = "No ICA",
                                                  "1" = "ICA"))
  
  plot_BC <- plot_grid( 
    ref_offline,
    ica_simple_plot,
    ncol = 2,
    align = "hv",
    axis = "tblr",
    labels = c("B", "C"),
    rel_widths = c(1.4, 0.6)
  )
  
  # Combine plots
  fig_ABCD <- plot_grid(
    plot_grid(ref_online, ncol = 1, labels = "A"),    
    NULL,                                              # Spacer
    plot_BC,                                          
    plot_grid(ica_rej_plot, ncol = 1, labels = "D"),  
    ncol = 1,
    align = "hv",
    axis = "tblr",
    vjust = 1,
    rel_heights = c(1, 0.05, 1, 1) # A, spacer, B&C, D
  )
  
  fig <- plot_grid(
    fig_ABCD,
    NULL,
    filter_plot,
    nrow = 1,
    labels = c("", "", "E"),
    rel_widths = c(1.2, 0.05, 1),
    vjust = 1
  )
  
  ggsave(
    filename = file.path(save_path, paste0("eeg_acq_prep_plot.", ext)),
    plot = fig,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}