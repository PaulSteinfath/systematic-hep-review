figure_meeg_acq_prep <- function(df, save_path = NULL, ext = 'png') {
  # NOTE: during preprocessing, some references merge into Other category,
  # which affects other measures such as entropy. Therefore, the 
  # preprocessing is performed right before making the plots
  df <- preprocess_reference(df)
  
  # Create individual histogtams for online / offline references
  ref_online <- hist_panel(df, "reference_online",
                           title = "Reference (online)",
                           discrete = TRUE, tilt_labels = F,
                           modality_filter = "EEG",
                           allowed = ref_mapping[online_ref_categories])
  
  ref_offline <- hist_panel(df, "reference_offline",
                            title = "Reference (offline)",
                            discrete = TRUE, tilt_labels = F,
                            modality_filter = "EEG",
                            allowed = ref_mapping[offline_ref_categories])
  
  # ICA usage and types of rejected components
  ica_rej_plot <- plot_rejected_components(df)
  ica_simple_plot <- hist_panel(df, "ICA", 
                                x.label = "",
                                title = "ICA usage",
                                discrete = T,
                                custom_labels = c("0" = "No ICA",
                                                  "1" = "ICA"))
  
  # Plot for filtering cutoffs
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
    rel_widths = c(1.2, 0.05, 0.9),
    vjust = 1
  )
  
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, paste0("fig3_meeg_acq_prep.", ext)),
      plot = fig,
      width = 10,
      height = 11,
      units = "in",
      dpi = 300,
      device = ext,
      bg = "white"
    )
  }
  
  return(fig)
}