figure_meeg_acq_prep <- function(df, save_path, ext = 'png') {
  # Define major reference categories once
  major_categories <- tolower(c(
    "Cz", "Nose", "Linked earlobes", "Linked mastoids",
    "FCz", "Common average", "Fpz", "CMS", "CMS and DRL",
    "unknown", "Laplacian reference", "REST"
  ))
  
  # Process reference categories
  df_ref <- df %>%
    mutate(across(
      .cols = c(reference_online, reference_offline),
      .fns = ~ case_when(
        tolower(.) %in% major_categories ~ tolower(.),
        TRUE ~ "Other"
      )
    ))
  
  # Common reference category mapping
  ref_categories <- c(
    "Common average" = "CAR",
    "Linked mastoids" = "LM",
    "Linked earlobes" = "LE",
    "Cz" = "Cz",
    "FCz" = "FCz",
    "Fpz" = "Fpz",
    "CMS" = "CMS",
    "CMS and DRL" = "CMS",
    "Nose" = "Nose",
    "Laplacian reference" = "LAP",
    "REST" = "REST",
    "Other" = "Other",
    "unknown" = "N/M"
  )
  
  # Create individual histogtams for online / offline references
  ref_online <- hist_panel(df_ref, "reference_online",
                           title = "Reference (online)",
                           discrete = TRUE, tilt_labels = F,
                           modality_filter = "EEG",
                           allowed = ref_categories[c(
                             "Common average", "Linked mastoids", "Cz", "FCz",
                             "Fpz", "CMS and DRL", "CMS", "Nose",
                             "Linked earlobes", "Other", "unknown"
                           )]
  )
  
  ref_offline <- hist_panel(df_ref, "reference_offline",
                            title = "Reference (offline)",
                            discrete = TRUE, tilt_labels = F,
                            modality_filter = "EEG",
                            allowed = ref_categories[c(
                              "Common average", "Linked mastoids", "Linked earlobes",
                              "Laplacian reference", "unknown", "Other"
                            )]
  )
  
  # Create plots for filtering cutoffs, ICA rejection, and ICA usage
  filter_plot <- create_filter_plots(df)
  ica_rej_plot <- create_ica_rej(df)
  ica_simple_plot <- create_simple_ica_plot(df)
  
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