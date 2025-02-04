library(cowplot)
library(tidyr)
library(dplyr)
library(stringr)
library(gridGraphics)

# Import plotting functions
source(file.path(func_path, "plots", "create_combined_plot.r"))
source(file.path(func_path, "plots", "hist_panel.r"))
source(file.path(func_path, "plots", "create_ica_usage_plot.R"))
source(file.path(func_path, "plots", "create_ica_rej.R"))
source(file.path(func_path, "plots", "create_time_windows_plot.r"))


  # Create and combine time window plots with aligned axes
  # Calculate overall x-axis limits from all three time windows
  all_limits <- rbind(
    df %>% select(start = hep_start, end = hep_end),
    df %>% select(start = baseline_start_ms, end = baseline_end_ms),
    df %>% select(start = significant_start_ms, end = significant_end_ms)
  ) %>%
    summarise(
      min = min(as.numeric(start), na.rm = TRUE),
      max = max(as.numeric(end), na.rm = TRUE)
    )
    
  shared_limits <- c(all_limits$min, all_limits$max)
  
  hep_windows_plot <- create_time_windows_plot(df, 
    "hep_start", "hep_end", "hep_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits)
  
  baseline_windows_plot <- create_time_windows_plot(df,
    "baseline_start_ms", "baseline_end_ms", "hep_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits)

  significant_windows_plot <- create_time_windows_plot(df,
    "significant_start_ms", "significant_end_ms", "significant_relative_to",
    "Time relative to R-peak (ms)",
    t_peak_offset = 300,
    x_limits = shared_limits)
  
  time_windows_plot <- plot_grid(
    hep_windows_plot,
    baseline_windows_plot,
    significant_windows_plot,
    ncol = 1,
    labels = c("A", "B", "C"),
    align = "v"
  )
  


# Create filter cutoff plots
create_filter_plots <- function(df) {
  filter_plot <- create_combined_plot(
    df = df,
    start_var = "high_pass",
    end_var = "low_pass",
    x_scale = "log",
    custom_breaks = c(0.01, 0.1, 0.5, 1, 20, 40, 80),
    x_label = "Filter Cutoff (Hz)",
    y_label = "Individual Studies"
  )
  return(filter_plot)
}


# Create EEG Acquisition & Preprocessing
eeg_aq_prep <- function(df) {
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
    "Other" = "other",
    "unknown" = "unknown"
  )

  # Create individual histogtams for online / offline references
  ref_online <- hist_panel(df_ref, "reference_online",
    x.label = "Reference (online)",
    discrete = TRUE, tilt_labels = TRUE,
    modality_filter = "EEG",
    allowed = ref_categories[c(
      "Common average", "Linked mastoids", "Cz", "FCz",
      "Fpz", "CMS and DRL", "CMS", "Nose",
      "Linked earlobes", "Other", "unknown"
    )]
  )

  ref_offline <- hist_panel(df_ref, "reference_offline",
    x.label = "Reference (offline)",
    discrete = TRUE, tilt_labels = TRUE,
    modality_filter = "EEG",
    allowed = ref_categories[c(
      "Common average", "Linked mastoids", "Linked earlobes",
      "Laplacian reference", "unknown", "Other"
    )]
  )

  # Create plots for filtering cutoffs and ICA rejection
  filter_plot <- create_filter_plots(df)
  ica_rej_plot <- create_ica_rej(df)

  # Combine reference plots
  ref_plots <- plot_grid(
    ref_online, ref_offline,
    ncol = 1,
    align = "v"
  )

  # Combine reference plots with filter plot
  plot_grid(
    ref_plots, filter_plot,
    ncol = 2,
    labels = c("A", "B"),
    align = "h",
    rel_widths = c(1, 1)
  )
}

# CFA Removal
cfa_removal <- function(df) {
  # Use hist_panel to create a histogram for rejected_cardiac_ics
  df_rej_ic <- df %>% distinct(PMID, rejected_cardiac_ics, .keep_all = TRUE)
  plot1 <- hist_panel(df_rej_ic, "rejected_cardiac_ics",
    x.label = "# rejected cardiac ICs",
    discrete = FALSE,
    binwidth = 1
  ) # adjust binwidth as needed

  summarize_cfa_criteria <- function(df) {
    category_mapping <- c(
      "time course" = "Time Course",
      "topography" = "Topography",
      "power spectrum" = "Power Spectrum",
      "phase consistency" = "Phase Consistency",
      "iclabel" = "ICLabel",
      "corrmap" = "CORRMAP",
      "correlation" = "Correlation",
      "sasica" = "SASICA"
    )

    criteria_expanded <- df %>%
      separate_rows(cfa_rej_criteria, sep = ",") %>%
      mutate(
        cfa_rej_criteria = tolower(str_trim(cfa_rej_criteria, side = "both")),
        cfa_rej_criteria = case_when(
          cfa_rej_criteria %in% names(category_mapping) ~ category_mapping[cfa_rej_criteria],
          TRUE ~ cfa_rej_criteria
        )
      ) %>%
      filter(cfa_rej_criteria != "") %>%
      # Exclude "unknown" criteria
      filter(cfa_rej_criteria != "unknown") %>%
      distinct(PMID, cfa_rej_criteria, .keep_all = TRUE)

    # Count occurrences and compute total n
    criteria_counts <- criteria_expanded %>%
      group_by(cfa_rej_criteria) %>%
      summarise(count = n()) %>%
      arrange(desc(count))

    total_n <- sum(criteria_counts$count)

    # Create plot with tilted x-axis labels, bars starting at zero, and n= annotation at top-left
    ggplot(criteria_counts, aes(x = reorder(cfa_rej_criteria, count), y = count)) +
      geom_bar(stat = "identity", fill = "#696969") +
      theme_classic() +  # Add theme_classic first
      labs(
      x = "",  # Removed x-axis title
      y = "Count",
      title = ""
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 10)) +
      annotate("text",
      x = -Inf, y = Inf, label = paste("n =", total_n),
      hjust = -0.1, vjust = 1.1
      )
  }

  # Re-added RR plot function
  rr_intervals_plot <- function(df) {
    df_rr <- df %>%
      select(PMID, other_cfa_removal_strategy) %>%
      distinct(PMID, other_cfa_removal_strategy, .keep_all = TRUE) %>%
      mutate(
        rr_match = str_match(
          tolower(other_cfa_removal_strategy),
          "rr at least\\s*(\\d+)\\s*ms"
        )
      ) %>%
      filter(!is.na(rr_match[, 2])) %>%
      mutate(rr_value = as.numeric(rr_match[, 2])) %>%
      count(rr_value)

    total_n <- sum(df_rr$n)
    min_val <- min(df_rr$rr_value, na.rm = TRUE)
    max_val <- max(df_rr$rr_value, na.rm = TRUE)
    tick_breaks <- unique(c(min_val, pretty(c(min_val, max_val)), max_val))

    ggplot(df_rr, aes(x = rr_value, y = n)) +
      geom_bar(stat = "identity", fill = "#696969") +
      theme_classic() +  # Add theme_classic first
      labs(
        x = "RR Interval (ms)",
        y = "Count",
        title = ""
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(breaks = tick_breaks) +
      coord_cartesian(xlim = c(min_val, max_val), clip = "off") +
      theme(plot.margin = margin(10, 10, 10, 10),
            axis.title.y = element_text(size = 10)) +
      annotate("text",
        x = -Inf, y = Inf, label = paste("n =", total_n),
        hjust = -0.1, vjust = 1.1
      )
  }

  # Updated function to visualize other strategies not part of the RR plot,
  # now grouping select items under a new label describing PCA on average HEPs
  other_strategy_plot <- function(df) {
    df_other <- df %>%
      select(PMID, other_cfa_removal_strategy) %>%
      distinct(PMID, other_cfa_removal_strategy, .keep_all = TRUE) %>%
      filter(
        other_cfa_removal_strategy != "",
        other_cfa_removal_strategy != "unknown",
        !str_detect(tolower(other_cfa_removal_strategy), "^rr at least")
      )


    df_other <- df_other %>%
      mutate(merged_strategy = case_when(
        tolower(other_cfa_removal_strategy) %in% c(
          "correct for cfa using the signal from the tip of the nose",
          "regress ecg out",
            "subtracting the cardiac signal artifact (skin-conducted ekg signal to the scalp) from the eeg signal",
          "scaled ecg subtracted from eeg",
          "subtract average ecg",
          "ecg subtraction"
        ) ~ "subtract / regress ECG from EEG",
        tolower(other_cfa_removal_strategy) == "limit analysis to time of minimal artifact" ~ "time of minimal artifact",
        tolower(other_cfa_removal_strategy) == "subtract subject average resting state heps from task heps" ~ "subtract rsHEP from task HEP",
        str_detect(tolower(other_cfa_removal_strategy), "pca|hep") ~ "PCA on HEP",     
        TRUE ~ other_cfa_removal_strategy
      ))

    df_other_counts <- df_other %>%
      group_by(merged_strategy) %>%
      summarise(count = n()) %>%
      arrange(desc(count))

    ggplot(df_other_counts, aes(x = reorder(merged_strategy, count), y = count)) +
      geom_bar(stat = "identity", fill = "#696969") +
      theme_classic() +  # Add theme_classic first
      labs(
        x = "",
        y = "Count",
        title = ""
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            axis.title.y = element_text(size = 10))+
      annotate("text",
      x = -Inf, y = Inf, label = paste("n =", total_n),
      hjust = -0.1, vjust = 1.1  # Removed size parameter
      )
  }

  # Use the newly created plot1 and the other two plots
  plot2 <- summarize_cfa_criteria(df)
  plot3 <- rr_intervals_plot(df)
  plot4 <- other_strategy_plot(df)
  plot5 <- create_ica_usage_plot(df)  # Added ICA usage plot

  # Layout: 3 rows, 2 columns
  top_row <- plot_grid(plot1, plot2, ncol = 2, labels = c("A", "B"), align = "h")
  middle_row <- plot_grid(plot3, plot4, ncol = 2, labels = c("C", "D"), align = "h")
  bottom_row <- plot_grid(plot5, ncol = 1, labels = "E", align = "h")

  plot_grid(
    top_row,
    middle_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1, 1, 0.8)  # Adjust heights as needed
  )
}


# Here we generate all figures
# Ideally, each panel / figure should be generated by a function that
# accepts the dataframe as the first argument so that the functions could be
# re-used in the Shiny app
make_figures <- function(df, save_path) {
  eeg_aq_prep_plot <- eeg_aq_prep(df)

  ggsave(
    filename = file.path(save_path, "eeg_aq_prep_plot.svg"),
    plot = eeg_aq_prep_plot,
    width = 6.85,
    height = 8,
    units = "in",
    dpi = 300,
    device = "svg"
  )
  show(eeg_aq_prep_plot)

  cfa_removal_plot <- cfa_removal(df)

  ggsave(
    filename = file.path(save_path, "cfa_removal_plot.svg"),
    plot = cfa_removal_plot,
    width = 6.85,
    height = 8,
    units = "in",
    dpi = 300,
    device = "svg"
  )
  show(cfa_removal_plot)

  ggsave(
    filename = file.path(save_path, "time_windows_plot.svg"),
    plot = time_windows_plot,
    width = 6.85,
    height = 4,
    units = "in",
    dpi = 300,
    device = "svg"
  )
  show(time_windows_plot)
}
