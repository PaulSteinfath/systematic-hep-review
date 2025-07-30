# Group processing steps in categories
pipeline_steps <- list(
  "Experiment" = c(
    "groups", 
    "conditions", 
    "length_min", 
    "rsHEP", 
    "sample_size", 
    "trials_original"
  ),
  "Acquisition" = c(
    "modality",
    "reference_online",
    "meeg_num_electrodes", 
    "ecg_num_electrodes", 
    "ecg_lead", 
    "ecg_locations", 
    "ecg_ground"
  ),
  "Preprocessing" = c(
    "reference_offline", 
    "high_pass", 
    "low_pass", 
    "ICA", 
    "ica_on_epochs", 
    "rejected_components", 
    "rejected_cardiac_ics",
    "cfa_rej_approach", 
    "cfa_rej_criteria", 
    "other_cleaning_strategy", 
    "other_cfa_removal_strategy"
  ),
  "HER Estimation" = c(
    "hep_relative_to", 
    "hep_start", 
    "hep_end",
    "baseline_start_ms", 
    "baseline_end_ms", 
    "value", 
    "hep_channels_selected",
    "averaging_channels", 
    "averaging_time", 
    "hep_window_type"
  ),
  "Statistics" = c(
    "clustering", 
    "statistics", 
    "permutations", 
    "hypothesis"
  )
)
