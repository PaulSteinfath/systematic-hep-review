# Group processing steps in categories
pipeline_steps <- list(
  "Experiment" = c(
    "groups", 
    "conditions", 
    "length_min", 
    "setting", 
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
    "ecg_ground",
    "ecg_low_pass",
    "ecg_high_pass",
    "ecg_event_method" 
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
    "hep_eeg_channels_selected",
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


# Reverse mapping: get pipeline step for a column
get_pipeline_step <- function(var) {
  # Check each pipeline step category
  for (step in names(pipeline_steps)) {
    if (var %in% pipeline_steps[[step]]) {
      return(step)
    }
  }
  
  stop("Could not find pipeline step for ", var)
}

