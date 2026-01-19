column_mapping_readable <- c(
  "Number of ECG Electrodes"= "ecg_num_electrodes",
  "ECG Lead" = "ecg_lead",
  "ECG Locations" = "ecg_locations",
  "ECG Ground" = "ecg_ground",
  "ECG Event Method" = "ecg_event_method",
  "ECG High-Pass Filter (Hz)" = "ecg_high_pass",
  "ECG Low-Pass Filter (Hz)" = "ecg_low_pass",
  "HER Window Type" = "hep_window_type",
  "HER Channels Selected" = "hep_eeg_channels_selected",
  "Hypothesis" = "hypothesis",
  "HER Start (ms)" = "hep_start",
  "HER End (ms)" = "hep_end",
  "Number of channels" = "meeg_num_electrodes",
  "Layout" = "meeg_layout",
  "Online Reference" = "reference_online",
  "Offline Reference" = "reference_offline",
  "M/EEG High-Pass Filter (Hz)" = "high_pass",
  "M/EEG Low-Pass Filter (Hz)" = "low_pass",
  "ICA on Epochs" = "ica_on_epochs",
  "ICA" = "ICA",
  "Type of Rejected Components" = "rejected_components",
  "Number of Rejected Cardiac ICs" = "rejected_cardiac_ics",
  "CFA Rejection Approach" = "cfa_rej_approach",
  "CFA Rejection Criteria" = "cfa_rej_criteria",
  "Other CFA Removal Strategy" = "other_cfa_removal_strategy",
  "Other Cleaning Strategy" = "other_cleaning_strategy",
  "Number of Groups" = "groups",
  "Number of Conditions" = "conditions",
  "HER Relative To R/T-Peak" = "hep_relative_to",
  "Baseline Start (ms)" = "baseline_start_ms",
  "Baseline End (ms)" = "baseline_end_ms",
  "HER Value" = "value",
  "Averaging Across Channels" = "averaging_channels",
  "Averaging Across Timepoints" = "averaging_time",
  "Statistical Test" = "statistics",
  "Number of Permutations" = "permutations",
  "Significant Test" = "significant_test",
  "Significant Channels" = "significant_eeg_channels",
  "Significant Relative To" = "significant_relative_to",
  "Significant Start (ms)" = "significant_start_ms",
  "Significant end (ms)" = "significant_end_ms",
  "Controls" = "controls",
  "Sample Size" = "sample_size",
  "Cluster-Based Permutation" = "clustering",
  "Length (min)" = "length_min", 
  "Modality (EEG/MEG)" = "modality",
  "EEG Locations" = "eeg_locations",
  "Resting-state HER" = "setting",
  "Number of Trials" = "trials_original"
)

make_readable <- function(names_vector) {
  sapply(names_vector, function(x) {
    ind <- which(column_mapping_readable == x)
    if (length(ind) > 0) {
      return(names(column_mapping_readable)[ind[1]])
    }
    
    stop("Could not find mapping for ", x)
  })
}