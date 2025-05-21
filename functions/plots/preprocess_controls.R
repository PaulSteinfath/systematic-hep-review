get_control_variable_mappings <- function() {

  # Create a list mapping control variables to their categories and synonyms
  control_variable_synonyms <- list(
    # ECG and Heartbeat-Related Controls
    "ECG" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c()),
    "HEP-ECG Correlation" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Correlation HEP-ECG")),
    "Surrogate Heartbeats" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Permuted Heartbeats")),
    "RR Interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Interbeat Interval", "IBI", "RR", "HR")),
    "Number of Heartbeats" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c()),
    "QT Interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("QT-Interval")),
    "Control interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("control interval 180-400 ms")), 
    "T-Wave Latency" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("t-wave mean latency", "t-wave mean latencey variability")),

    # Heart Rate Variability Controls
    "SDRR" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "SDNN" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "RMSSD" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "pNN50" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "VLF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "LF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "HF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "LF/HF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c("LF/HF Ratio")),
    "HFlog" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "HRV" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c("IBI Variability")),
    "rrHRV" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c("Vollmer2015", "hrv (vollmer2015)")),
  
    # Cardiovascular and Blood Pressure Controls
    "Systolic Volume" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()), 
    "Stroke Volume" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "Systolic Blood Pressure" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "Diastolic Blood Pressure" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "VO₂ at VAT" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c("VO2 at VAT")),

    # Demographic and Psychosocial Controls
    "Age" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Sex" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Gender" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "BMI" = list(category = "Demographic and Psychosocial Controls", synonyms = c("Body Mass Index")),
    "Depression" = list(category = "Demographic and Psychosocial Controls", synonyms = c("bdi")),
    "Anxiety" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Affective Disorders" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Education" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Handedness" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Hypnotizability" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),

    # Respiration
    "Respiration" = list(category = "Respiration", synonyms = c("Respiration Phase", "Respiration Amplitude", "Phase bifurcation index")),
    "I/E" = list(category = "Respiration", synonyms = c("Inhalation/Exhalation ratio")),

    # Physiological and Environmental Controls
    "Medication" = list(category = "Physiological and Environmental Controls", synonyms = c()),
    "Hormones" = list(category = "Physiological and Environmental Controls", synonyms = c()),
    "Menstrual Cycle Phase" = list(category = "Physiological and Environmental Controls", synonyms = c()),
    "Pupil diameter" = list(category = "Physiological and Environmental Controls", synonyms = c()),
    "Cortisol" = list(category = "Physiological and Environmental Controls", synonyms = c("CAR")),

    # Task and Experimental Controls
    "Attention" = list(category = "Task and Experimental Controls", synonyms = c("attention to the task")),
    "Alpha Power" = list(category = "Task and Experimental Controls", synonyms = c()),
    "Total Intracranial Volume" = list(category = "Task and Experimental Controls", synonyms = c()),
    "Arousal" = list(category = "Task and Experimental Controls", synonyms = c()),
    "Interoception" = list(category = "Task and Experimental Controls", synonyms = c()),
    "Daydreaming" = list(category = "Task and Experimental Controls", synonyms = c()),

    # Other Controls
    "Scanner Type" = list(category = "Other Controls", synonyms = c()),
    "Illness Duration" = list(category = "Other Controls", synonyms = c()),
    "Hospitalization" = list(category = "Other Controls", synonyms = c())
  )
  
  return(control_variable_synonyms)
}

get_category_order <- function() {
  c(
    "ECG and Heartbeat-Related Controls",
    "Heart Rate Variability (HRV) Controls",
    "Cardiovascular and Blood Pressure Controls",
    "Respiration",
    "Demographic and Psychosocial Controls",
    "Physiological and Environmental Controls",
    "Task and Experimental Controls",
    "Other Controls"
    )
}

get_category_colors <- function() {
  c(
    "ECG and Heartbeat-Related Controls" = "#4D6B89",
    "Heart Rate Variability (HRV) Controls" = "#6A8A82",
    "Cardiovascular and Blood Pressure Controls" = "#7D9D85",
    "Respiration" = "#A4B494",
    "Demographic and Psychosocial Controls" = "#5D576B",
    "Physiological and Environmental Controls" = "#9B8EA9",
    "Task and Experimental Controls" = "#847E89",
    "Other Controls" = "#BBBBBB"
    )
}
