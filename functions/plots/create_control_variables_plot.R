library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

create_control_variables_plot <- function(df) {
  # Create a list mapping control variables to their categories and synonyms
  control_variable_synonyms <- list(
    # ECG and Heartbeat-Related Controls
    "ECG" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c()),
    "HEP-ECG Correlation" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Correlation HEP-ECG")),
    "Surrogate Heartbeats" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Permuted Heartbeats")),
    "RR Interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("Interbeat Interval", "IBI", "RR")),
    "Number of Heartbeats" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c()),
    "QT Interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c("QT-Interval")),
    "Control interval" = list(category = "ECG and Heartbeat-Related Controls", synonyms = c()),
    
    # Heart Rate Variability Controls
    "HR" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "SDRR" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "SDNN" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "RMSSD" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "pNN50" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "VLF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "LF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "HF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "LF/HF" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c("LF/HF Ratio")),
    "HFlog" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "HRV" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c()),
    "rrHRV" = list(category = "Heart Rate Variability (HRV) Controls", synonyms = c("Vollmer2015")),
    
    # Cardiovascular and Blood Pressure Controls
    "Systolic Volume" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "Systolic Blood Pressure" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "Diastolic Blood Pressure" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c()),
    "VO₂ at VAT" = list(category = "Cardiovascular and Blood Pressure Controls", synonyms = c("VO2 at VAT")),
    
    # Demographic and Psychosocial Controls
    "Age" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Sex" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "Gender" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
    "BMI" = list(category = "Demographic and Psychosocial Controls", synonyms = c("Body Mass Index")),
    "Depression" = list(category = "Demographic and Psychosocial Controls", synonyms = c()),
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
    
    # Task and Experimental Controls
    "Attention" = list(category = "Task and Experimental Controls", synonyms = c()),
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
  
  # Initialize data frame to store counts
  control_counts <- data.frame(
    Control_Variable = names(control_variable_synonyms),
    Count = 0,
    Category = sapply(control_variable_synonyms, function(x) x$category)
  )
  
  # Deduplicate by PMID and controls
  df_unique <- df %>% distinct(PMID, controls)
  
  # Count occurrences for each control variable and its synonyms
  total_pipelines <- nrow(df_unique)  # Get total number of unique pipelines
  
  for (i in 1:nrow(control_counts)) {
    control <- control_counts$Control_Variable[i]
    synonyms <- c(control, control_variable_synonyms[[control]]$synonyms)
    pattern <- paste0("\\b(", paste(synonyms, collapse = "|"), ")\\b")
    
    df_controls <- as.character(df_unique$controls)
    df_controls[is.na(df_controls)] <- ""
    # Calculate percentage instead of absolute count
    control_counts$Count[i] <- 100 * sum(str_detect(tolower(df_controls), tolower(pattern))) / total_pipelines
  }
  
  # Filter and arrange data
  control_counts <- control_counts %>%
    filter(Count > 0) %>%
    mutate(Category = factor(Category, levels = c(
      "ECG and Heartbeat-Related Controls",
      "Heart Rate Variability (HRV) Controls",
      "Cardiovascular and Blood Pressure Controls",
      "Respiration",
      "Demographic and Psychosocial Controls",
      "Physiological and Environmental Controls",
      "Task and Experimental Controls",
      "Other Controls"
    ))) %>%
    arrange(Category, desc(Count)) %>%
    mutate(Control_Variable = factor(Control_Variable, levels = rev(Control_Variable)))
  
  # Define colors
  category_colors <- c(
    "ECG and Heartbeat-Related Controls" = "#4D6B89",
    "Heart Rate Variability (HRV) Controls" = "#6A8A82",
    "Cardiovascular and Blood Pressure Controls" = "#7D9D85",
    "Respiration" = "#A4B494",
    "Demographic and Psychosocial Controls" = "#5D576B",
    "Physiological and Environmental Controls" = "#9B8EA9",
    "Task and Experimental Controls" = "#847E89",
    "Other Controls" = "#BBBBBB"
  )
  
  # Create plot with adjusted legend position
  ggplot(control_counts, aes(x = Count, y = Control_Variable, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = category_colors) +
    scale_x_continuous(labels = function(x) paste0(round(x, 1), "%")) +  # Add % to axis labels
    labs(x = "Percentage of Pipelines", y = "Control Variables", fill = "Category") +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 9),
      legend.position = c(0.65, 0.5),  
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}
