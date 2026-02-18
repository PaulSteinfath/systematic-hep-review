common_colors <- list(
  ecg_controls = "#647499ff",
  fill_default = "#696969",
  grid_lines = "grey90",
  r_peak_vline = "gray40",
  simulated_ecg = "#696969",
  simulated_ecg_alpha = 0.3
)

figure_setup <- list(
  width = 190,
  units = "mm",
  dpi = 300
)

font_setup <- list(
  title = 10,
  subtitle = 8,
  axis.title = 8,
  axis.text = 7,
  legend.title = 9,
  legend.text = 8
)

plot_theme_default <- theme(panel.grid = element_blank(), 
                            plot.title = element_text(size = font_setup$title), 
                            plot.subtitle = element_text(size = font_setup$subtitle),
                            axis.title.x = element_text(size = font_setup$axis.title),
                            axis.text.x = element_text(size = font_setup$axis.text),
                            axis.title.y = element_text(size = font_setup$axis.title),
                            axis.text.y = element_text(size = font_setup$axis.text),
                            legend.title = element_text(size = font_setup$legend.title),
                            legend.text = element_text(size = font_setup$legend.text))

# Color palettes
pipeline_colors <- c(
  "Experiment"     = "#2E2E2E",
  "Acquisition"    = "#5A5A5A",
  "Preprocessing"  = "#808080",
  "HER Estimation" = "#A8A8A8",
  "Statistics"     = "#D0D0D0" 
)

leads_palette <- c(
  'Lead I' = '#fc8d62', 
  'Lead II' = '#66c2a5', 
  'Lead III' = '#8da0cb'
)

r_t_peak_palette <- c(
  "R-peak" = "#696969", 
  "T-peak" = "#E69F00"
)

palette_Coll2021 <- c("Attention to the heart" = "#1b9e77",
                      "Interoceptive performance" = "#d95f02",
                      "Arousal" = "#7570b3",
                      "Clinical vs. control groups" = "#e7298a")

control_category_colors <- c(
  "ECG and Heartbeat-Related Controls" = "#4D6B89",
  "Heart Rate Variability (HRV) Controls" = "#6A8A82",
  "Cardiovascular and Blood Pressure Controls" = "#7D9D85",
  "Respiration" = "#A4B494",
  "Demographic and Psychosocial Controls" = "#5D576B",
  "Physiological and Environmental Controls" = "#9B8EA9",
  "Task and Experimental Controls" = "#847E89",
  "Other Controls" = "#BBBBBB"
)