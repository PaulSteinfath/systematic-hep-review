# Analysis parameters
params <- list(
  entropy_unique_threshold = 10,
  entropy_num_bins = 10,
  hedges_sig_level = 0.05,
  hedges_power_level = 0.8,
  r_peak_offset = 0,
  t_peak_offset = 300,
  peak_stats_permutations = 1000
)

t_peak_offset <- 300
r_peak_offset <- 0

effect_sizes_Coll2021 <- data.frame(
  kind = c("Attention to the heart",
           "Interoceptive performance",
           "Arousal",
           "Clinical vs. control groups"),
  value = c(0.37, 0.35, 0.72, 0.49)
)