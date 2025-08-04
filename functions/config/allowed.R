allowed <- list(
  ica_component_types = c(
    "eye movements" = "Ocular",
    "blinks" = "Ocular",
    "muscle" = "Muscle",
    "cfa" = "CFA",
    "channel noise" = "Channel\nnoise",
    "line noise" = "Line\nnoise",
    "other" = "Other"
  ),
  
  cfa_approach = c(
    "Manual" = "Manual",
    "Automatic" = "Automatic",
    "Semi-automatic" = "Semi-\nautomatic",
    "unknown" = "N/M"
  ),
  
  cfa_criteria = c(
    "time course" = "Time course",
    "topography" = "Topography",
    "power spectrum" = "Power\nspectrum",
    "phase consistency" = "Phase\nconsistency",
    "iclabel" = "Algorithm",
    "corrmap" = "Algorithm",
    "correlation" = "Correlation",
    "sasica" = "Algorithm",
    "unknown" = "N/M"
  ),
  
  other_cfa_removal = c(
    "cfa_use_minimal_rr" = "Minimal RR\ninterval", 
    "cfa_use_minimal_artifact_window" = "Minimal artifact\nwindow", 
    "cfa_csd" = "CSD", 
    "cfa_regress" = "Subtract/regress\nECG from EEG", 
    "cfa_pca" = "PCA on\nHER", 
    "cfa_subtract_rest" = "Subtract rsHER\nfrom task HER"
  ),
  
  statistics = c(
    "t-test" = "t-test",
    "Correlation" = "Correlation",
    "Regression" = "Regression", 
    "ANOVA" = "ANOVA",
    "Non-parametric comparison" = "Non-parametric\ncomparison",
    "Classification" = "Classification",
    "F-test" = "F-test"
  )
)
