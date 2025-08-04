allowed <- list(
  other_cfa_removal = c(
    "cfa_use_minimal_rr" = "Minimal RR\ninterval", 
    "cfa_use_minimal_artifact_window" = "Minimal artifact\nwindow", 
    "cfa_csd" = "CSD", 
    "cfa_regress" = "Subtract/regress\nECG from EEG", 
    "cfa_pca" = "PCA on\nHEP", 
    "cfa_subtract_rest" = "Subtract rsHEP\nfrom task HEP"
  ),
  
  ica_component_types = c(
    "eye movements" = "Ocular",
    "blinks" = "Ocular",
    "muscle" = "Muscle",
    "cfa" = "CFA",
    "channel noise" = "Channel\nnoise",
    "line noise" = "Line\nnoise",
    "other" = "Other"
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
