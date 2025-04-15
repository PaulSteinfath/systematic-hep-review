epoch_continuous_ica <- function(df) {
  hist_panel(
    df %>% distinct(PMID, ica_on_epochs),
    col = "ica_on_epochs",
    x.label = "ICA Usage",
    discrete = TRUE,
    custom_labels = c("Continuous", "Epoched")
  )
}
