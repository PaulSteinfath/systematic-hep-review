epoch_continuous_ica <- function(df) {
  hist_panel(
    df %>% 
      distinct(PMID, ICA, ica_on_epochs) %>%
      filter(
        ICA == 1,  
        !is.na(ica_on_epochs),  
        ica_on_epochs != "unknown" 
      ),
    col = "ica_on_epochs",
    title = "ICA Usage",
    discrete = TRUE,
    custom_labels = c("Continuous", "Epoched")
  )
}
