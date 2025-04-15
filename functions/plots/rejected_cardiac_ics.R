
rejected_cardiac_ics <- function(df) {

  df_rej_ic <- df %>%
    distinct(PMID, rejected_cardiac_ics, .keep_all = TRUE) %>%
    filter(rejected_cardiac_ics != "")
    

  hist_panel(df_rej_ic, "rejected_cardiac_ics",
    x.label = "Average Number of rejected CFA-related ICs",
    discrete = FALSE,
    binwidth = 1
  ) 
}