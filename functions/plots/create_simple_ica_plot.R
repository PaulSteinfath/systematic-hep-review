create_simple_ica_plot <- function(df) {
  # Get ICA usage counts
  ica_counts <- df %>%
    distinct(PMID, ICA) %>%
    count(ICA) %>%
    mutate(
      prop = n / sum(n),
      label = ifelse(ICA == 1, "ICA", "No ICA")
    )

  # Create plot
  ggplot(ica_counts, aes(x = label, y = prop)) +
    geom_bar(stat = "identity", fill = '#696969', width = 0.8) +
    scale_y_continuous(labels = scales::percent,
                      expand = expansion(mult = c(0, .1))) +
    labs(x = "",
         y = "Proportion of Studies",
         title = paste("n =", sum(ica_counts$n))) +
    theme_classic(base_family = "sans") +
    theme(
      title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(hjust = 0)
    )
}
