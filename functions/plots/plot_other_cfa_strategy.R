plot_other_cfa_strategy <- function(df) {
  strategy_df <- get_usage_studies(df, names(allowed$other_cfa_removal)) %>%
    arrange(-percentage)
  strategy_df$column <- allowed$other_cfa_removal[strategy_df$column]
  ordered_columns <- strategy_df$column
  strategy_df$column <- factor(strategy_df$column, levels = ordered_columns)
  level <- unique(strategy_df$level)
  total_count <- unique(strategy_df$total)
  
  bar_panel(strategy_df, 
            "percentage", 
            "column", 
            colors = NULL, 
            percentages = T,
            title = "Other CFA removal strategies",
            x_lab = "",
            y_lab = paste("Proportion of", level)) +
    labs(subtitle = paste("n =", total_count, level))
}
