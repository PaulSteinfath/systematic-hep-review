create_control_variables_plot <- function(df) {
  # Get control variable mappings from utility function
  control_variable_synonyms <- get_control_variable_mappings()
  category_order <- get_category_order()
  category_colors <- get_category_colors()
  
  # Initialize data frame to store counts
  control_counts <- data.frame(
    Control_Variable = names(control_variable_synonyms),
    Count = 0,
    Category = sapply(control_variable_synonyms, function(x) x$category)
  )
  
  # Deduplicate by PMID and controls
  df_unique <- df %>% distinct(PMID, controls)
  
  # Count occurrences for each control variable and its synonyms
  total_pipelines <- nrow(df_unique)  
  
  df_controls <- as.character(df_unique$controls)
  df_controls[is.na(df_controls)] <- ""

  for (i in 1:nrow(control_counts)) {
    control <- control_counts$Control_Variable[i]
    synonyms <- c(control, control_variable_synonyms[[control]]$synonyms)
    pattern <- paste0("\\b(", paste(synonyms, collapse = "|"), ")\\b")

    # Calculate percentage instead of absolute count
    control_counts$Count[i] <- 100 * sum(str_detect(tolower(df_controls), tolower(pattern))) / total_pipelines
  }
  
  # Filter and arrange data
  control_counts <- control_counts %>%
    mutate(Category = factor(Category, levels = category_order)) %>%
    arrange(Category, desc(Count)) %>%
    mutate(Control_Variable = factor(Control_Variable, levels = rev(Control_Variable)))
  
  # Create plot with adjusted legend position
  ggplot(control_counts, aes(x = Count, y = Control_Variable, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = category_colors) +
    scale_x_continuous(
      labels = function(x) paste0(round(x, 1), "%"),
      expand = c(0, 0)  
    ) +
    labs(x = "Percentage of Pipelines", y = "Control Variables", fill = "Category") +
    plot_theme_default +
    custom_theme() +
    theme(
      axis.text.y = element_text(size = 9),
      legend.position = c(0.65, 0.5),  
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}
