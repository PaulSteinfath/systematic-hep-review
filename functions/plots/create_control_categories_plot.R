create_control_categories_plot <- function(df) {
  # Get control variable mappings and other settings from utility functions
  control_variable_synonyms <- get_control_variable_mappings()
  category_order <- get_category_order()
  category_colors <- get_category_colors()
  
  # remove duplicate rows
  df_unique <- df %>% distinct(PMID, controls)
  total_pipelines <- nrow(df_unique)
  
  # Initialize category presence matrix once
  categories <-  uniqu(sapply(control_variable_synonyms, function(x) x$category))
  category_presence <- matrix(FALSE, 
                            nrow = nrow(df_unique), 
                            ncol = length(categories),
                            dimnames = list(NULL, categories))
  
  df_controls <- as.character(df_unique$controls)
  df_controls[is.na(df_controls)] <- ""
  
  # Group all patterns by category
  category_patterns <- list()
  for (control_name in names(control_variable_synonyms)) {
    control <- control_variable_synonyms[[control_name]]
    category <- control$category
    if (is.null(category_patterns[[category]])) {
      category_patterns[[category]] <- c()
    }
    category_patterns[[category]] <- c(
      category_patterns[[category]], 
      control_name, 
      control$synonyms
    )
  }
  
  # Check each category's patterns
  for (category in names(category_patterns)) {
    patterns <- unique(category_patterns[[category]])
    pattern <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")
    category_presence[, category] <- str_detect(tolower(df_controls), tolower(pattern))
  }
  
  # Calculate percentage for each category
  category_counts <- data.frame(
    Category = categories,
    Count = colSums(category_presence) * 100 / total_pipelines
  )
  
  # Arrange data
  category_counts <- category_counts %>%
    mutate(Category = factor(Category, levels = category_order)) %>%
    arrange(desc(Count))
  
  # Create plot
  ggplot(category_counts, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = category_colors) +
    scale_y_continuous(
      labels = function(x) paste0(round(x, 1), "%"),
      expand = c(0, 0),
      limits = c(0, max(category_counts$Count) * 1.05)
    ) +
    labs(x = "", y = "Percentage of Pipelines") +
    coord_flip() +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}
