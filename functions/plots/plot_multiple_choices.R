multiple_choice_columns_default <- list(c(
  "reference_offline", 
  "ICA", "ica_on_epochs", "rejected_components", 
  "rejected_cardiac_ics", "cfa_rej_approach", "cfa_rej_criteria", "hep_relative_to","value", "baseline_start_ms", "baseline_end_ms"), 
  
  c("hep_channels_selected", 
   "hep_start", "hep_end", 
  "averaging_channels", "averaging_time", "clustering")
)

plot_multiple_choices <- function(df, variables = multiple_choice_columns_default, vertical = FALSE, group_var = NULL, 
                                  percentages = TRUE, align_by_magnitude = TRUE,
                                  column_mapping_readable = column_mapping_readable_default,
                                  plot_fill = plot_fill_default,
                                  plot_theme = plot_theme_default,
                                  gap = 0.5, group_bar_pos = "dodge",
                                  show_wordy_title = FALSE) {
  
  choice_analysis <- NULL
  
  if (is.null(group_var)) {
    total_papers <- n_distinct(df$PMID)
    if (is.list(variables)) {
      flat_vars <- unlist(variables)
    } else {
      flat_vars <- variables
    }
    for (var in flat_vars) {
      temp <- df %>%
        group_by(PMID) %>%
        summarise(Unique_Choices = n_distinct(!!sym(var)), .groups = "drop") %>%
        summarise(Multiple_Choices = sum(Unique_Choices > 1)) %>%
        mutate(Method = var)
      if (percentages) {
        temp <- temp %>% mutate(Metric = (Multiple_Choices / total_papers) * 100)
      } else {
        temp <- temp %>% mutate(Metric = Multiple_Choices)
      }
      choice_analysis <- bind_rows(choice_analysis, temp)
    }
  } else {
    if (!(group_var %in% names(df))) {
      stop(paste("Grouping variable", group_var, "not found in data."))
    }
    if (is.list(variables)) {
      flat_vars <- unlist(variables)
    } else {
      flat_vars <- variables
    }
    for (var in flat_vars) {
      temp <- df %>%
        group_by(PMID, !!sym(group_var)) %>%
        summarise(Unique_Choices = n_distinct(!!sym(var)), .groups = "drop") %>%
        filter(Unique_Choices > 1) %>%
        group_by(!!sym(group_var)) %>%
        summarise(Multiple_Choices = n(), .groups = "drop") %>%
        mutate(Method = var) %>%
        rename(Group = !!sym(group_var))
      group_totals <- df %>% 
        group_by(!!sym(group_var)) %>% 
        summarise(total = n_distinct(PMID), .groups = "drop")
      temp <- left_join(temp, group_totals, by = c("Group" = group_var))
      if (percentages) {
        temp <- temp %>% mutate(Metric = (Multiple_Choices / total) * 100)
      } else {
        temp <- temp %>% mutate(Metric = Multiple_Choices)
      }
      choice_analysis <- bind_rows(choice_analysis, temp)
    }
    choice_analysis$Group <- factor(choice_analysis$Group)
  }
  
  # Apply column mapping if provided.
  if (!is.null(column_mapping_readable)) {
    choice_analysis$Method <- sapply(choice_analysis$Method, function(x) {
      ind <- which(column_mapping_readable == x)
      if (length(ind) > 0) names(column_mapping_readable)[ind[1]] else x
    })
  }
  
  choice_analysis$Method <- apply_column_mapping(choice_analysis$Method, column_mapping_readable)
  
  if (show_wordy_title){
    my_title <- if (is.null(group_var)) 
      "Papers with Multiple Choices per Method" 
    else 
      "Papers with Multiple Choices per Method by Group"
  } else{
    n_unique_papers <- n_distinct(df$PMID)
    my_title <- paste("n =", n_unique_papers)
  }
  
  p <- column_barplot(results_df = choice_analysis,
                      x_col = "Method",
                      y_col = "Metric",
                      variables = variables,
                      vertical = vertical,
                      group_var = if (is.null(group_var)) NULL else "Group",
                      align_by_magnitude = align_by_magnitude,
                      gap = gap,
                      x_lab = "Methodological Choice",
                      y_lab = ifelse(percentages, "Percentage of Papers with Multiple Decisions", "Number of Papers with Multiple Decisions"),
                      plot_title = my_title,
                      plot_fill = plot_fill,
                      plot_theme = plot_theme,
                      column_mapping_readable = column_mapping_readable,
                      group_bar_pos = group_bar_pos)
  return(p)
}