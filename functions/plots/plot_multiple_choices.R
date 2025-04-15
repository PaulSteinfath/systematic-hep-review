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
                                  show_wordy_title = FALSE,
                                  x_lab = "Methodological Choice",
                                  x_ticks = TRUE,
                                  pipeline_steps = NULL,
                                  pipeline_colors = NULL,
                                  fixed_order = NULL) {
  
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
  
  # Apply column mapping
  choice_analysis$Method <- apply_column_mapping(choice_analysis$Method, column_mapping_readable)
  
  # Add Step column based on the ORIGINAL variable names (before mapping)
  choice_analysis$Step <- sapply(choice_analysis$Method, function(readable) {
    var_name <- column_mapping_readable[readable]  # because 'readable' is the name in the mapping
    if (is.na(var_name)) var_name <- readable      # fallback if not found
    get_pipeline_step(var_name)
  })
  
  # Set Step as factor with pipeline_colors levels 
  choice_analysis$Step <- factor(choice_analysis$Step, levels = names(pipeline_colors))

  # Set fixed_order if provided
  if (!is.null(fixed_order)) {
    choice_analysis$Method <- factor(choice_analysis$Method, levels = fixed_order)
  }

  if (show_wordy_title){
    my_title <- if (is.null(group_var)) 
      "Papers with Multiple Choices per Method" 
    else 
      "Papers with Multiple Choices per Method by Group"
  } else{
    n_unique_papers <- n_distinct(df$PMID)
    my_title <- paste("n =", n_unique_papers)
  }
  
  p <- ggplot(choice_analysis, aes(x = Method, y = Metric, fill = Step)) +
       geom_bar(stat = "identity") +
       scale_fill_manual(values = pipeline_colors) +
       labs(x = x_lab, y = ifelse(percentages, "Percentage of Papers with Multiple Decisions", "Number of Papers with Multiple Decisions"), title = my_title) +
       plot_theme

  if (vertical) {
    p <- p + coord_flip() + scale_x_discrete(labels = NULL)
  } else {
    p <- p + scale_y_continuous(breaks = NULL)
  }

  p <- p + theme(legend.position = "none")

  return(p)
}