add_control_presence <- function(df,
                                 level = "category") {
  # Split controls by comma
  controls <- ifelse(is.na(df$controls), "", df$controls)
  controls_split <- str_split(controls, ', ')
  
  # TRUE/FALSE presence matrices for variables and categories
  n_rows <- nrow(df)
  n_categories <- length(control_categories)
  n_variables <- length(control_variables)
  item_presence <- matrix(
    FALSE,
    nrow = n_rows,
    ncol = n_variables,
    dimnames = list(NULL, control_variables)
  )
  category_presence <- matrix(
    FALSE,
    nrow = n_rows,
    ncol = n_categories,
    dimnames = list(NULL, control_categories)
  )
  for (cat in control_categories) {
    category_present = rep(F, n_rows)
    for (item in category_variables[[cat]]) {
      item_present <- sapply(controls_split, \(x) is.element(tolower(item), tolower(x)))
      item_presence[, item] = item_present
      
      # Category is present if at least one item is present
      category_present <- category_present | item_present
    }
    
    category_presence[, cat] <- category_present
  }
  
  if (level == "category") {
    presence <- category_presence
  } else if (level == "variable") {
    presence <- item_presence
  } else {
    stop("control are evaluated either by variable or by category")
  }
  
  bind_cols(df[, c("PMID", "controls")], data.frame(presence))
}


control_counts <- function(df,
                           group_col = "PMID",
                           by = "study",
                           distinct_pipelines = F,
                           level = "category") {
  presence_df <- add_control_presence(df, level = level)
  columns <- if (level == "category") control_categories else control_variables
  
  if (by == "pipeline") {
    # Consider only distinct pipelines w.r.t. controls
    if (distinct_pipelines) {
      presence_df <- distinct(presence_df, !!sym(group_col), controls, .keep_all = T)
    }
    multiple_rows_per_group <- any(table(presence_df[[group_col]]) > 1)
    analysis.level <- if (multiple_rows_per_group) "pipelines" else "studies"
  } else if (by == "study") {
    presence_df <- presence_df %>%
      group_by(Key = factor(PMID, levels = unique(PMID))) %>%
      summarize(controls = paste(controls[nzchar(controls)], collapse = ", "),
                across(make.names(columns), \(x) any(x)))
    analysis.level <- "studies"
  } else {
    stop("either by study or by pipeline")
  }
  
  # Initialize data frame to store counts
  if (level == "category") {
    counts_df <- data.frame(
      category = control_categories,
      count = 0
    )
  } else if (level == "variable") {
    counts_df <- data.frame(
      variable = names(control_variable_synonyms),
      category = sapply(control_variable_synonyms, \(x) x$category, USE.NAMES = F),
      count = 0
    )
    rownames(counts_df) <- c()
  }
  
  # Collect the statistics
  target_column <- if (level == "category") "category" else "variable"
  for (i in 1:nrow(counts_df)) {
    # Collect all synonyms in case of variables
    var_name <- counts_df[[i, target_column]]
    all_synonyms <- list(var_name)
    if (level == "variable") {
      all_synonyms <- c(all_synonyms, 
                        control_variable_synonyms[[var_name]]$synonyms)
    }
    
    # Combine presence from all synonyms
    is_present <- rep(F, nrow(presence_df))
    for (synonym in all_synonyms) {
      # We need to adjust the name according to the way R formats them when 
      # creating columns, make.names takes care of that
      col_name <- make.names(synonym)
      is_present <- is_present | presence_df[[col_name]]
    }
      
    # Calculate percentage instead of absolute count
    counts_df$count[i] <- sum(is_present)
  }
  
  # Finalize the data frame
  counts_df$total <- nrow(presence_df)
  counts_df$percentage <- counts_df$count / counts_df$total
  counts_df$level <- analysis.level
  
  counts_df
}