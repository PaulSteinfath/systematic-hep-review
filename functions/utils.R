process_char_vector <- function(x) {
  # Remove elements that are empty or "unknown" (after trimming and converting to lower case)
  x_clean <- x[!(trimws(tolower(x)) %in% c("", "unknown"))]
  if (length(x_clean) == 0) return(character(0))
  
  # Split each remaining element by comma
  tokens_list <- strsplit(x_clean, ",")
  # Trim spaces from each token and combine into one vector
  tokens <- unlist(lapply(tokens_list, function(vec) trimws(vec)))
  # Remove any tokens that are empty or "unknown"
  tokens <- tokens[!(tokens == "" | tolower(tokens) == "unknown")]
  return(tokens)
}


round_custom <- function(x) {
  # Show one digit after decimal point only for small values (<10)
  above <- x > 10
  x[above] <- round(x[above], digits = 0)
  x[!above] <- round(x[!above], digits = 1)
  
  x
}


save_figure <- function(fig, aspect_ratio, save_path, filename, ext) {
  ggsave(
    filename = file.path(save_path, paste0(filename, ".", ext)),
    plot = fig,
    width = figure_setup$width,
    height = aspect_ratio * figure_setup$width,
    units = figure_setup$units,
    dpi = figure_setup$dpi,
    device = ext,
    bg = "white"
  )
}


safe_merge <- function(df1, df2, by, sort) {
  # Merge and check that the number of rows didn't change in the process - 
  # no data was lost
  n_before <- nrow(df1)
  df_merged <- merge(df1, df2, by = by, sort = sort)
  n_after <- nrow(df_merged)
  
  assert("number of rows should not change due to merge", n_before == n_after)
  df_merged
}


merge_into_other <- function(df, group_col, col, thresh) {
  # Merge all values that appear in less than `thresh` studies
  # into 'Other'
  # 
  # NOTE: better to apply right before plotting to not interfere with
  # entropy calculations
  values_to_merge <- df %>% 
    distinct(!!sym(group_col), !!sym(col)) %>% 
    group_by(!!sym(col)) %>% 
    summarize(count = n()) %>% 
    filter(count < thresh) %>%
    pull(!!sym(col))
  
  all_values <- df %>% pull(!!sym(col))
  case_when(
    all_values %in% values_to_merge ~ "Other",
    .default = all_values
  )
}