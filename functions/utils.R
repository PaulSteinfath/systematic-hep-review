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