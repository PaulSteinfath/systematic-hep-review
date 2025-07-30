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