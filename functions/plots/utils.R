no_valid_data_stub <- function(message) {
  ggplot() + 
    theme_void() + 
    annotate("text", x = 0.5, y = 0.5, label = message)
}


clip_values <- function(values, low = 0, high = 1) {
  # Clip values to be in [low, high] range
  pmin(high, pmax(low, values))
}