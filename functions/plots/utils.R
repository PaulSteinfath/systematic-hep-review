no_valid_data_stub <- function(message) {
  ggplot() + 
    theme_void() + 
    annotate("text", x = 0.5, y = 0.5, label = message)
}