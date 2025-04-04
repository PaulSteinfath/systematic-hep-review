# Create empty plot with consistent dimensions for alignment
create_empty_plot <- function(message = "", x_limits = NULL) {
  # If x_limits is NULL, provide a default range
  if (is.null(x_limits)) {
    x_limits <- c(-200, 800)  # Default range to show R and T peaks
  }
  
  # Create an empty plot with the same coordinate system
  p <- ggplot() +
    # Add a transparent rectangle to force the proper dimensions
    annotate("rect", xmin = x_limits[1], xmax = x_limits[2], ymin = 0, ymax = 1, alpha = 0) +
    # Add the "No data" message
    annotate("text", x = mean(x_limits), y = 0.5, label = message) +
    # Keep consistent coordinates
    coord_cartesian(xlim = x_limits) +
    # Add the expected dashed lines for R and T peaks at the right locations
    geom_vline(xintercept = 0, color = "#0072B2", alpha = 0.5, linetype = "dashed") +
    geom_vline(xintercept = 300, color = "#E69F00", alpha = 0.5, linetype = "dashed") +
    # Use void theme but keep the plot box
    theme_void() +
    # Make sure the plot has no expansion
    scale_x_continuous(expand = c(0, 0))
  
  return(p)
}
