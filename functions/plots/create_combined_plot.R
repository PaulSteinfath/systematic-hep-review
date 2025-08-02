create_combined_plot <- function(
  df,
  start_var,
  end_var,
  x_scale,
  custom_breaks,
  x_label,
  y_label = "Individual Studies",
  font_face = "plain",
  show_legend = TRUE 
) {

  # Remove missing data and validate ranges
  df_filtered <- df %>%
    filter(!is.na(.data[[start_var]]) & !is.na(.data[[end_var]])) 
  
  # Only apply positive value filter if using log scale
  if (x_scale == "log") {
    df_filtered <- df_filtered %>% filter(.data[[start_var]] > 0 & .data[[end_var]] > 0)
  }
        
  # Return empty plot if no valid data
  if (nrow(df_filtered) == 0) {
    return(no_valid_data_stub(message = "No valid data selected"))
  }
  
  df <- df_filtered %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .keep_all = TRUE) %>%
    arrange(.data[[start_var]], .data[[end_var]]) %>%
    mutate(studyid = row_number())
    
  # Create main plot
  p1 <- ggplot(df, 
               aes(x = .data[[start_var]], 
                   xend = .data[[end_var]], 
                   y = studyid, 
                   yend = studyid)) +
    geom_segment(color = "black", linewidth = 0.4)

  # Calculate top 3 most frequent start_var values
  top_3_values <- df %>%
    count(.data[[start_var]]) %>%
    arrange(desc(n)) %>%
    head(3) %>%
    pull(.data[[start_var]])

  # Add vertical lines for top 3 most frequent values
  p1 <- p1 +
    geom_vline(xintercept = top_3_values, 
               color = "#aaaaaa", 
               alpha = 0.3,
               linetype = "11") +
    geom_point(aes(x = .data[[start_var]]), color = "grey", size = 1, shape = 32) +
    geom_point(aes(x = .data[[end_var]]), color = "grey", size = 1, shape = 32) +
    labs(x = x_label, 
         y = y_label,
         title = "Filter cutoffs", 
         subtitle = paste("n =", nrow(df), "studies")) +
    plot_theme_default +
    custom_theme() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )

  # Density calculation
  n_bins <- 160 #n_bins defines resolution of density plot
  frequency_range <- exp(seq(log(min(df[[start_var]], na.rm = TRUE)),
                           log(max(df[[end_var]], na.rm = TRUE)), length.out = n_bins))
  
  frequency_matrix <- matrix(0, nrow = nrow(df), ncol = length(frequency_range))
  for (i in 1:nrow(df)) {
    freq_indices <- which(frequency_range >= df[[start_var]][i] & frequency_range <= df[[end_var]][i])
    frequency_matrix[i, freq_indices] <- 1
  }
  
  density_df <- data.frame(
    Frequency = frequency_range,
    Density = colSums(frequency_matrix)
  )

  # Create density plot
  p2 <- ggplot(density_df, aes(x = Frequency, y = 1, fill = Density)) +
    geom_raster() +
    scale_fill_gradient(
      low = "#EEEEEE", 
      high = "#696969",
      limits = c(0, max(density_df$Density, na.rm = TRUE)),
      breaks = scales::pretty_breaks(n = 4)
    ) +
    labs(x = x_label, y = "", fill = "Number of studies") +
    plot_theme_default +
    custom_theme() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = if(show_legend) "bottom" else "none",
      legend.direction = "horizontal",
      legend.title.position = "top"
    )

  # Adapt x-axis label resolution
  custom_label_function <- function(breaks) {
    if (is.null(breaks) || length(breaks) == 0) return(NULL)
    sapply(breaks, function(x) {
      if (is.na(x) || is.null(x)) return("")
      if (x < 1) sprintf("%.2f", x) else sprintf("%.0f", x)
    })
  }

  # Add scales only if we have valid data (for shiny app data selections)
  if (nrow(df) > 0) {
    p1 <- p1 + scale_x_log10(breaks = custom_breaks, labels = custom_label_function)
    p2 <- p2 + scale_x_log10(breaks = custom_breaks, labels = custom_label_function)
  }

  # Create the base combined plot
  legend <- get_plot_component(p2, "guide-box", return_all = T)[[3]]
  shifted_legend <- plot_grid(
    NULL, legend,
    nrow = 1,
    rel_widths = c(0.1, 1)
  )
  combined_plot <- plot_grid(
    p1, 
    p2 + theme(legend.position = "none"),
    NULL,
    shifted_legend,
    NULL,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(7, 0.7, 0.05, 0.8, 0.2),
    labels = c('E', '', '', '', ''),
    label_x = c(-0.05, 0, 0, 0, 0)
  )

  return(combined_plot)
}
