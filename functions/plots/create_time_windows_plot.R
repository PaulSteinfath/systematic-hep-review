create_time_windows_plot <- function(df, start_var, end_var, reference_var, x_label, 
                                     t_peak_offset = 300, x_limits = NULL, add_vlines = TRUE,
                                     fill_color = "#696969", legend_label = "Count",
                                     plot_class = NULL) {  
                                      
  # Process R-peak data
  df_rpeak <- df %>%
    filter(.data[[reference_var]] == "R-peak") %>%
    filter(!is.na(.data[[start_var]]) & !is.na(.data[[end_var]])) %>%
    filter(!.data[[start_var]] %in% c("none", "unknown") & 
           !.data[[end_var]] %in% c("none", "unknown")) %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .keep_all = TRUE) %>%
    mutate(
      start_time = suppressWarnings(as.numeric(as.character(.data[[start_var]]))),
      end_time = suppressWarnings(as.numeric(as.character(.data[[end_var]])))
    ) %>%
    filter(!is.na(start_time) & !is.na(end_time) & 
           is.finite(start_time) & is.finite(end_time))
  
  # Process T-peak data with offset
  df_tpeak <- df %>%
    filter(.data[[reference_var]] == "T-peak") %>%
    filter(!is.na(.data[[start_var]]) & !is.na(.data[[end_var]])) %>%
    filter(!.data[[start_var]] %in% c("none", "unknown") & 
           !.data[[end_var]] %in% c("none", "unknown")) %>%
    distinct(PMID, .data[[start_var]], .data[[end_var]], .keep_all = TRUE) %>%
    mutate(
      start_time = suppressWarnings(as.numeric(as.character(.data[[start_var]]))) + t_peak_offset,
      end_time = suppressWarnings(as.numeric(as.character(.data[[end_var]]))) + t_peak_offset
    ) %>%
    filter(!is.na(start_time) & !is.na(end_time) & 
           is.finite(start_time) & is.finite(end_time))
  
  # Modified inner function: set fill color and legend name based on title
  create_density_plot <- function(df_filtered, title = "", x_limits = NULL, show_ecg = TRUE, remove_axes = FALSE) {
    if (nrow(df_filtered) == 0) {
      return(ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "No valid data"))
    }
    if (!is.null(x_limits)) {
      t <- seq(x_limits[1], x_limits[2], length.out = 1000)
    } else {
      t <- seq(-200, 800, length.out = 1000)
    }
    if(show_ecg){
      ecg <- create_ecg_wave(t)
      ecg_df <- data.frame(Time = t, ECG = ecg)
    }
    
    n_bins <- 160
    time_range <- seq(min(df_filtered$start_time, na.rm = TRUE),
                      max(df_filtered$end_time, na.rm = TRUE),
                      length.out = n_bins)
    density_matrix <- matrix(0, nrow = nrow(df_filtered), ncol = length(time_range))
    for (i in 1:nrow(df_filtered)) {
      time_indices <- which(time_range >= df_filtered$start_time[i] & time_range <= df_filtered$end_time[i])
      density_matrix[i, time_indices] <- 1
    }
    density_df <- data.frame(
      Time = time_range,
      Density = colSums(density_matrix)
    )
    
    # Set fill color and legend text based on title
    my_fill <- fill_color
    my_legend <- legend_label
    if(title == "R-peak referenced"){
      my_fill <- "#0072B2"  # R wave color
      my_legend <- "R Peak"
    } else if(title == "T-peak referenced"){
      my_fill <- "#E69F00"  # T wave color
      my_legend <- "T Peak"
    }
    
    p <- ggplot() +
      geom_raster(data = density_df, aes(x = Time, y = 1, fill = Density)) +
      scale_fill_gradient(
        low = "white", 
        high = my_fill, 
        name = my_legend,
        guide = guide_colorbar(title.position = "top")
      ) +
      labs(x = x_label, y = "") +  # Title removed so legend shows only via fill scale
      {if(add_vlines) geom_vline(xintercept = 0, color = "#0072B2", alpha = 0.3, linetype = "dashed")} +
      {if(add_vlines && title == "T-peak referenced") geom_vline(xintercept = t_peak_offset, color = "#E69F00", alpha = 0.3, linetype = "dashed")} +
      {if(!is.null(x_limits)) coord_cartesian(xlim = x_limits)} +
      scale_x_continuous(expand = c(0,0))
    
    if(remove_axes){
      p <- p + theme_void() + theme(legend.position = "none")
    } else {
      p <- p + theme_classic() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.8, "lines")
        )
    }
    return(p)
  }
  
  # Create density plots (ECG panel omitted)
  p1 <- create_density_plot(df_rpeak, "R-peak referenced", x_limits, show_ecg = FALSE, remove_axes = TRUE)
  p2 <- create_density_plot(df_tpeak, "T-peak referenced", x_limits, show_ecg = FALSE, remove_axes = TRUE)
  
  # If plot_class is provided then override titles (already handled elsewhere if needed)
  if(!is.null(plot_class)){
    p1 <- create_density_plot(df_rpeak, "", x_limits, show_ecg = FALSE, remove_axes = TRUE)
    p2 <- create_density_plot(df_tpeak, "", x_limits, show_ecg = FALSE, remove_axes = TRUE)
  }
  
  combined_plot <- plot_grid(p1, p2, ncol = 1, align = "v")
  return(combined_plot)
}
