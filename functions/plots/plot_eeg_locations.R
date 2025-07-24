get_channel_freq <- function(df, col, ch, 
                             by = "pipeline", 
                             group_col = "PMID", 
                             divide.over = "n_rows") {
  in_selection <- sapply(df[[col]], 
                         \(x) ch %in% unlist(str_split(x, ", ")))
  
  if (by == "study") {
    sel <- data.frame(group = df[[group_col]], present = in_selection)
    
    # NOTE: aggregating within studies -> the channel is used by the study if
    # any pipeline uses it
    sel <- sel %>%
      group_by(group) %>%
      summarise(present = any(present))
  } else {
    sel <- data.frame(present = in_selection)
  }
    
  n_selected <- sum(sel$present)
  n_total = nrow(sel)
  
  if (is.null(divide.over)) {
    return(n_selected)
  }
  
  if (divide.over == "n_rows") {
    return(n_selected / n_total)
  } else {
    stop(paste("Unsupported option for divide.over:", divide.over))
  }
}


count_occurrences <- function(df, col, channels = ch_names, 
                              by = "pipeline", group_col = 'PMID', 
                              divide.over = "n_rows", add.locs = T) {
  layout <- unique(df$meeg_layout)
  if (length(layout) > 1) {
    layout_desc <- paste(layout, collapse = ", ")
    warning(paste("count_occurrences: expected layout to be same",
                  "across the data frame, got:", layout_desc))
  }
  
  df_distinct <- df %>% distinct(meeg_layout, eeg_locations,
                                 !!sym(group_col), !!sym(col))
  freqs <- sapply(channels[[layout]], get_channel_freq,
                  df = df_distinct, col = col, 
                  by = by, group_col = group_col,
                  divide.over = divide.over)

  num_rows = if (by == "pipeline") nrow(df_distinct) else length(unique(df_distinct[[group_col]]))
  df_freq <- data.frame(meeg_layout = layout,
                        num_rows = num_rows,
                        electrode = channels[[layout]],
                        freq = freqs)
  
  if (add.locs) {
    montage <- if (layout == "biosemi128") "biosemi128" else NULL
    df_freq <- df_freq %>%
      electrode_locations(montage = montage)
  }
  
  df_freq
}


plot_eeg_locations_separate <- function(df, 
                                        lim = NULL, 
                                        colormap = "magma") {
  # Calculate the limit automatically if not provided
  if (is.null(lim)) {
    lim <- max(df$freq)
  }
  
  ggplot(df, aes(x = x,
                 y = y,
                 fill = freq,
                 z = freq)) +
    geom_topo(grid_res = 200,
              chan_size = rel(0.25), 
              head_size = rel(0.5),
              color = 'black',
              linetype = 'solid',
              linewidth = rel(0.1)) +
    facet_wrap(. ~ meeg_layout, nrow = 1) +
    theme_void() + 
    coord_equal()
}


get_weighted_scalpmap <- function(df, grid_res = 200, interp_limit = "skirt") {
  # NOTE: get_scalpmap requires that the data to be interpolated is stored 
  # in the 'fill' column
  df$fill <- df$freq
  
  # Get the interpolated data for the topomap
  map <- get_scalpmap(df, grid_res = grid_res, interp_limit = interp_limit)
  
  # Weigh the data by the fraction of papers that use the corresponding layout
  layout_weight <- unique(df$weight)
  if (length(layout_weight) > 1) {
    message("get_weighted_scalpmap: expected one weight value per layout")
  }
  map$fill <- map$fill * layout_weight
  
  # Rename the fill column to avoid duplicate column names when merging the
  # maps together
  layout <- unique(df$meeg_layout)
  map <- map %>% rename(!!sym(paste0("fill.", layout)) := fill)
  
  map
}


plot_eeg_locations_combined <- function(df, 
                                        display.layout = "standard61",
                                        lim = NULL,
                                        colormap = "magma") {
  # When combining topomaps, we weigh the frequencies to account for different
  # number of rows (papers/pipelines) with different layouts, preparing the weights here
  num_rows_total <- with(df %>% distinct(meeg_layout, num_rows), sum(num_rows))
  df$weight <- df$num_rows / num_rows_total
  
  # Calculate and combine the topomaps
  maps <- by(df, df$meeg_layout, get_weighted_scalpmap)
  maps_merged <- purrr::reduce(maps, merge, by = c('x', 'y'), sort = F) %>%
    mutate(fill.sum = rowSums(across(starts_with("fill"))),
           fill.sum = clip_values(fill.sum))
  
  # Prepare the channels that should be displayed on top of the combined topomap
  montage = if (display.layout == "biosemi128") "biosemi128" else NULL
  df_display <- data.frame(electrode = ch_names[[display.layout]]) %>%
    electrode_locations(montage = montage)
  
  # Calculate the limit automatically if not provided
  if (is.null(lim)) {
    lim <- max(maps_merged$fill.sum)
  }
  
  ggplot(data = df_display,
         aes(x = x, y = y)) + 
    geom_raster(data = maps_merged,
                aes(fill = fill.sum)) +
    geom_head(linewidth = rel(0.5),
              color = "black") +
    geom_channels(size = rel(0.25),
                  color = 'black') +
    geom_contour(data = maps_merged, 
                 aes(z = fill.sum), 
                 color = "black", 
                 linewidth = rel(0.1),
                 bins = 6) +
    theme_void() +
    coord_equal()
}


plot_eeg_locations <- function(df, 
                               col,
                               by = "pipeline",
                               group_col = 'PMID',
                               divide.over = "n_rows",
                               layouts = names(ch_names), 
                               lim = NULL,
                               colormap = "magma",
                               show_colorbar = T,
                               colorbar_title = "Proportion",
                               main_title = NULL,
                               combined = T,
                               display.layout = "standard61") {
  # Get the number of occurrences for each channel in each layout
  df_sel <- df[df$meeg_layout %in% layouts,]
  counts <- by(df_sel, df_sel$meeg_layout, count_occurrences,
               col = col, by = by, group_col = group_col, divide.over = divide.over)
  df_counts <- bind_rows(lapply(counts, as.data.frame))
  
  # Plot the results for each layout separately or combined
  if (combined) {
    p <- plot_eeg_locations_combined(df_counts, display.layout,
                                     lim = lim, colormap = colormap)
  } else {
    p <- plot_eeg_locations_separate(df_counts,
                                     lim = lim, colormap = colormap)
  }
  
  if (!is.null(main_title)) {
    p <- p + 
      labs(title = main_title) + 
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  p <- p + 
    scale_fill_distiller(palette = colormap,
                         labels = if (!is.null(divide.over)) scales::percent else waiver(),
                         direction = 1,
                         guide = if (show_colorbar) 
                           guide_colorbar(title = colorbar_title) 
                         else guide_none(),
                         limits = c(0, lim),
                         breaks = c(0, 0.5 * lim, lim))
  
  p
}