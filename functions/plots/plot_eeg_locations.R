library(eegUtils)
library(ggplot2)
library(purrr)
library(tidyr)
library(viridis)


get_channel_freq <- function(df, col, ch, divide.over = "n_rows") {
  in_layout <- sapply(df$meeg_locations, 
                      \(x) grepl(ch, x, fixed = T))
  in_selection <- sapply(df[[col]], 
                         \(x) grepl(ch, x, fixed = T))
  
  n_selected <- sum(in_selection)
  n_used <- sum(in_layout)
  n_rows <- length(in_layout)
  
  if (divide.over == "n_rows") {
    return(n_selected / n_rows)
  }
  
  if (n_used == 0) {
    return(0)
  }
  
  n_selected / n_used
}


count_occurrences <- function(df, col, group_col = 'PMID', 
                              divide.over = "n_rows", add.locs = T) {
  layout <- unique(df$meeg_layout)
  if (length(layout) > 1) {
    layout_desc <- paste(layout, collapse = ", ")
    warning(paste("count_occurrences: expected layout to be same",
                  "across the data frame, got:", layout_desc))
  }
  
  df_distinct <- df %>% distinct(meeg_layout, meeg_locations,
                                 !!sym(group_col), !!sym(col))
  freqs <- sapply(ch_names[[layout]], get_channel_freq,
                  df = df_distinct, col = col, divide.over = divide.over)
  
  df_freq <- data.frame(meeg_layout = layout,
                        num_rows = nrow(df_distinct),
                        electrode = ch_names[[layout]],
                        freq = freqs)
  
  if (add.locs) {
    montage <- if (layout == "biosemi128") "biosemi128" else NULL
    df_freq <- df_freq %>%
      electrode_locations(montage = montage)
  }
  
  df_freq
}


plot_eeg_locations_separate <- function(df, 
                                        lim = 1.0, 
                                        colormap = "magma") {
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
    scale_fill_viridis(option = colormap,
                       direction = 1,
                       limits = c(0, lim)) + 
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
  
  # Weight the data by the fraction of papers that use the corresponding layout
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
                                        lim = 1.0,
                                        colormap = "magma") {
  # When combining topomaps, we weigh the frequencies to account for different
  # number of rows (papers/pipelines) with different layouts, preparing the weights here
  num_rows_total <- with(df %>% distinct(meeg_layout, num_rows), sum(num_rows))
  df$weight <- df$num_rows / num_rows_total
  
  # Calculate and combine the topomaps
  maps <- by(df, df$meeg_layout, get_weighted_scalpmap)
  maps_merged <- purrr::reduce(maps, merge, by = c('x', 'y'), sort = F) %>%
    mutate(fill.sum = rowSums(across(starts_with("fill"))))
  
  # Prepare the channels that should be displayed on top of the combined topomap
  montage = if (display.layout == "biosemi128") "biosemi128" else NULL
  df_display <- data.frame(electrode = ch_names[[display.layout]]) %>%
    electrode_locations(montage = montage)
  
  ggplot(data = df_display,
         aes(x = x, y = y)) + 
    geom_raster(data = maps_merged,
                aes(fill = fill.sum)) +
    scale_fill_viridis(option = colormap,
                       direction = 1,
                       limits = c(0, lim)) +
    geom_head(linewidth = rel(0.5),
              color = "black") +
    geom_channels(size = rel(0.25),
                  color = 'black') +
    # Interpolating the already interpolated map takes a lot of time, need to
    # look for another solution: pick values that correspond to displayed electrodes?
    # stat_scalpcontours(data = maps_merged, 
    #                    aes(z = fill.sum, fill = fill.sum)) +
    theme_void() +
    coord_equal()
}


plot_eeg_locations <- function(df, 
                               col,
                               group_col = 'PMID',
                               divide.over = "n_rows",
                               layouts = names(ch_names), 
                               colormap = "magma",
                               combined = T,
                               display.layout = "standard61") {
  # Get the number of occurrences for each channel in each layout
  df_sel <- df[df$meeg_layout %in% layouts,]
  counts <- by(df_sel, df_sel$meeg_layout, count_occurrences,
               col = col, group_col = group_col, divide.over = divide.over)
  df_counts <- bind_rows(lapply(counts, as.data.frame))
  
  # Plot the results for each layout separately or combined
  if (combined) {
    p <- plot_eeg_locations_combined(df_counts, display.layout,
                                     colormap = colormap)
  } else {
    p <- plot_eeg_locations_separate(df_counts,
                                     colormap = colormap)
  }
  
  p
}