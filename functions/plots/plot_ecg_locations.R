# Body image
body_image_path <- file.path(getwd(), 'assets', 'body_lowres.png')
body_image_info <- image_info(magick::image_read(body_image_path))
aspect <- body_image_info$height / body_image_info$width

# Assign coordinates to different ECG locations
pos <- data.frame(
  x = c(0.335, 0.62, 0.075, 0.9, 0.15, 0.65, 0.65, 0.625),
  y = c(0.875, 0.8675, 0.125, 0.175, 0.8, 0.075, 0.25, 0.55), 
  label = c('RC', 'LC', 'RW', 'LW', 'RS', 'LL', 'LAb', 'LCh'),
  name = c('right clavicle', 'left clavicle', 'right wrist', 'left wrist',
           'right shoulder', 'left leg', 'left abdomen', 'left chest')
)


plot_ecg_locations <- function(
  df, 
  leads_to_plot
) {
  # Pick rows with leads of interest and known locations
  cols_to_pick <- c('PMID', 'ecg_lead', 'ecg_locations')
  ecg <- df[df$ecg_lead %in% names(leads_to_plot), cols_to_pick] %>%
    distinct(pick(cols_to_pick)) %>%
    filter(ecg_locations != "unknown")
  
  # Shiny: exit immediately if all rows are removed
  if (nrow(ecg) == 0) {
    return(no_valid_data_stub(message = "No usages of leads I/II/III in the selection"))
  }
  
  # Make sure that two locations per lead are specified
  ecg$num_locations <- sapply(ecg$ecg_locations, \(x) length(unlist(strsplit(x, ", "))))
  if (any(ecg$num_locations > 2)) {
    bad_pmids <- ecg$PMID[ecg$num_locations > 2]
    message(paste("plot_ecg_locations: Too many ECG locations specified for the following PMIDs:", 
                  paste(bad_pmids, collapse = ", ")))
  }
  
  # Preprocess the electrode positions
  # NOTE: only the first two locations are processed from now on
  ecg$start <- sapply(ecg$ecg_locations, \(x) str_to_lower(str_split_i(x, ", ", 1)))
  ecg$end <- sapply(ecg$ecg_locations, \(x) str_to_lower(str_split_i(x, ", ", 2)))
  ecg <- ecg %>%
    mutate(start = case_when(start == "left ankle" ~ "left leg",
                             .default = start),
           end = case_when(end == "left ankle" ~ "left leg",
                           .default = end))
  
  # NOTE: use sorting to match the same locations listed in different order
  ecg$setup <- apply(ecg, 1, \(x) paste(sort(c(x['start'], x['end'])), 
                                        collapse = "; "))

  # Count the occurrences of all leads
  leads <- ecg %>%
    group_by(ecg_lead, setup) %>%
    summarize(count = n())
  leads$start <- sapply(leads$setup, \(x) str_split_i(x, "; ", 1))
  leads$end <- sapply(leads$setup, \(x) str_split_i(x, "; ", 2))
  
  # Pivot longer to have two rows (start and end position) per lead for plotting
  leads <- leads %>% pivot_longer(
    cols = c('start', 'end'),
    names_to = c('pos_type'),
    values_to = c('name')
  )
  n_before_merge <- nrow(leads)
  
  # Add electrode positions
  leads <- merge(leads, pos, by.x = "name", by.y = "name", sort = F)
  n_after_merge <- nrow(leads)
  if (n_before_merge != n_after_merge) {
    diff <- n_before_merge - n_after_merge
    message(paste("Missing", diff, "rows after merging ECG electrode positions"))
  }
  
  # Count the occurrences of all electrode positions to plot only those that are
  # actually present in the data
  occurrences <- as.data.frame(table(leads$label))
  names(occurrences) <- c("label", "freq")
  # NOTE: unused locations will be dropped here and not appear in pos_actual
  pos_actual <- merge(pos, occurrences)
  
  # Show the total number of papers that use each lead
  lead_counts <- leads %>%
    group_by(ecg_lead) %>%
    summarize(count = sum(count) / 2)   # rows are duplicated
  leads$type <- as.factor(leads$ecg_lead)
  levels(leads$type) <- sapply(
    levels(leads$type), 
    \(x) paste0(x, ' (n = ', lead_counts$count[lead_counts$ecg_lead == x], ')')
  )
  
  # Plot the leads and positions on top of the body image
  p <- ggplot(leads, aes(x = x, y = y)) +
    geom_image(data = data.frame(x = 0.5, y = 0.5),
               aes(image = body_image_path),
               size = 0.865) +    # more or less a magic number found 
                                  # through trial and error :(
    geom_line(aes(group = setup, color = ecg_lead, linewidth = count)) +
    geom_segment(data = pos_actual[pos_actual$label == "LL",], 
                 mapping = aes(x = x, y = y, xend = x, yend = 0),
                 linewidth = 1, color = plot_fill_default_single,
                 arrow = arrow(length = unit(0.25, "cm"))) + 
    geom_point(data = pos_actual, size = 8, color = plot_fill_default_single) +
    geom_text(data = pos_actual, aes(label = label), color="white", size = 3) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = leads_to_plot, labels = levels(leads$type)) +
    scale_linewidth_continuous(range = c(1, 4)) +
    guides(color = guide_legend(title = "ECG lead"),
           linewidth = guide_legend(title = "Number of studies")) +
    theme_void() +
    theme(aspect.ratio = aspect,
          strip.background = element_blank())
  
  p
}
