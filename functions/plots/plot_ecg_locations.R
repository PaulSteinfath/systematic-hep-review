# Body image
body_image_path <- file.path(getwd(), 'assets', 'body_lowres.png')
body_image_info <- image_info(magick::image_read(body_image_path))

# Assign coordinates to different ECG locations
pos <- data.frame(
  x = c(0.335, 0.62, 0.075, 0.9, 0.15, 0.47, 0.65, 0.65, 0.625),
  y = c(0.875, 0.8675, 0.125, 0.175, 0.8, 0.8, 0.05, 0.35, 0.55), 
  label = c('RC', 'LC', 'RW', 'LW', 'RS', 'S', 'LL', 'LAb', 'C'),
  name = c('right clavicle', 'left clavicle', 'right wrist', 'left wrist',
           'right shoulder', 'sternum', 'left leg', 'left abdomen', 'chest')
)

# Define start and end positions for each lead
LEADS <- data.frame(
  start = c('C', 'C', 'C', 'C', 'LAb', 'LAb', 'LC', 'LL', 'LL', 'LW'),
  end = c('LC', 'RC', 'RW', 'S', 'RC', 'RS', 'RC', 'LW', 'RW', 'RW'),
  type = c('III', 'II', 'II', 'II', 'II', 'II', 'I', 'III', 'II', 'I')
)

plot_ecg_locations <- function(
  df, 
  leads_to_consider = c('Lead I', 'Lead II', 'Lead III'),
  border = F,
  split = F
) {
  cols_to_pick <- c('PMID', 'ecg_lead', 'ecg_locations')
  ecg <- df[df$ecg_lead %in% leads_to_consider, cols_to_pick] %>%
    group_by(PMID) %>%
    summarise(ecg_lead = unique(ecg_lead),
              ecg_locations = unique(ecg_locations)) %>%
    filter(ecg_locations != "unknown")
  
  ecg$start <- sapply(ecg$ecg_locations, \(x) str_to_lower(str_split_i(x, ", ", 1)))
  ecg$end <- sapply(ecg$ecg_locations, \(x) str_to_lower(str_split_i(x, ", ", 2)))
  ecg <- ecg %>%
    mutate(start = case_when(start == "v5" ~ "chest",
                             start == "left chest" ~ "chest",
                             start == "left ankle" ~ "left leg",
                             .default = start),
           end = case_when(end == "v5" ~ "chest",
                           end == "left chest" ~ "chest",
                           end == "left ankle" ~ "left leg",
                           .default = end))

  ecg$start_pos <- sapply(ecg$start, \(x) as.vector(pos$label[str_to_lower(pos$name) == x]))
  ecg$end_pos <- sapply(ecg$end, \(x) as.vector(pos$label[str_to_lower(pos$name) == x]))
  
  ecg$start_len <- sapply(ecg$start_pos, \(x) length(x))
  ecg$end_len <- sapply(ecg$end_pos, \(x) length(x))
  
  ecg <- ecg[ecg$start_len > 0 & ecg$end_len > 0,]
  
  leads <- t(rbind(
    unname(unlist(ecg$start_pos)),
    unname(unlist(ecg$end_pos))
  ))
  leads <- data.frame(t(apply(leads, 1, sort)))
  colnames(leads) <- c('start', 'end')
  leads <- leads %>%
    count(start, end)
  leads$setup <- apply(leads, 1, \(x) str_c(x['start'], x['end'], sep = "-"))
  
  
  leads <- merge(leads, LEADS) %>%
    pivot_longer(
      cols = c('start', 'end'),
      names_to = c('pos_type'),
      values_to = c('pos')
    )
  leads$x <- sapply(leads$pos, \(p) pos$x[pos$label == p])
  leads$y <- sapply(leads$pos, \(p) pos$y[pos$label == p])
  
  lead_counts <- leads %>%
    group_by(type) %>%
    summarise(n = sum(n) / 2)
  
  
  
  leads$type <- as.factor(leads$type)
  levels(leads$type) <- sapply(
    c('I', 'II', 'III'), 
    \(x) paste0('Lead ', x, ' (n = ', lead_counts$n[lead_counts$type == x], ')')
  )
  
  asp <- body_image_info$height / body_image_info$width
  
  p <- ggplot(leads, aes(x = x, y = y)) +
    geom_image(data = data.frame(x = 0.5, y = 0.5),
               aes(image = body_image_path),
               size = 0.865) +    # more or less a magic number found 
                                  # through trial and error :(((
    geom_line(aes(group = setup, color = type, linewidth = n)) +
    geom_point(data = pos, size = 8, color="#999999") +
    geom_text(data = pos, aes(label = label), color="white", size = 3) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_linewidth_continuous(range = c(1, 4)) +
    guides(color = guide_legend(title = "ECG Lead"),
           linewidth = guide_legend(title = "Number of studies")) +
    theme_void() +
    theme(aspect.ratio = asp,
          strip.background = element_blank())
  
  if (border) {
    p <- p +
      geom_hline(yintercept = c(0, 1)) +
      geom_vline(xintercept = c(0, 1))
  }
  
  if (split) {
    p <- p + 
      facet_wrap(. ~ type, nrow = 1) +
      guides(color = guide_none(),
             linewidth = guide_legend(title = "Number of studies",
                                      title.position = "top")) +
      theme(strip.text.x = element_text(face = "bold", size = 13, hjust = 0.47))
  }
  
  p
}

# For the calling side:
# ggsave('ecg_locations_combined.png', p, width = 6, height = 5, bg = "white")
# ggsave('ecg_locations_split.png', p, width = 15, height = 5, bg = "white")

