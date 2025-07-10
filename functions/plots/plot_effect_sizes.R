
# first we prepare a figure with a placeholder for stats
# hopefully we only need code for this and other can be visualised with histograms

zero_pmids <- unique(df$PMID[df$significant_test == 0])
one_pmids  <- unique(df$PMID[df$significant_test == 1])
zero_pmids <- zero_pmids[!is.na(zero_pmids)]
one_pmids  <- one_pmids[!is.na(one_pmids)]
diff_pmids <- setdiff(zero_pmids, one_pmids)

df$conditions[df_included$conditions==19] = 1 # correct in table -> here we need to just check if any statistic was done because there it's descriptive

a<-hist_panel(df = df, col = "sample_size", x.label = "Sample Size")
b<-hist_panel(df = df, col = "groups", x.label = "Number of Groups", binwidth = 1)
c<-hist_panel(df = df, col = "conditions", x.label = "Number of Conditions", binwidth = 1)

# num trials 
new_cols <- compute_sd_cols(df,trials)
df <- bind_cols(df, new_cols)
d<-hist_panel(df = df, col = "trials_Mean", force.numeric = T, x.label = "Estimated Number of Averaged Epochs", use_proportion = T)

#e<-hist_panel(df = df, col = "hep_window_type", x.label = "Analysis Window", discrete = T) # already in another figure
#f<-hist_panel(df = df, col = "clustering", x.label = "Approach", binwidth = 1, discrete = T, custom_labels = c('Averaging','Clustering')) # already iin another figure 

# need clean up for sure:
clean_statistics_column <- function(statistics_column) {
  case_when(
    str_detect(statistics_column, regex("anova", ignore_case = TRUE)) ~ "ANOVA",
    str_detect(statistics_column, regex("unknown", ignore_case = TRUE)) ~ "Unknown",
    str_detect(statistics_column, regex("parametric", ignore_case = TRUE)) ~ "Nonparametric",
    str_detect(statistics_column, regex("none", ignore_case = TRUE)) ~ "None",
    TRUE ~ statistics_column
  )
}
df <- df %>%
  mutate(statistics = clean_statistics_column(statistics))
g<-hist_panel(df = df, col = "statistics", x.label = "Statistics family", discrete = T, tilt_labels = T)+coord_flip()
# do we add unknown?none? 

# hedges g
new_cols <- compute_effect_columns(df, 
                                   test_type_col = statistics, 
                                   sample_size_col = sample_size, 
                                   trials_mean_col = trials_Mean, 
                                   groups_col = groups, 
                                   conditions_col = conditions)

df <- bind_cols(df, new_cols)
h<-hist_panel(df = df, col = "hedges_g", force.numeric = T, x.label = "Hedges' g", use_proportion = T)+ geom_vline(xintercept = 0.6, linetype = "dashed", color = "#E69F00")

# also we wanted to add controls
# but need to force it to also be grey
i<-create_control_categories_plot(df)
i<- plot_control_category_histogram(df)
i<-control_categories_hist_panel(df)+coord_flip()

first_row <- plot_grid(
  a,d,h,
  ncol = 3, labels = c("A","B", "C"), rel_widths = c(1.5, 1.5, 2),
  align = "h"
)
second_row <- plot_grid(
  b,c,e,f,
  ncol = 4, labels = c("D","E","F","G"),
  rel_widths = c(1.5, 1.5, 1, 1),
  align = "h"
)
third_row <-plot_grid(
  i,g,
  ncol = 2, labels = c("H","I"),rel_widths = c(1.5, 1),
  align = "h"
)

p<- plot_grid(first_row, second_row, third_row, ncol = 1, rel_heights = c(1,1,1.8))

ggsave(
  filename = file.path(save_path, paste0("stats.", 'png')),
  plot = p,
  width = 10,
  height = 11,
  units = "in",
  dpi = 300,
  device = 'png',
  bg = "white"
)

ggsave(
  filename = file.path(save_path, paste0("simulations.", 'png')),
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300,
  device = 'png',
  bg = "white"
)

save_path<-getwd()


bottom_row <- plot_grid(
  a,b,c,d,e,f,g,h,i,
  ncol = 2, labels = c("A","B","C","D","E","F","G","H","I"),
  align = "hv"
)


control_categories_hist_panel <- function(df,
                                          fill_as_aesthetic = TRUE,
                                          tilt_labels      = TRUE) {
  ## ── 0.  Utility look-ups (unchanged) ────────────────────────────────────────
  control_variable_synonyms <- get_control_variable_mappings()
  category_order            <- get_category_order()
  category_colors           <- get_category_colors()
  
  ## ── 1.  Pre-processing – verbatim from create_control_categories_plot() ────
  df_unique <- df %>% dplyr::distinct(PMID, controls)          # one row per paper
  df_controls <- as.character(df_unique$controls)
  df_controls[is.na(df_controls)] <- ""
  
  categories <- unique(vapply(control_variable_synonyms, `[[`, "", "category"))
  
  # build pattern lists by category
  category_patterns <- list()
  for (ctrl in names(control_variable_synonyms)) {
    meta <- control_variable_synonyms[[ctrl]]
    cat  <- meta$category
    category_patterns[[cat]] <- c(category_patterns[[cat]],
                                  ctrl, meta$synonyms)
  }
  
  # TRUE/FALSE matrix: rows = papers, cols = categories
  category_presence <- matrix(
    FALSE,
    nrow = nrow(df_unique),
    ncol = length(categories),
    dimnames = list(NULL, categories)
  )
  
  for (cat in names(category_patterns)) {
    pat <- paste0("\\b(",
                  paste(unique(category_patterns[[cat]]), collapse = "|"),
                  ")\\b")
    category_presence[, cat] <- stringr::str_detect(
      stringr::str_to_lower(df_controls),
      stringr::str_to_lower(pat)
    )
  }
  
  ## ── 2.  Reshape to long format – one row per (PMID, category) ──────────────
  long_df <- category_presence %>%
    as.data.frame() %>%
    dplyr::mutate(PMID = df_unique$PMID) %>%
    tidyr::pivot_longer(
      cols      = all_of(categories),
      names_to  = "category",
      values_to = "present"
    ) %>%
    dplyr::filter(present) %>%              # keep only matched categories
    dplyr::select(PMID, category)
  
  ## ── 3.  Plot with your existing helper hist_panel() ────────────────────────
  p <- hist_panel(
    long_df,
    col               = "category",
    group_col         = "PMID",
    discrete          = TRUE,
    use_proportion    = TRUE,
    x.label           = "Control category",
    fill_as_aesthetic = fill_as_aesthetic,
    tilt_labels       = tilt_labels
  )
  
  ## optional: keep the palette & order you already defined
  if (fill_as_aesthetic) {
    p <- p + ggplot2::scale_fill_manual(values = category_colors)
  }
  p <- p + ggplot2::scale_x_discrete(limits = category_order)
  
  p
}

# ────────────────────────────────────────────────────────────────────────────────
#  Re-use category-detection code, but plot with hist_panel() in one colour
# ────────────────────────────────────────────────────────────────────────────────
control_categories_hist_panel <- function(df, tilt_labels = TRUE) {
  ## 0. Look-ups ---------------------------------------------------------------
  control_variable_synonyms <- get_control_variable_mappings()
  
  ## 1. Pre-processing ----------
  df_unique    <- dplyr::distinct(df, PMID, controls)
  df_controls  <- ifelse(is.na(df_unique$controls), "", df_unique$controls)
  
  # accumulate patterns per category
  category_patterns <- list()
  for (nm in names(control_variable_synonyms)) {
    meta <- control_variable_synonyms[[nm]]
    category_patterns[[meta$category]] <-
      c(category_patterns[[meta$category]], nm, meta$synonyms)
  }
  categories <- names(category_patterns)
  
  # TRUE/FALSE presence matrix
  category_presence <- matrix(
    FALSE,
    nrow = nrow(df_unique),
    ncol = length(categories),
    dimnames = list(NULL, categories)
  )
  for (cat in categories) {
    pat <- paste0("\\b(", paste(unique(category_patterns[[cat]]), collapse = "|"), ")\\b")
    category_presence[, cat] <- stringr::str_detect(
      tolower(df_controls),
      tolower(pat)
    )
  }
  
  ## 2. Long data frame for hist_panel() ---------------------------------------
  long_df <- category_presence %>%
    as.data.frame() %>%
    dplyr::mutate(PMID = df_unique$PMID) %>%
    tidyr::pivot_longer(
      cols      = categories,
      names_to  = "category",
      values_to = "present"
    ) %>%
    dplyr::filter(present) %>%          # keep only matches
    dplyr::select(PMID, category)
  
  ## 3. Plot  – one uniform colour, no fill aesthetic --------------------------
  hist_panel(
    long_df,
    col               = "category",
    group_col         = "PMID",
    discrete          = TRUE,
    use_proportion    = TRUE,
    x.label           = "Control category",
    fill_as_aesthetic = FALSE,   # <- single colour, matches other hist_panel plots
    tilt_labels       = tilt_labels
  )
}












# proportion works weirdly, and when i do count, the histogram moves

# number of subject per group? clustering by window type?

# not sure if we need
hist_panel(df = df_included, col = "permutations", x.label = "Number of Permutations", binwidth = 1000)
# dont really need: 
hist_panel(df = df_included, col = "hypothesis", x.label = "Number of Conditions", discrete = T)
hist_panel(df = df_included, col = "value", x.label = "Number of Conditions", discrete = T)



# improve stat code for basic usage (regression, anova conditions, ttest conditions or groups) + hedges g


# can try to generate examples with number of trials and hypothesized signal to noise