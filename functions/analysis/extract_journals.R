# Add journal column
add_journal_column <- function(df) {
  
  # Extract journals from Citation column
  # PubMed format: "Journal. Year Month;Volume(Issue):Pages. doi: xxx"
  extract_journal <- function(citation) {
    # The journal is the first part before the period
    parts <- strsplit(citation, "\\.")[[1]]
    
    if (length(parts) >= 1) {
      journal <- parts[1]
      journal <- trimws(journal)
      return(journal)
    }
    
    return(NA)
  }
  
  # Apply to all citations
  df$journal <- sapply(df$citation, extract_journal)
  
  # Apply mapping to get full journal names
  df$journal_full <- ifelse(
    df$journal %in% names(journal_name_mapping),
    journal_name_mapping[df$journal],
    df$journal  
  )
  
  return(df)
}

# Count journals and save to CSV
count_journals <- function(df, output_dir = "results") {
    
  # Get unique journals with counts 
  journal_counts <- df %>%
    distinct(PMID, .keep_all = TRUE) %>% 
    group_by(journal_full, journal) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count), journal_full) %>%
    select(journal_full, journal_abbreviation = journal, count)
  
  # Save to CSV file
  csv_file <- file.path(output_dir, "unique_journals.csv")
  write.csv(journal_counts, csv_file, row.names = FALSE)
  
  return(journal_counts)
}
