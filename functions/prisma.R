plot_prisma_diagram <- function(df_prisma) {
  prisma_data <- PRISMA_data(df_prisma)
  PRISMA_flowdiagram(prisma_data, previous = F, other = T, 
                     detail_databases = T, fontsize = 14)
}


reasons_to_exclude <- function(df_excluded) {
  ignore <- c("Consider", "Duplicate", "Preprint")
 
  reasons <- df_excluded %>%
    filter(Freq > 0) %>%
    filter(!(comment %in% ignore)) %>%
    select(c("comment", "Freq")) %>%
    arrange(-Freq, decreasing = T) %>%
    mutate(Desc = paste0(comment, ", ", Freq))
  
  paste(reasons$Desc, collapse = "; ") 
}


generate_prisma <- function(df_screening, template_path, derivatives_path, 
                            plot_path, ext = 'svg') {
  # Process all reasons to exclude
  comments <- with(df_screening, as.data.frame(table(source, comment, include)))
  excluded <- comments[comments$include == 0,]
  excluded_pubmed <- excluded[excluded$source == "pubmed",]
  excluded_manual <- excluded[excluded$source == "manual",]
  reasons_pubmed <- reasons_to_exclude(excluded_pubmed)
  reasons_manual <- reasons_to_exclude(excluded_manual)
  
  # NOTE: duplicates and preprints only appear in Pubmed results
  num_duplicates <- excluded_pubmed$Freq[excluded_pubmed$comment == "Duplicate"]
  num_preprints <- excluded_pubmed$Freq[excluded_pubmed$comment == "Preprint"]
  
  # Prepare all numbers
  num_pubmed <- nrow(df_screening[df_screening$source == "pubmed",])
  num_screened <- num_pubmed - num_preprints - num_duplicates
  num_manual <- nrow(df_screening[df_screening$source == "manual",])
  num_included <- nrow(df_screening[df_screening$Include == 1,])
  
  # Fill in the template PRISMA CSV file
  df_prisma <- read.csv(template_path)
  
  # Pubmed
  df_prisma$n[df_prisma$data == "database_specific_results"] <- paste("Pubmed,", num_pubmed)
  df_prisma$n[df_prisma$data == "records_screened"] <- num_screened
  df_prisma$n[df_prisma$data == "dbr_sought_reports"] <- num_screened
  df_prisma$n[df_prisma$data == "dbr_assessed"] <- num_screened
  df_prisma$n[df_prisma$data == "dbr_excluded"] <- reasons_pubmed
  df_prisma$n[df_prisma$data == "new_studies"] <- num_included
  df_prisma$n[df_prisma$data == "duplicates"] <- num_duplicates
  df_prisma$n[df_prisma$data == "excluded_other"] <- num_preprints
  
  # Manual
  df_prisma$n[df_prisma$data == "citations_results"] <- num_manual
  df_prisma$n[df_prisma$data == "other_sought_reports"] <- num_manual
  df_prisma$n[df_prisma$data == "other_assessed"] <- num_manual
  df_prisma$n[df_prisma$data == "other_excluded"] <- reasons_manual
  
  # Remove unnecessary parts of the visualization by making the corresponding 
  # rows empty
  df_prisma$n[df_prisma$data == "database_results"] <- NA
  df_prisma$n[df_prisma$data == "register_results"] <- NA
  df_prisma$n[df_prisma$data == "website_results"] <- NA
  df_prisma$n[df_prisma$data == "organisation_results"] <- NA
  df_prisma$n[df_prisma$data == "new_reports"] <- NA
  df_prisma$n[df_prisma$data == "excluded_automatic"] <- NA
  
  # Adjust the box text
  df_prisma$boxtext[df_prisma$data == "excluded_other"] <- "Preprints"
  df_prisma$boxtext[df_prisma$data == "identification"] <- "Search"
  
  # Save the resulting dataframe
  write.csv(df_prisma, file.path(derivatives_path, 'PRISMA.csv'), row.names = F)
  
  # Plot the resulting diagram
  p_prisma <- plot_prisma_diagram(df_prisma)
  PRISMA_save(
    p_prisma, 
    filename = file.path(plot_path, paste0("PRISMA.", ext)),
    filetype = ext,
    overwrite = T
  )
}