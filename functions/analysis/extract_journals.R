# Extract unique journals from citations and save to CSV
extract_journals <- function(df_full, output_dir = "results") {
  
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
  df_full$Journal <- sapply(df_full$citation, extract_journal)
  
  # Create mapping of journal abbreviations to full names
  journal_name_mapping <- c(
    "Biol Psychol" = "Biological Psychology",
    "Cereb Cortex" = "Cerebral Cortex",
    "Clin Neurophysiol" = "Clinical Neurophysiology",
    "Cogn Affect Behav Neurosci" = "Cognitive, Affective, & Behavioral Neuroscience",
    "Elife" = "eLife",
    "Eur Eat Disord Rev" = "European Eating Disorders Review",
    "Eur J Psychotraumatol" = "European Journal of Psychotraumatology",
    "Front Hum Neurosci" = "Frontiers in Human Neuroscience",
    "Front Neurosci" = "Frontiers in Neuroscience",
    "Front Neurol" = "Frontiers in Neurology",
    "Front Physiol" = "Frontiers in Physiology",
    "Front Psychol" = "Frontiers in Psychology",
    "Front Psychiatry" = "Frontiers in Psychiatry",
    "Eur J Neurosci" = "European Journal of Neuroscience",
    "Int J Neurosci" = "International Journal of Neuroscience",
    "Hum Brain Mapp" = "Human Brain Mapping",
    "Int J Psychophysiol" = "International Journal of Psychophysiology",
    "J Affect Disord" = "Journal of Affective Disorders",
    "J Altern Complement Med" = "Journal of Alternative and Complementary Medicine",
    "J Clin Neurol" = "Journal of Clinical Neurology",
    "J Neurol Neurosurg Psychiatry" = "Journal of Neurology, Neurosurgery & Psychiatry",
    "J Neurosci" = "Journal of Neuroscience",
    "J Neurosci Methods" = "Journal of Neuroscience Methods",
    "J Psychosom Res" = "Journal of Psychosomatic Research",
    "J Relig Health" = "Journal of Religion and Health",
    "JACC Clin Electrophysiol" = "JACC: Clinical Electrophysiology",
    "Med Biol Eng Comput" = "Medical & Biological Engineering & Computing",
    "Nat Commun" = "Nature Communications",
    "Nat Neurosci" = "Nature Neuroscience",
    "Neuroimage" = "NeuroImage",
    "NeuroImage" = "NeuroImage",
    "Neurocrit Care" = "Neurocritical Care",
    "Neurosci Bull" = "Neuroscience Bulletin",
    "Neurosci Lett" = "Neuroscience Letters",
    "PLoS Biol" = "PLOS Biology",
    "PLoS One" = "PLOS ONE",
    "Philos Trans R Soc Lond B Biol Sci" = "Philosophical Transactions of the Royal Society B: Biological Sciences",
    "Physiol Behav" = "Physiology & Behavior",
    "Proc Natl Acad Sci U S A" = "Proceedings of the National Academy of Sciences",
    "Prog Neuropsychopharmacol Biol Psychiatry" = "Progress in Neuro-Psychopharmacology and Biological Psychiatry",
    "Psychol Med" = "Psychological Medicine",
    "Psychosom Med" = "Psychosomatic Medicine",
    "Sci Rep" = "Scientific Reports",
    "Soc Cogn Affect Neurosci" = "Social Cognitive and Affective Neuroscience",
    "Brain Res" = "Brain Research",
    "Electroencephalogr Clin Neurophysiol" = "Electroencephalography and Clinical Neurophysiology",
    "Neuroimage Clin" = "NeuroImage: Clinical",
    "Am J Respir Crit Care Med" = "American Journal of Respiratory and Critical Care Medicine",
    "Ann Clin Transl Neurol" = "Annals of Clinical and Translational Neurology",
    "Appl Psychophysiol Biofeedback" = "Applied Psychophysiology and Biofeedback",
    "Auton Neurosci" = "Autonomic Neuroscience",
    "BMJ Neurol Open" = "BMJ Neurology Open",
    "Biol Psychiatry" = "Biological Psychiatry",
    "Biol Psychiatry Cogn Neurosci Neuroimaging" = "Biological Psychiatry: Cognitive Neuroscience and Neuroimaging",
    "Borderline Personal Disord Emot Dysregul" = "Borderline Personality Disorder and Emotion Dysregulation",
    "Brain Commun" = "Brain Communications",
    "Brain Sci" = "Brain Sciences",
    "Brain Stimul" = "Brain Stimulation",
    "Cereb Cortex Commun" = "Cerebral Cortex Communications",
    "Commun Biol" = "Communications Biology"
  )
  
  # Apply mapping to get full journal names
  df_full$Journal_Full <- ifelse(
    df_full$Journal %in% names(journal_name_mapping),
    journal_name_mapping[df_full$Journal],
    df_full$Journal  
  )
  
  # Filter to included studies only
  df_full_included <- df_full %>% filter(include == 1)
  
  # Get unique journals with counts (using full names)
  journal_counts <- df_full_included %>%
    group_by(Journal_Full, Journal) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(!is.na(Journal_Full)) %>%
    arrange(desc(count), Journal_Full) %>%
    select(Journal_Full, Journal_Abbreviation = Journal, count)
  
  # Save to CSV file
  csv_file <- file.path(output_dir, "unique_journals.csv")
  write.csv(journal_counts, csv_file, row.names = FALSE)
    
}
