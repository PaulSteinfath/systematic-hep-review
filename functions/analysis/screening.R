get_export_stats <- function(exports_path) {
  df_query1 <- read.csv(file.path(exports_path, '20240115_pubmed_query1.csv'))
  df_query2 <- read.csv(file.path(exports_path, '20240724_pubmed_query2.csv'))
  df_query2_rerun <- read.csv(file.path(exports_path, '20240805_pubmed_query2.csv'))
  df_query3 <- read.csv(file.path(exports_path, '20240805_pubmed_query3.csv'))
  
  df_merged <- df_query1 %>%
    bind_rows(df_query2) %>%
    bind_rows(df_query2_rerun) %>%
    bind_rows(df_query3) %>%
    distinct(PMID)
  
  list(
    num_query1 = nrow(df_query1),
    num_query2 = nrow(df_query2),
    num_query2_rerun = nrow(df_query2_rerun),
    num_query3 = nrow(df_query3),
    num_merged = nrow(df_merged)
  )
}