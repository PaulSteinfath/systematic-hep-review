library(tools)

get_check_sums <- function(results_path) {
  checksums <- tools::md5sum(dir(results_path, full.names = T))
  df_sums <- data.frame(filename = unlist(lapply(names(checksums), basename)), 
                        md5 = as.vector(checksums))
  
  df_sums
}