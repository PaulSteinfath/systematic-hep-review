func_path <- "/data/hu_steinfath/Desktop/Code/systematic-hep-review/functions"
source(file.path(func_path, "plots", "utils.R"))

calculate_peak_distribution <- function(method_data) {
  all_times <- c(method_data$start_time, method_data$end_time)
  time_range <- seq(floor(min(all_times)), ceiling(max(all_times)), by = 1)
  counts <- calculate_cumulative_counts(method_data, time_range)
  
  # Find peak/mode of distribution 
  peak_time <- time_range[which.max(counts)]
  
  return(list(
    peak_time = peak_time,
    max_count = max(counts)
  ))
}

# Perform permutation test comparing peak timing distributions
#  
# @param method1_data Data frame with start_time and end_time columns for method 1
# @param method2_data Data frame with start_time and end_time columns for method 2
# @param n_permutations Number of permutations to perform (default: 1000)
# @param seed Random seed for reproducibility (default: 42)
# @return List containing test results and statistics
perform_peak_timing_comparison <- function(method1_data, method2_data, n_permutations = 1000, seed = 42) {
  
  # Calculate distributions for both methods
  dist1 <- calculate_peak_distribution(method1_data)
  dist2 <- calculate_peak_distribution(method2_data)
  
  # Perform permutation test for peak difference
  observed_peak_diff <- abs(dist1$peak_time - dist2$peak_time)
  
  # Combine all data for permutation
  combined_data <- rbind(method1_data, method2_data)
  n1 <- nrow(method1_data)
  n2 <- nrow(method2_data)
  n_total <- n1 + n2
  
  # Perform permutation test
  set.seed(seed)
  permuted_diffs <- numeric(n_permutations)
  
  for(i in 1:n_permutations) {
    # Randomly permute the combined data
    shuffled_indices <- sample(n_total)
    perm_group1 <- combined_data[shuffled_indices[1:n1], ]
    perm_group2 <- combined_data[shuffled_indices[(n1+1):n_total], ]
    
    # Calculate peak difference for this permutation
    perm_dist1 <- calculate_peak_distribution(perm_group1)
    perm_dist2 <- calculate_peak_distribution(perm_group2)
    
    if(!is.na(perm_dist1$peak_time) && !is.na(perm_dist2$peak_time)) {
      permuted_diffs[i] <- abs(perm_dist1$peak_time - perm_dist2$peak_time)
    } else {
      permuted_diffs[i] <- 0
    }
  }
  
  # Calculate p-value: proportion of permuted differences >= observed difference
  p_value_perm <- mean(permuted_diffs >= observed_peak_diff)
  
  return(list(
    peak1 = dist1$peak_time,
    peak2 = dist2$peak_time,
    peak_difference = observed_peak_diff,
    max_count1 = dist1$max_count,
    max_count2 = dist2$max_count,
    permutation_p_value = p_value_perm,
    n_permutations = n_permutations,
    n1 = n1,
    n2 = n2
  ))
}


