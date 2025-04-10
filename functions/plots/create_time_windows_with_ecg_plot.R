create_time_windows_with_ecg_plot <- function(df, averaging_type = "both") {
  
  shared_limits <- c(-300, 1000)
  
  # if averaging_type "both", creates plots for averaging and clustering
  if (averaging_type == "both") {
    avg_plot <- create_single_ecg_plot(df, "1", shared_limits, "Averaging")
    cluster_plot <- create_single_ecg_plot(df, "0", shared_limits, "Clustering")
    return(list(averaging = avg_plot, clustering = cluster_plot))
  } else {
    # For specific averaging_type, return just that plot
    return(create_single_ecg_plot(df, averaging_type, shared_limits, 
                                  ifelse(averaging_type == "1", "Averaging", "Clustering")))
  }
}
