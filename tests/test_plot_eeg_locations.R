source(file.path(getwd(), '..', 'functions', 'plots', 'plot_eeg_locations.R'))

create_test_data <- function() {
  # Add a test layout for debugging
  ch_names[["test"]] <- c("Ch1", "Ch2", "Ch3", "Ch4")
  
  data.frame(
    PMID = c(1, 2, 3),
    meeg_layout = "test",
    meeg_locations = c("Ch1, Ch2", "Ch1", "Ch1, Ch2, Ch3"),
    hep_channels_selected = c("Ch1", "Ch1", "Ch1")
  )
}

test_that("get_channel_freq, channel is present in the selection", {
  df <- create_test_data()
  expect_equal(get_channel_freq(df, "hep_channels_selected", "Ch1"), 1.0)
})

test_that("get_channel_freq, channel is NOT present in the selection", {
  df <- create_test_data()
  expect_equal(get_channel_freq(df, "hep_channels_selected", "Ch2"), 0.0)
})

test_that("get_channel_freq, channel is NOT present in the layouts", {
  df <- create_test_data()
  expect_equal(get_channel_freq(df, "hep_channels_selected", "Ch4"), 0.0)
})

test_that("count_occurrences, all PMIDs are unique", {
  df <- create_test_data()
  df_freqs <- count_occurrences(df, "hep_channels_selected", add.locs = F)
  
  # only channel 1 is present in the selection
  expect_equal(df_freqs$freq, c(1.0, 0.0, 0.0, 0.0))
})
