source(file.path(getwd(), '..', 'functions', 'preprocess.R'))

test_that("All except XX is resolved correctly", {
  all_locs <- "C1, C2, C3, C4, C5"
  except_locs <- "C2, C7"
  kept_locs <- resolve_all_except(list(all_locs, except_locs))
  
  expect_false(grepl("C2", kept_locs))
  expect_true(grepl("C1", kept_locs))
})