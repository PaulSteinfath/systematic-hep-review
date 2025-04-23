source(file.path(getwd(), '..', 'functions', 'plots', 'utils.R'))

test_that("clip_values works properly", {
  expect_equal(clip_values(c(-0.1, 0.0, 1.0, 1.1)), c(0.0, 0.0, 1.0, 1.0))
})