test_that("otsu_threshold_smoothed returns a numeric threshold within range", {
  set.seed(123)
  values <- rnorm(1000)
  hist_data <- hist(values, plot = FALSE)
  hist_vals <- hist_data$counts
  mids <- hist_data$mids

  threshold <- otsu_threshold_smoothed(hist_vals, mids)

  expect_type(threshold, "double")
  expect_true(threshold >= min(mids) && threshold <= max(mids))
})

test_that("otsu_threshold_smoothed handles NA and zero counts gracefully", {
  hist_vals <- c(NA, 10, 0, 5, NA)
  mids <- c(1, 2, 3, 4, 5)

  threshold <- otsu_threshold_smoothed(hist_vals, mids)

  expect_type(threshold, "double")
  expect_false(is.na(threshold))
  expect_true(threshold >= min(mids) && threshold <= max(mids))
})

