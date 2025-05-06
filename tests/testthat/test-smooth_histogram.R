test_that("smooth_histogram returns numeric vector of same length", {
  raw_counts <- c(1, 3, 7, 9, 5, 2, 0, 4, 6, 8)
  smoothed <- smooth_histogram(raw_counts)

  expect_type(smoothed, "double")
  expect_length(smoothed, length(raw_counts))
})

test_that("smooth_histogram smooths sharp peaks", {
  raw_counts <- c(0, 0, 10, 0, 0)
  smoothed <- smooth_histogram(raw_counts, window_size = 3)

  # Valor central debería ser menor que 10 tras suavizado
  expect_true(smoothed[3] < 10)
})

test_that("smooth_histogram handles NAs", {
  raw_counts <- c(1, NA, 3, NA, 5, 6, 7)
  smoothed <- smooth_histogram(raw_counts, window_size = 3)

  expect_type(smoothed, "double")
  expect_length(smoothed, length(raw_counts))
  # No debería devolver un error aunque haya NAs
  expect_true(all(is.na(smoothed) | is.numeric(smoothed)))
})
