#' @keywords internal
#' @title Smooth Histogram Data
#' @description
#' This is an internal function that applies a moving average filter to smooth a histogram.
#' It is not intended for direct use by package users.
#'
#' @param hist_counts A numeric vector of histogram bin counts.
#' @param window_size An integer specifying the smoothing window size (default is 5).
#' @return A numeric vector of smoothed histogram counts.
#' @importFrom zoo rollapply
#' @examples
#' \donttest{
#'   raw_counts <- hist(rnorm(1000), plot = FALSE)$counts
#'   smoothed <- smooth_histogram(raw_counts)
#'   plot(smoothed, type = "l", col = "blue")
#' }
#' @export
smooth_histogram <- function(hist_counts, window_size = 5) {
  smoothed_counts <- zoo::rollapply(hist_counts, width = window_size, FUN = mean, align = "center", fill = NA)
  return(smoothed_counts)
}
