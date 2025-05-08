#' Otsu's Thresholding with Smoothed Histogram:
#'
#' This function applies Otsu's thresholding to a smoothed histogram to determine the optimal threshold
#' for image segmentation.
#'
#' @param hist_vals A numeric vector of histogram counts.
#' @param mids A numeric vector of histogram bin midpoints.
#' @return The optimal threshold value.
#' @examples
#' \donttest{
#'   hist_vals <- hist(rnorm(1000), plot = FALSE)$counts
#'   mids <- hist(rnorm(1000), plot = FALSE)$mids
#'   threshold <- otsu_threshold_smoothed(hist_vals, mids)
#'   print(threshold)
#' }
#' @export
# Otsu's thresholding function
otsu_threshold_smoothed <- function(hist_vals, mids) {
  hist_vals[is.na(hist_vals)] <- 0  # Handle NA values after smoothing
  total_pixels <- sum(hist_vals)   # Total number of pixels
  cumulative_sum <- cumsum(hist_vals)
  cumulative_mean <- cumsum(hist_vals * mids)
  global_mean <- cumulative_mean[length(cumulative_mean)] / total_pixels
  max_variance <- 0
  optimal_threshold <- 0
  for (t in 1:(length(hist_vals) - 1)) {
    weight_background <- cumulative_sum[t] / total_pixels
    weight_foreground <- 1 - weight_background

    if (cumulative_sum[t] == 0 || (total_pixels - cumulative_sum[t]) == 0) {
      next
    }

    mean_background <- cumulative_mean[t] / cumulative_sum[t]
    mean_foreground <- (global_mean * total_pixels - cumulative_mean[t]) / (total_pixels - cumulative_sum[t])
    between_class_variance <- weight_background * weight_foreground * (mean_background - mean_foreground)^2

    if (between_class_variance > max_variance) {
      max_variance <- between_class_variance
      optimal_threshold <- mids[t]  # Get corresponding intensity value
    }
  }

  return(optimal_threshold)
}
