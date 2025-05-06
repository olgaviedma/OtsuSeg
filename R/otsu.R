#' Binarize a Raster Using Otsu's Thresholding (with Inter-Class and Intra-Class Variance)
#'
#' This function computes deltaNBR, rescales it, applies a smoothed histogram,
#' and uses Otsu's thresholding to create a binary raster representing burn scars.
#' It also plots the smoothed histogram, inter-class variance curve, and the
#' inter-class and intra-class variance curves on separate plots.
#'
#' @param x A raster layer object representing post-fire NBR (e.g., `raster::RasterLayer`).
#' @param y A raster layer object representing pre-fire NBR (e.g., `raster::RasterLayer`).
#' @param output_shapefile Logical. Whether to save the binary result as a shapefile. Default is TRUE.
#' @param shapefile_path Character. Path to save the shapefile. Used only if output_shapefile is TRUE.
#' @return A list containing:
#'   \item{best_threshold}{The computed Otsu threshold.}
#'   \item{area_hectares}{Estimated burned area in hectares.}
#'   \item{binary_raster_smoothed}{The binary raster layer created using the Otsu threshold.}
#'   \item{plots}{A list of plots: histogram and inter/intra-class variance plot.}
#' @importFrom graphics par abline legend axis mtext
#' @import raster
#' @import zoo
#' @importFrom sf st_as_sf st_write
#' @export
#' @examples
#' \dontrun{
#'   pre_fire <- get_external_data("NBRpre", load = TRUE)
#'   post_fire <- get_external_data("NBRpost", load = TRUE)
#'   result <- binarize_raster(post_fire, pre_fire)
#'   print(result$area_hectares)
#' }
binarize_raster <- function(x, y, output_shapefile = TRUE, shapefile_path = "binary_raster.shp") {
  # Ensure that x and y are RasterLayer objects
  if (!inherits(x, "RasterLayer") | !inherits(y, "RasterLayer")) {
    stop("Both x and y must be RasterLayer objects.")
  }

  # Compute deltaNBR
  deltaNBR <- x - y

  # Rescale deltaNBR to 0-255
  min_val <- min(values(deltaNBR), na.rm = TRUE)
  max_val <- max(values(deltaNBR), na.rm = TRUE)
  deltaNBR_rescaled <- (deltaNBR - min_val) / (max_val - min_val) * 255

  # Compute histogram of deltaNBR_rescaled
  hist_values <- hist(values(deltaNBR_rescaled), breaks = 256, plot = FALSE)

  # Smooth the histogram counts
  smoothed_counts <- smooth_histogram(hist_values$counts)

  # Handle NAs in smoothed_counts
  smoothed_counts[is.na(smoothed_counts)] <- 0

  # Otsu’s thresholding from smoothed histogram
  threshold_value_smoothed <- otsu_threshold_smoothed(smoothed_counts, hist_values$mids)

  # Binarize the raster using the threshold
  binary_raster_smoothed <- deltaNBR_rescaled
  binary_raster_smoothed[] <- ifelse(deltaNBR_rescaled[] > threshold_value_smoothed, 1, 0)

  # Calculate the area in hectares
  pixel_area_m2 <- res(binary_raster_smoothed)[1] * res(binary_raster_smoothed)[2]  # pixel area in square meters
  num_pixels <- sum(binary_raster_smoothed[] == 1, na.rm = TRUE)  # number of foreground pixels
  total_area_m2 <- num_pixels * pixel_area_m2  # total area in square meters
  total_area_hectares <- total_area_m2 / 10000  # convert area to hectares

  # Optionally, convert only the '1' pixels to polygons
  binary_polygon <- rasterToPolygons(binary_raster_smoothed, fun = function(x) x == 1, na.rm = TRUE, dissolve = TRUE)

  # Convert the SpatialPolygonsDataFrame to an sf object
  binary_sf <- sf::st_as_sf(binary_polygon)

  # If output_shapefile is TRUE, save the shapefile to disk
  if (output_shapefile) {
    sf::st_write(binary_sf, shapefile_path)
    cat("Shapefile saved to", shapefile_path, "\n")
  }

  # Compute interclass variance for all threshold values using the same approach as in otsu_threshold_smoothed
  total_pixels <- sum(hist_values$counts)
  cumulative_sum <- cumsum(hist_values$counts)
  cumulative_mean <- cumsum(hist_values$counts * hist_values$mids)
  global_mean <- cumulative_mean[length(cumulative_mean)] / total_pixels
  interclass_variance_curve <- sapply(1:(length(hist_values$counts) - 1), function(t) {
    weight_background <- cumulative_sum[t] / total_pixels
    weight_foreground <- 1 - weight_background

    if (cumulative_sum[t] == 0 || (total_pixels - cumulative_sum[t]) == 0) {
      return(NA)  # Avoid division by zero
    }

    mean_background <- cumulative_mean[t] / cumulative_sum[t]
    mean_foreground <- (global_mean * total_pixels - cumulative_mean[t]) / (total_pixels - cumulative_sum[t])

    weight_background * weight_foreground * (mean_background - mean_foreground)^2
  })

  # Compute intra-class variance
  intra_class_variance_curve <- sapply(1:(length(hist_values$counts) - 1), function(t) {
    weight_background <- cumulative_sum[t] / total_pixels
    weight_foreground <- 1 - weight_background

    if (cumulative_sum[t] == 0 || (total_pixels - cumulative_sum[t]) == 0) {
      return(NA)  # Avoid division by zero
    }

    mean_background <- cumulative_mean[t] / cumulative_sum[t]
    mean_foreground <- (global_mean * total_pixels - cumulative_mean[t]) / (total_pixels - cumulative_sum[t])

    # Intra-class variance (sum of squared differences within each class)
    background_variance <- sum((hist_values$mids[1:t] - mean_background)^2 * hist_values$counts[1:t]) / cumulative_sum[t]
    foreground_variance <- sum((hist_values$mids[(t+1):length(hist_values$mids)] - mean_foreground)^2 * hist_values$counts[(t+1):length(hist_values$mids)]) / (total_pixels - cumulative_sum[t])

    return(weight_background * background_variance + weight_foreground * foreground_variance)
  })

  # Set up plotting layout (2 rows, 1 column)
  graphics::par(mfrow = c(1, 2))  # 2 rows, 1 column layout

  # First Plot: Smoothed Histogram + Inter-class Variance Curve + Optimal Threshold Line
  plot(hist_values$mids, smoothed_counts, type = "h", col = "grey", lwd = 2,
       xlab = "Intensity", ylab = "Frequency", cex = 0.8)
  graphics::par(new = TRUE)  # Allow overlaying a second plot

  # Add inter-class variance curve on a new Y-axis
  plot(hist_values$mids[-1], interclass_variance_curve, type = "l", col = "blue", lwd = 2,
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
  axis(side = 4, col.axis = "blue")  # Add labels to the secondary axis
  mtext("Inter-Class Variance", side = 4, line = 2, col = "blue")  # Label the second Y-axis

  # Add threshold line
  abline(v = threshold_value_smoothed, col = "red", lty = 2)

  # Add legend with the threshold value
  legend("topright", legend = c("Smoothed Histogram", "Inter-Class Variance",
                                paste("Optimal Threshold (", round(threshold_value_smoothed, 2), ")", sep = "")),
         col = c("grey", "blue", "red"), lty = c(1, 1, 2), lwd = c(2, 2, 1), cex = 0.8)


  # Second Plot: Inter-Class vs Intra-Class Variance Curve (Superimposed)
  plot(hist_values$mids[-1], interclass_variance_curve, type = "l", col = "blue", lwd = 2,
       xlab = "Threshold",ylab="")
  lines(hist_values$mids[-1], intra_class_variance_curve, col = "green", lwd = 2)
  abline(v = threshold_value_smoothed, col = "red", lty = 2)  # Add the threshold line
  legend("topright", legend = c("Inter-Class Variance", "Intra-Class Variance", "Optimal Threshold"),
         col = c("blue", "green", "red"), lty = c(1, 1, 2), lwd = c(2, 2, 1), cex = 0.8)

  # Reset par to default layout
  graphics::par(mfrow = c(1, 1))

  # Return the result
  result <- list(
    best_threshold = threshold_value_smoothed,
    area_hectares = total_area_hectares,
    binary_raster_smoothed = binary_raster_smoothed,
    binary_shapefile = binary_sf  # Return shapefile as an sf object
  )
  return(result)
}
