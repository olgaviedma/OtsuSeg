#' Binarize a Raster Using Otsu's Thresholding (with Inter-Class and Intra-Class Variance)
#'
#' This function computes deltaNBR (difference between post-fire and pre-fire NBR), rescales it,
#' applies a smoothed histogram, and uses Otsu's thresholding to create a binary raster representing burn scars.
#' It also generates and saves plots of the smoothed histogram, inter-class variance curve, and the
#' inter-class and intra-class variance curves on separate plots.
#'
#' @param x RasterLayer. A raster layer object representing pre-fire NBR (e.g., `raster::RasterLayer`).
#' @param y RasterLayer. A raster layer object representing post-fire NBR (e.g., `raster::RasterLayer`).
#' @param output_shapefile Logical. If TRUE, saves the binary raster as a shapefile. Default is TRUE.
#' @param shapefile_path Character. Path to save the shapefile. Used only if output_shapefile is TRUE.
#'
#' @return A list containing:
#'   \item{best_threshold}{Numeric. The computed Otsu threshold value.}
#'   \item{area_hectares}{Numeric. The estimated burned area in hectares.}
#'   \item{binary_raster_smoothed}{RasterLayer. The binary raster created using the Otsu threshold.}
#'   \item{binary_shapefile}{sf object. The binary shapefile created, if output_shapefile is TRUE.}
#'   \item{shapefile_path}{Character. Path where the shapefile was saved, if output_shapefile is TRUE.}
#'
#' @importFrom graphics par abline legend axis mtext
#' @import raster
#' @import zoo
#' @importFrom sf st_as_sf st_write
#' @examplesIf interactive()
#' #For CRAN checks, a temporary directory is used to avoid leaving files.
#' #For permanent use, specify a path like "results/binary_raster.shp"
#'   pre_fire <- get_external_data("NBRpre.tif", load = TRUE)
#'   post_fire <- get_external_data("NBRpost.tif", load = TRUE)
#'   shapefile_path <- file.path(tempdir(), "binary_raster.shp")
#'   result <- binarize_raster(pre_fire, post_fire, shapefile_path = shapefile_path)
#'   print(result$area_hectares)
#'   # Clean up (optional)
#'   unlink(list.files(tempdir(), pattern = "binary_raster\\.(shp|shx|dbf|prj)$", full.names = TRUE))
#' @export
binarize_raster <- function(x, y, output_shapefile = TRUE, shapefile_path = "binary_raster.shp") {
  # Ensure that x and y are RasterLayer objects
  if (!inherits(x, "RasterLayer") | !inherits(y, "RasterLayer")) {
    stop("Both x and y must be RasterLayer objects.")
  }

  # Preserve original graphics settings
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  # Compute deltaNBR
  deltaNBR <- x - y

  # Rescale deltaNBR to 0-255
  min_val <- min(values(deltaNBR), na.rm = TRUE)
  max_val <- max(values(deltaNBR), na.rm = TRUE)
  deltaNBR_rescaled <- (deltaNBR - min_val) / (max_val - min_val) * 255

  # Compute histogram of deltaNBR_rescaled
  hist_values <- hist(values(deltaNBR_rescaled), breaks = 256, plot = FALSE)
  smoothed_counts <- smooth_histogram(hist_values$counts)
  smoothed_counts[is.na(smoothed_counts)] <- 0

  # Otsu’s thresholding from smoothed histogram
  threshold_value_smoothed <- otsu_threshold_smoothed(smoothed_counts, hist_values$mids)
  binary_raster_smoothed <- deltaNBR_rescaled
  binary_raster_smoothed[] <- ifelse(deltaNBR_rescaled[] > threshold_value_smoothed, 1, 0)

  # Calculate the area in hectares
  pixel_area_m2 <- res(binary_raster_smoothed)[1] * res(binary_raster_smoothed)[2]
  num_pixels <- sum(binary_raster_smoothed[] == 1, na.rm = TRUE)
  total_area_hectares <- (num_pixels * pixel_area_m2) / 10000

  # Convert binary raster to polygons
  binary_polygon <- rasterToPolygons(binary_raster_smoothed, fun = function(x) x == 1, na.rm = TRUE, dissolve = TRUE)
  binary_sf <- sf::st_as_sf(binary_polygon)

  # Save shapefile if requested
  if (output_shapefile) {
    # Eliminar manualmente archivos previos si existen
    files <- list.files(dirname(shapefile_path), pattern = paste0(tools::file_path_sans_ext(basename(shapefile_path)), "\\.(shp|shx|dbf|prj)$"), full.names = TRUE)
    unlink(files)

    sf::st_write(binary_sf, shapefile_path)
    message("Shapefile saved to ", shapefile_path)
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

  # Set up plotting layout (1 row, 2 columns)
  par(mfrow = c(1, 2))

  # First Plot: Smoothed Histogram + Inter-class Variance Curve + Optimal Threshold Line
  plot(hist_values$mids, smoothed_counts, type = "h", col = "grey", lwd = 2,
       xlab = "Intensity", ylab = "Frequency", main = "Smoothed Histogram with Inter-Class Variance")
  lines(hist_values$mids[-1], interclass_variance_curve, col = "blue", lwd = 2)
  abline(v = threshold_value_smoothed, col = "red", lty = 2)
  axis(side = 4, col.axis = "blue")
  mtext("Inter-Class Variance", side = 4, line = 3, col = "blue")
  legend("topright", legend = c("Smoothed Histogram", "Inter-Class Variance",
                                paste("Optimal Threshold (", round(threshold_value_smoothed, 2), ")", sep = "")),
         col = c("grey", "blue", "red"), lty = c(1, 1, 2), lwd = c(2, 2, 1), cex = 0.8)

  # Second Plot: Inter-Class vs Intra-Class Variance Curve (Separate)
  plot(hist_values$mids[-1], interclass_variance_curve, type = "l", col = "blue", lwd = 2,
       xlab = "Threshold", ylab = "Variance", main = "Inter-Class and Intra-Class Variance")
  lines(hist_values$mids[-1], intra_class_variance_curve, col = "green", lwd = 2)
  abline(v = threshold_value_smoothed, col = "red", lty = 2)
  legend("topright", legend = c("Inter-Class Variance", "Intra-Class Variance", "Optimal Threshold"),
         col = c("blue", "green", "red"), lty = c(1, 1, 2), lwd = c(2, 2, 1), cex = 0.8)

  # Reset par to default layout
  par(mfrow = c(1, 1))


  # Return result
  result <- list(
    best_threshold = threshold_value_smoothed,
    area_hectares = total_area_hectares,
    binary_raster_smoothed = binary_raster_smoothed,
    binary_shapefile = if (output_shapefile) binary_sf else NULL,
    shapefile_path = if (output_shapefile) shapefile_path else NULL
  )
  return(result)
}

