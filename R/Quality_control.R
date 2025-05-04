#' Quantitative Comparison of Binary Raster with Reference Shape
#'
#' This function calculates various metrics (e.g., precision, recall, F1 score)
#' to compare a binary raster (in shapefile format) with a reference vector shape.
#'
#' @param binary_shape A shapefile representing the binary raster (e.g., burn scars).
#' @param reference_shape A shapefile representing the reference vector shape (e.g., actual burn scars).
#' @param metrics A vector of metric names to calculate. If NULL, all available metrics are computed.
#'        Available metrics are: "Precision", "Recall", "F1_Score", "IoU", "OS", "US", "E",
#'        "SimSize", "Loc", and "AFI". Default is NULL, which computes all metrics.
#' @return A data frame containing the computed metrics and their values.
#' @import sf
#' @import raster
#' @export
#' @examples
#' \dontrun{
#'   binary_shape <- st_read("binary_shape.shp")  # Your binary raster shapefile
#'   reference_shape <- st_read("reference_shape.shp")  # Your reference shapefile
#'   result <- Quality_control(binary_shape, reference_shape)
#'   print(result)
#' }
Quality_control <- function(binary_shape, reference_shape, metrics = NULL) {
  library(sf)

  # Ensure input data are in sf format
  if (!inherits(binary_shape, "sf")) stop("binary_shape must be an sf object.")
  if (!inherits(reference_shape, "sf")) stop("reference_shape must be an sf object.")

  # Ensure both layers have the same CRS
  if (st_crs(binary_shape) != st_crs(reference_shape)) stop("binary_shape and reference_shape must have the same CRS.")

  # Define available metrics
  all_metrics <- c("Precision", "Recall", "F1_Score", "IoU", "OS", "US", "E", "SimSize", "Loc", "AFI")

  # Handle case where metrics is NULL or empty
  if (is.null(metrics) || length(metrics) == 0) {
    metrics <- all_metrics  # Default to calculating all metrics
  } else {
    # Check if any provided metric is invalid
    invalid_metrics <- setdiff(metrics, all_metrics)
    if (length(invalid_metrics) > 0) {
      stop("Invalid metrics: ", paste(invalid_metrics, collapse = ", "), ". Please provide valid metric names.")
    }
  }

  # Compute intersection
  intersection <- st_intersection(binary_shape, reference_shape)

  # Ensure intersection is valid
  intersection_area <- ifelse(nrow(intersection) == 0, 0, sum(as.numeric(st_area(intersection))))

  # Compute total areas safely
  total_binary_area <- sum(as.numeric(st_area(st_make_valid(binary_shape))))
  total_reference_area <- sum(as.numeric(st_area(st_make_valid(reference_shape))))
  union_shape <- st_union(binary_shape, reference_shape)
  union_area <- sum(as.numeric(st_area(st_make_valid(union_shape))))

  results <- data.frame(Metric = character(0), Value = numeric(0), stringsAsFactors = FALSE)

  # Compute selected metrics
  if ("Precision" %in% metrics) {
    precision <- ifelse(total_binary_area == 0, 0, intersection_area / total_binary_area)
    results <- rbind(results, data.frame(Metric = "Precision", Value = precision))
  }

  if ("Recall" %in% metrics) {
    recall <- ifelse(total_reference_area == 0, 0, intersection_area / total_reference_area)
    results <- rbind(results, data.frame(Metric = "Recall", Value = recall))
  }

  if ("F1_Score" %in% metrics) {
    if (precision + recall == 0) {
      f1_score <- 0
    } else {
      f1_score <- 2 * (precision * recall) / (precision + recall)
    }
    results <- rbind(results, data.frame(Metric = "F1_Score", Value = f1_score))
  }

  if ("IoU" %in% metrics) {
    iou <- ifelse(union_area == 0, 0, intersection_area / union_area)
    results <- rbind(results, data.frame(Metric = "IoU", Value = iou))
  }

  if ("OS" %in% metrics) {
    os <- ifelse(total_reference_area == 0, NA, 1 - (intersection_area / total_reference_area))
    results <- rbind(results, data.frame(Metric = "OS", Value = os))
  }

  if ("US" %in% metrics) {
    us <- ifelse(total_binary_area == 0, NA, 1 - (intersection_area / total_binary_area))
    results <- rbind(results, data.frame(Metric = "US", Value = us))
  }

  if ("E" %in% metrics) {
    error <- ifelse((total_binary_area + total_reference_area) == 0, NA,
                    (total_binary_area + total_reference_area - 2 * intersection_area) /
                      (total_binary_area + total_reference_area))
    results <- rbind(results, data.frame(Metric = "E", Value = error))
  }

  if ("SimSize" %in% metrics) {
    size_similarity <- ifelse(max(total_binary_area, total_reference_area) == 0, 0,
                              1 - abs(total_binary_area - total_reference_area) / max(total_binary_area, total_reference_area))
    results <- rbind(results, data.frame(Metric = "SimSize", Value = size_similarity))
  }

  if ("Loc" %in% metrics) {
    centroid_binary <- st_centroid(st_union(binary_shape))
    centroid_reference <- st_centroid(st_union(reference_shape))
    location_quality <- ifelse(st_is_empty(centroid_binary) | st_is_empty(centroid_reference), NA,
                               as.numeric(st_distance(centroid_binary, centroid_reference)))
    results <- rbind(results, data.frame(Metric = "Loc", Value = location_quality))
  }

  if ("AFI" %in% metrics) {
    afi <- ifelse((total_binary_area + total_reference_area - intersection_area) == 0, 0,
                  intersection_area / (total_binary_area + total_reference_area - intersection_area))
    results <- rbind(results, data.frame(Metric = "AFI", Value = afi))
  }

  return(results)
}





