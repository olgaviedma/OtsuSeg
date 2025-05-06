#' Quantitative Comparison of Binary Shape with Reference Shape
#'
#' This function calculates various metrics (e.g., precision, recall, F1 score)
#' to compare a binary raster (in shapefile format) with a reference vector shape.
#'
#' @param binary_shape A shapefile (sf object) representing the binary raster (e.g., burn scars).
#' @param reference_shape A shapefile (sf object) representing the reference vector shape (e.g., actual burn scars).
#' @param metrics A vector of metric names to calculate. If NULL, all available metrics are computed.
#'        Available metrics are: "Precision", "Recall", "F1_Score", "IoU", "OS", "US", "E",
#'        "SimSize", "Loc", and "AFI". Default is NULL, which computes all metrics.
#'
#' @return A data frame containing the computed metrics and their values.
#' @export
#'
#' @examples
#' \dontrun{
#'   binary_shape <- sf::st_read("binary_shape.shp")
#'   reference_shape <- sf::st_read("reference_shape.shp")
#'   result <- Quality_control(binary_shape, reference_shape)
#'   print(result)
#' }

Quality_control <- function(binary_shape, reference_shape, metrics = NULL) {
  if (!inherits(binary_shape, "sf")) stop("binary_shape must be an sf object.")
  if (!inherits(reference_shape, "sf")) stop("reference_shape must be an sf object.")

  if (sf::st_crs(binary_shape) != sf::st_crs(reference_shape)) {
    stop("binary_shape and reference_shape must have the same CRS.")
  }

  all_metrics <- c("Precision", "Recall", "F1_Score", "IoU", "OS", "US", "E", "SimSize", "Loc", "AFI")

  if (is.null(metrics) || length(metrics) == 0) {
    metrics <- all_metrics
  } else {
    invalid_metrics <- setdiff(metrics, all_metrics)
    if (length(invalid_metrics) > 0) {
      stop("Invalid metrics: ", paste(invalid_metrics, collapse = ", "), ".")
    }
  }

  binary_shape <- sf::st_make_valid(binary_shape)
  reference_shape <- sf::st_make_valid(reference_shape)

  intersection <- sf::st_intersection(binary_shape, reference_shape)
  intersection_area <- ifelse(nrow(intersection) == 0, 0, sum(as.numeric(sf::st_area(intersection))))

  total_binary_area <- sum(as.numeric(sf::st_area(binary_shape)))
  total_reference_area <- sum(as.numeric(sf::st_area(reference_shape)))
  union_area <- sum(as.numeric(sf::st_area(sf::st_make_valid(sf::st_union(binary_shape, reference_shape)))))

  results <- list()

  if ("Precision" %in% metrics) {
    precision <- ifelse(total_binary_area == 0, 0, intersection_area / total_binary_area)
    results[["Precision"]] <- precision
  }

  if ("Recall" %in% metrics) {
    recall <- ifelse(total_reference_area == 0, 0, intersection_area / total_reference_area)
    results[["Recall"]] <- recall
  }

  if ("F1_Score" %in% metrics) {
    precision <- ifelse(total_binary_area == 0, 0, intersection_area / total_binary_area)
    recall <- ifelse(total_reference_area == 0, 0, intersection_area / total_reference_area)
    f1_score <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
    results[["F1_Score"]] <- f1_score
  }

  if ("IoU" %in% metrics) {
    iou <- ifelse(union_area == 0, 0, intersection_area / union_area)
    results[["IoU"]] <- iou
  }

  if ("OS" %in% metrics) {
    os <- ifelse(total_reference_area == 0, NA, 1 - (intersection_area / total_reference_area))
    results[["OS"]] <- os
  }

  if ("US" %in% metrics) {
    us <- ifelse(total_binary_area == 0, NA, 1 - (intersection_area / total_binary_area))
    results[["US"]] <- us
  }

  if ("E" %in% metrics) {
    error <- ifelse((total_binary_area + total_reference_area) == 0, NA,
                    (total_binary_area + total_reference_area - 2 * intersection_area) /
                      (total_binary_area + total_reference_area))
    results[["E"]] <- error
  }

  if ("SimSize" %in% metrics) {
    size_similarity <- ifelse(max(total_binary_area, total_reference_area) == 0, 0,
                              1 - abs(total_binary_area - total_reference_area) / max(total_binary_area, total_reference_area))
    results[["SimSize"]] <- size_similarity
  }

  if ("Loc" %in% metrics) {
    centroid_binary <- sf::st_centroid(sf::st_union(binary_shape))
    centroid_reference <- sf::st_centroid(sf::st_union(reference_shape))
    location_quality <- ifelse(sf::st_is_empty(centroid_binary) | sf::st_is_empty(centroid_reference), NA,
                               as.numeric(sf::st_distance(centroid_binary, centroid_reference)))
    results[["Loc"]] <- location_quality
  }

  if ("AFI" %in% metrics) {
    afi <- ifelse((total_binary_area + total_reference_area - intersection_area) == 0, 0,
                  intersection_area / (total_binary_area + total_reference_area - intersection_area))
    results[["AFI"]] <- afi
  }

  return(data.frame(Metric = names(results), Value = unlist(results), row.names = NULL))
}
