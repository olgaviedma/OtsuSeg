#' Access external test data for the otsuSeg package
#'
#' This function helps users easily access the test data provided with the otsuSeg package.
#'
#' @param data_type Character string specifying the type of data to retrieve.
#'                  One of "NBRpre", "NBRpost", or "shapefile_reference".
#' @return Returns the file path of the requested data.
#' @importFrom raster raster
#' @importFrom sf st_read
#' @export
#'
#' @examples
#' # Get file path
#' get_external_data("NBRpre")
#'
#' # Load raster manually
#' raster_data <- raster::raster(get_external_data("NBRpre"))
#' plot(raster_data)
#'
#' # Load shapefile manually
#' shape_data <- sf::st_read(get_external_data("shapefile_reference"))
#' plot(shape_data)
get_external_data <- function(data_type) {
  # Define valid data types
  valid_types <- c("NBRpre", "NBRpost", "shapefile_reference")

  # Check if data_type is valid
  if (!(data_type %in% valid_types)) {
    stop("Invalid data type. Choose from 'NBRpre', 'NBRpost', or 'shapefile_reference'.")
  }

  # Construct the file path
  file_ext <- ifelse(data_type == "shapefile_reference", ".shp", ".asc")  # Use .asc for raster data
  file_path <- system.file("extdata", paste0(data_type, file_ext), package = "otsuSeg")

  # Check if the file exists
  if (file_path == "") {
    stop("File not found. Ensure the 'otsuSeg' package contains the requested data.")
  }

  return(file_path)
}
