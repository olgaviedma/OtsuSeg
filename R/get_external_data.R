#' Download and load example data for OtsuSeg
#'
#' Downloads a ZIP file with example rasters and shapefiles from GitHub releases and optionally loads a specific file.
#'
#' @param filename Optional. The name of the file to return (after extraction).
#' @param path Local directory to download and extract files. Default is a temp folder.
#' @param load Logical. If TRUE, returns a loaded object (RasterLayer for .tif, sf for .shp). Default is FALSE.
#' @return File path or loaded object if `load = TRUE`.
#' @importFrom utils download.file unzip
#' @importFrom raster raster
#' @importFrom sf st_read
#' @export
#' @examples
#' \donttest{
#' # Download example data and list contents
#' data_dir <- get_external_data(path = tempdir())
#' list.files(data_dir)
#'
#' # Load a specific raster file (.tif)
#' pre_fire <- get_external_data("NBRpre.tif", path = tempdir(), load = TRUE)
#' print(pre_fire)
#'
#' # Load a specific shapefile (.shp)
#' shape_data <- get_external_data("shapefile_reference.shp", path = tempdir(), load = TRUE)
#' print(shape_data)
#' }
#'
get_external_data <- function(filename = NULL, path = tempdir(), load = FALSE) {
  url <- "https://github.com/olgaviedma/OtsuSeg/releases/download/v0.1.0/example_data.zip"
  zip_file <- file.path(path, "example_data.zip")

  # Download and unzip the example data if not already done
  if (!file.exists(zip_file)) {
    message("Downloading example data from GitHub...")
    download.file(url, destfile = zip_file, mode = "wb")
    unzip(zip_file, exdir = path)
  }

  # If no filename is specified, return the directory path
  if (is.null(filename)) {
    return(path)
  }

  # Attempt to detect file type if no extension is specified
  file_path <- file.path(path, filename)

  # Try .tif if no extension is specified
  if (!file.exists(file_path) && !grepl("\\.", filename)) {
    possible_tif <- paste0(file_path, ".tif")
    possible_shp <- paste0(file_path, ".shp")

    if (file.exists(possible_tif)) {
      file_path <- possible_tif
    } else if (file.exists(possible_shp)) {
      file_path <- possible_shp
    } else {
      stop("File not found: ", file_path, ", .tif, or .shp")
    }
  }

  # Stop if the specified file does not exist
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Load the file if requested
  if (load) {
    if (grepl("\\.tif$", file_path, ignore.case = TRUE)) {
      return(raster::raster(file_path))
    } else if (grepl("\\.shp$", file_path, ignore.case = TRUE)) {
      return(sf::st_read(file_path, quiet = TRUE))
    } else {
      stop("Only .tif and .shp files are supported for automatic loading.")
    }
  } else {
    return(file_path)
  }
}
