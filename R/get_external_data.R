#' Download and load example data for OtsuSeg
#'
#' Downloads a ZIP file with example rasters from GitHub releases and optionally loads a specific file.
#'
#' @param filename Optional. The name of the file to return (after extraction).
#' @param path Local directory to download and extract files. Default is a temp folder.
#' @param load Logical. If TRUE, returns a loaded raster object (only for .tif). Default is FALSE.
#' @return File path or loaded object if `load = TRUE`.
#' @importFrom utils download.file unzip
#'
#' @export
get_external_data <- function(filename = NULL, path = tempdir(), load = FALSE) {
  url <- "https://github.com/olgaviedma/OtsuSeg/releases/download/v0.1.0/example_data.zip"
  zip_file <- file.path(path, "example_data.zip")

  if (!file.exists(zip_file)) {
    message("Downloading example data from GitHub...")
    download.file(url, destfile = zip_file, mode = "wb")
    unzip(zip_file, exdir = path)
  }

  if (!is.null(filename)) {
    file_path <- file.path(path, filename)
    if (!file.exists(file_path)) stop("File not found: ", file_path)

    if (load) {
      if (grepl("\\.tif$", filename, ignore.case = TRUE)) {
        return(raster::raster(file_path))
      } else {
        stop("Only .tif files are supported for automatic loading.")
      }
    } else {
      return(file_path)
    }
  } else {
    return(path)
  }
}
