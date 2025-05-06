test_that("get_external_data returns path when no filename is specified", {
  skip_on_cran()
  skip_if_offline()

  path <- get_external_data()
  expect_true(dir.exists(path))
})

test_that("get_external_data returns correct file path if filename is specified", {
  skip_on_cran()
  skip_if_offline()

  file <- "NBRpre.tif"
  file_path <- get_external_data(filename = file, load = FALSE)

  expect_true(file.exists(file_path))
  expect_match(basename(file_path), file)
})

test_that("get_external_data loads raster if load = TRUE", {
  skip_on_cran()
  skip_if_offline()

  file <- "NBRpre.tif"
  rast <- get_external_data(filename = file, load = TRUE)

  expect_s4_class(rast, "RasterLayer")
})

test_that("get_external_data errors on unsupported file type for load", {
  skip_on_cran()
  skip_if_offline()

  # Creamos un archivo temporal simulado en la carpeta de extracción
  path <- get_external_data()
  txt_file <- file.path(path, "example.txt")
  writeLines("dummy", txt_file)

  expect_error(
    get_external_data(filename = "example.txt", load = TRUE),
    "Only .tif files are supported"
  )

  # Limpieza (opcional)
  unlink(txt_file)
})
