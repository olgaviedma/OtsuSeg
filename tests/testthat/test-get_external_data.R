test_that("get_external_data errors on unsupported file type for load", {
  skip_on_cran()
  skip_if_offline()

  # Creamos un archivo temporal simulado en la carpeta de extracción
  path <- get_external_data()
  txt_file <- file.path(path, "example.txt")
  writeLines("dummy", txt_file)

  expect_error(
    get_external_data(filename = "example.txt", load = TRUE),
    "Only .tif and .shp files are supported for automatic loading."
  )

  # Limpieza (opcional)
  unlink(txt_file)
})
