test_that("binarize_raster computes threshold and area from synthetic rasters", {
  skip_on_cran()  # evita ejecutar en CRAN automáticamente

  library(raster)

  # Crear dos rasters sintéticos pequeños (pre y post fuego)
  r_pre <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10)
  r_post <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10)

  # Rellenar con valores simulados
  set.seed(42)
  values(r_pre) <- runif(ncell(r_pre), min = 0.4, max = 0.8)
  values(r_post) <- runif(ncell(r_post), min = 0.1, max = 0.6)

  # Llamar a la función
  result <- binarize_raster(r_post, r_pre, output_shapefile = FALSE)

  # Comprobar que los elementos existen y tienen el tipo esperado
  expect_type(result$best_threshold, "double")
  expect_true(result$best_threshold > 0)

  expect_type(result$area_hectares, "double")
  expect_true(result$area_hectares > 0)

  expect_s4_class(result$binary_raster_smoothed, "RasterLayer")
  expect_s3_class(result$binary_shapefile, "sf")
})

