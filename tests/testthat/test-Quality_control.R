test_that("Quality_control computes all metrics correctly for simple polygons", {
  library(sf)

  # Crear dos polígonos parcialmente superpuestos
  poly1 <- st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))))
  poly2 <- st_polygon(list(rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))))

  # Convertir a objetos sf
  sf1 <- st_sf(geometry = st_sfc(poly1), crs = 4326)
  sf2 <- st_sf(geometry = st_sfc(poly2), crs = 4326)

  # Ejecutar función
  result <- Quality_control(sf1, sf2)

  # Verificaciones básicas
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Metric", "Value") %in% names(result)))

  # Verificar métricas conocidas
  expect_true("Precision" %in% result$Metric)
  expect_true("Recall" %in% result$Metric)
  expect_true("F1_Score" %in% result$Metric)
  expect_true("IoU" %in% result$Metric)
  expect_true(all(!is.na(result$Value)))
})

test_that("Quality_control errors on CRS mismatch", {
  library(sf)

  p1 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 4326))
  p2 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 3857))

  expect_error(Quality_control(p1, p2), "must have the same CRS")
})

test_that("Quality_control handles invalid metric input", {
  library(sf)

  p1 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 4326))
  p2 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 4326))

  expect_error(Quality_control(p1, p2, metrics = c("XYZ")), "Invalid metrics")
})
