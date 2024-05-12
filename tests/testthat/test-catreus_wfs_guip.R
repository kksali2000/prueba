test_that("BBOX Check errors", {
  expect_error(catreus_gipu_wfs_get_address_bbox(x = "1234"))
  expect_error(catreus_gipu_wfs_get_address_bbox(c("1234", "a", "3", "4")))
  expect_error(catreus_gipu_wfs_get_address_bbox(c(1, 2, 3)))
  expect_error(catreus_gipu_wfs_get_address_bbox(c(1, 2, 3, 4)))
})


test_that("BBOX Check projections", {
  skip_on_cran()

 # expect_null(catreus_gipu_wfs_get_address_bbox(c(760926, 4019259, 761155, 4019366),
 #    srs = 25829
 #  ))

  expect_message(catreus_gipu_wfs_get_address_bbox(
    c(539089,4763548,550139,4795812),
    srs = 25830,
    verbose = TRUE,
    count = 5
  ))

  obj <- catreus_gipu_wfs_get_address_bbox(c(539089,4763548,550139,4795812),
    srs = 25830,
    count = 10
  )

  expect_true(sf::st_crs(obj) == sf::st_crs(25830))
  expect_true(nrow(obj) == 10)


  # test conversion
  testconv <- get_sf_from_bbox(obj[1, ])
  expect_identical(obj[1, ], testconv)

  # Convert to spatial object

  bbox <- get_sf_from_bbox(
    c(539089,4763548,550139,4795812),
    srs=25830
  )
  expect_s3_class(bbox, "sfc")

  obj2 <- catreus_gipu_wfs_get_address_bbox(bbox,count=10)
  expect_true(sf::st_crs(obj2) == sf::st_crs(25830))

  # Transform object to geographic coords
  bbox2 <- sf::st_transform(obj2[1, ], 4326)
  expect_true(sf::st_is_longlat(bbox2))
  expect_s3_class(bbox2, "sf")

  #obj3 <- catreus_gipu_wfs_get_address_bbox(bbox2,count=10)

  #expect_true(sf::st_is_longlat(obj3))
  #expect_true(sf::st_crs(obj3) == sf::st_crs(4326))

  # BBox with coordinates

  vec <- as.double(sf::st_bbox(obj2[1, ]))

  obj4 <- catreus_gipu_wfs_get_address_bbox(vec, verbose = TRUE, srs = 4326,count=10)

  expect_error(sf::st_is_longlat(obj4))
  expect_error(sf::st_crs(obj4) == sf::st_crs(25830))
})
