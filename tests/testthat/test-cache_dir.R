test_that("Test cache online", {
  # Get current cache dir
  current <- catreus_hlp_detect_cache_dir()

  cat("User cache dir is ", current, "\n")

  # Set a temp cache dir
  expect_message(
    catreus_set_cache_dir(verbose = TRUE)
  )
  testdir <- expect_silent(catreus_set_cache_dir(
    file.path(current, "testthat"),
    verbose = FALSE
  ))

  # Clean
  expect_silent(
    catreus_clear_cache(config = FALSE, verbose = FALSE)
  )
  # Cache dir should be deleted now
  expect_false(dir.exists(testdir))


  # Reset just for testing all cases
  testdir <- file.path(tempdir(), "CatastRoEUS", "testthat")
  expect_message(catreus_set_cache_dir(testdir))

  cat("Testing cache dir is ", Sys.getenv("CATASTROEUS_CACHE_DIR"), "\n")


  skip_on_cran()
  skip_if_offline()

  #expect_message(catreus_atom_get_parcels_db_all(verbose = TRUE))

  expect_true(dir.exists(testdir))

  expect_message(catreus_clear_cache(config = FALSE, verbose = TRUE))

  # Cache dir should be deleted now
  expect_false(dir.exists(testdir))

  # Restore cache
  expect_message(catreus_set_cache_dir(current, verbose = TRUE))
  expect_silent(catreus_set_cache_dir(current, verbose = FALSE))
  expect_equal(current, Sys.getenv("CATASTROEUS_CACHE_DIR"))
  expect_true(dir.exists(current))
})
