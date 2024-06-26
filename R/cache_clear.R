#' Clear your \pkg{CatastRoEus} cache dir
#'
#' @family cache utilities
#'
#' @return Invisible. This function is called for its side effects.
#'
#' @description
#' **Use this function with caution**. This function would clear your cached data and configuration, specifically:
#'
#' * Deletes the \pkg{CatastRoEus} config directory
#'   (`rappdirs::user_config_dir("CatastRoEus", "R")`).
#' * Deletes the `cache_dir` directory.
#' * Deletes the values on stored on `Sys.getenv("CATASTROEUS_CACHE_DIR")`.
#'
#' @param config if `TRUE`, will delete the configuration folder of \pkg{CatastRoEus}.
#' @param cached_data If this is set to `TRUE`, it will delete your `cache_dir` and all its content.
#' @inheritParams catreus_set_cache_dir
#'
#' @details
#' This is an overkill function that is intended to reset your status as it you would never have installed and/or used \pkg{CatastRoEus}.
#'
#' @examples
#'
#' # Don't run this! It would modify your current state
#' \dontrun{
#' catreus_clear_cache(verbose = TRUE)
#' }
#'
#' Sys.getenv("CATASTROEUS_CACHE_DIR")
#' @export


catreus_clear_cache <- function(config = FALSE,
                                cached_data = TRUE,
                                verbose = FALSE) {
  config_dir <- rappdirs::user_config_dir("CatastRoEus", "R")
  data_dir <- catreus_hlp_detect_cache_dir()

  # nocov start
  if (config && dir.exists(config_dir)) {
    unlink(config_dir, recursive = TRUE, force = TRUE)
    if (verbose) message("CatastRoEus cache config deleted")
  }
  # nocov end

  if (cached_data && dir.exists(data_dir)) {
    unlink(data_dir, recursive = TRUE, force = TRUE)
    if (verbose) message("CatastRoEus cached data deleted: ", data_dir)
  }


  Sys.setenv(CATASTROEUS_CACHE_DIR = "")

  # Reset cache dir
  return(invisible())
}
