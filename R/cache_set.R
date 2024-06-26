#' Set your \pkg{CatastRoEus} cache dir
#'
#' @family cache utilities
#' @seealso [rappdirs::user_config_dir()]
#'
#' @return An (invisible) character with the path to your `cache_dir`.
#' @description
#' This function will store your `cache_dir` path on your local machine and would load it for future sessions. Type `Sys.getenv("CATASTROEUS_CACHE_DIR")`
#' to find your cached path.
#'
#' Alternatively, you can store the `cache_dir` manually with the following options:
#'   * Run `Sys.setenv(CATASTROEUS_CACHE_DIR = "cache_dir")`. You would need to run this command on each session (Similar to `install = FALSE`).
#'   * Write this line on your `.Renviron` file:
#'     `CATASTROEUS_CACHE_DIR = "value_for_cache_dir"` (same behavior than `install = TRUE`). This would store your `cache_dir` permanently.
#'
#' @param cache_dir A path to a cache directory. On missing value the function would store the cached files on a temporary dir (See [base::tempdir()]).
#' @param overwrite If this is set to `TRUE`, it will overwrite an existing `CATASTROEUS_CACHE_DIR` that you already have in local machine.
#' @param install if `TRUE`, will install the key in your local machine for use in future sessions.  Defaults to `FALSE.` If `cache_dir` is `FALSE`
#' this parameter is set to `FALSE` automatically.
#' @param verbose Logical, displays information. Useful for debugging, default is `FALSE`.
#'
#' @details
#' # About caching
#'
#' Sometimes cached files may be corrupt. On that case, try re-downloading the data setting `update_cache = TRUE`.
#'
#' If you experience any problem on download, try to download the corresponding file by any other method and save it on your
#' `cache_dir`. Use the option `verbose = TRUE` for debugging the API query.
#'
#' @examples
#'
#' # Don't run this! It would modify your current state
#' \dontrun{
#' catreus_set_cache_dir(verbose = TRUE)
#' }
#'
#' Sys.getenv("CATASTROEUS_CACHE_DIR")
#' @export


catreus_set_cache_dir <- function(cache_dir,
                                  overwrite = FALSE,
                                  install = FALSE,
                                  verbose = TRUE) {
  # Default if not provided
  if (missing(cache_dir) || cache_dir == "") {
    if (verbose) {
      message(
        "Using a temporary cache dir. ",
        "Set 'cache_dir' to a value for store permanently"
      )
    }
    # Create a folder on tempdir
    cache_dir <- file.path(tempdir(), "CatastRoEus")
    is_temp <- TRUE
    install <- FALSE
  } else {
    is_temp <- FALSE
  }


  # Validate
  stopifnot(
    is.character(cache_dir),
    is.logical(overwrite),
    is.logical(install)
  )

  # Expand
  cache_dir <- path.expand(cache_dir)

  # Create cache dir if it doesn't exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  if (verbose) {
    message(
      "CatastRoEus cache dir is: ",
      cache_dir
    )
  }


  # Install path on environ var.

  # nocov start

  if (install) {
    config_dir <- rappdirs::user_config_dir("CatastRoEus", "R")
    # Create cache dir if not presente
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE)
    }

    catastroeus_file <- file.path(config_dir, "CATASTROEUS_CACHE_DIR")

    if (!file.exists(catastroeus_file) || overwrite == TRUE) {
      # Create file if it doesn't exist
      writeLines(cache_dir, con = catastroeus_file)
    } else {
      stop(
        "A cache_dir path already exists.\nYou can overwrite it with the ",
        "argument overwrite = TRUE",
        call. = FALSE
      )
    }
    # nocov end
  } else {
    if (verbose && !is_temp) {
      message(
        "To install your cache_dir path for use in future sessions,",
        "\nrun this function with `install = TRUE`."
      )
    }
  }

  Sys.setenv(CATASTROEUS_CACHE_DIR = cache_dir)
  return(invisible(cache_dir))
}


#' Detect cache dir for CatastRoEus
#'
#' @noRd


catreus_hlp_detect_cache_dir <- function() {
  # Try from getenv
  getvar <- Sys.getenv("CATASTROEUS_CACHE_DIR")

  if (is.null(getvar) || is.na(getvar) || getvar == "") {
    # Not set - tries to retrieve from cache
    cache_config <- file.path(
      rappdirs::user_config_dir("CatastRoEus", "R"),
      "catastroeus_cache_dir"
    )

    # nocov start
    if (file.exists(cache_config)) {
      cached_path <- readLines(cache_config)

      # Case on empty cached path - would default
      if (any(
        is.null(cached_path),
        is.na(cached_path),
        cached_path == ""
      )) {
        cache_dir <- catreus_set_cache_dir(
          overwrite = TRUE,
          verbose = FALSE
        )
        return(cache_dir)
      }

      # 3. Return from cached path
      Sys.setenv(CATASTROEUS_CACHE_DIR = cached_path)
      return(cached_path)
      # nocov end
    } else {
      # 4. Default cache location

      cache_dir <- catreus_set_cache_dir(
        overwrite = TRUE,
        verbose = FALSE
      )
      return(cache_dir)
    }
  } else {
    return(getvar)
  }
}


#' Creates `cache_dir`
#'
#' @inheritParams catreus_set_cache_dir
#'
#' @noRd



catreus_hlp_cachedir <- function(cache_dir = NULL) {
  # Check cache dir from options if not set
  if (is.null(cache_dir)) {
    cache_dir <- catreus_hlp_detect_cache_dir()
  }

  # Create cache dir if needed
  if (isFALSE(dir.exists(cache_dir))) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}
