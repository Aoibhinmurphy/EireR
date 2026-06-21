# =============================================================================
# R/utils_cache.R
# Local disk caching for EireR — downloads GeoJSON files once, reuses forever.
# =============================================================================
 
#' @importFrom rappdirs user_cache_dir
#' @importFrom cli cli_inform cli_abort
NULL
 
# Returns the cache folder path, creating it if it doesn't exist yet
eirer_cache_dir <- function() {
  d <- rappdirs::user_cache_dir("EireR")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}
 
#' Show or clear the EireR local data cache
#'
#' EireR saves downloaded GeoJSON files locally so repeated calls to
#' `get_*()` functions do not re-download data.
#'
#' @param clear Logical. If `TRUE`, delete all cached files. Default `FALSE`.
#' @return The cache directory path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' eirer_cache()              # see what is cached
#' eirer_cache(clear = TRUE)  # wipe everything
#' }
eirer_cache <- function(clear = FALSE) {
  d <- eirer_cache_dir()
 
  if (clear) {
    files <- list.files(d, full.names = TRUE)
    file.remove(files)
    cli::cli_inform("Cache cleared: {length(files)} file{?s} removed.")
    return(invisible(NULL))
  }
 
  files <- list.files(d)
  if (length(files) == 0) {
    cli::cli_inform("Cache is empty. Run any get_*() function to populate it.")
    cli::cli_inform("Cache location: {.path {d}}")
  } else {
    cli::cli_inform("Cache location: {.path {d}}")
    cli::cli_inform("{length(files)} file{?s} cached:")
    cli::cli_inform(paste("  -", files, collapse = "\n"))
  }
  invisible(d)
}
 
# Internal: download a GeoJSON file if not already cached, return local path.
# Called by every get_*() function — the user never calls this directly.
fetch_cached <- function(url, filename, quiet = FALSE) {
  dest <- file.path(eirer_cache_dir(), filename)
 
  if (file.exists(dest)) {
    if (!quiet) cli::cli_inform("Using cached: {.file {filename}}")
    return(dest)
  }
 
  if (!quiet) cli::cli_inform("Downloading {.file {filename}} ...")
 
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "EireR/0.1.0 R-package (github.com/Aoibhinmurphy/EireR)") |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform(path = dest)
 
  if (httr2::resp_status(resp) >= 400) {
    if (file.exists(dest)) file.remove(dest)
    cli::cli_abort(c(
      "Failed to download {.file {filename}}.",
      "x" = "HTTP status {httr2::resp_status(resp)}",
      "i" = "URL: {.url {url}}"
    ))
  }
 
  if (!quiet) cli::cli_inform("Saved: {.file {filename}}")
  dest
}
 
