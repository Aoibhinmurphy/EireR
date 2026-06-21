# =============================================================================
# R/get_layer.R
# Generic function to download any dataset from the EireR registry.
# =============================================================================
 
#' Download any dataset from the EireR registry
#'
#' Provide any `dataset_id` from
#' `eire_catalogue()`, downloads and returns the layer as an sf object
#' in ITM (EPSG:2157).
#'
#' This unlocks all datasets in the registry with a single function.
#' Use `eire_catalogue()` to browse available datasets and find their IDs.
#'
#' @param dataset_id A dataset ID from `eire_datasets$dataset_id`.
#' @param quiet Suppress messages. Default `FALSE`.
#' @return An `sf` object in EPSG:2157 (ITM).
#' @export
#' @examples
#' \dontrun{
#' # Browse available datasets
#' eire_catalogue(theme = "hydrology")
#'
#' # Download a specific layer by ID
#' rivers <- get_layer("epa_wfd_riverwaterbodies_cycle3")
#' catchments <- get_layer("epa_water_riverbasins")
#'
#' # Plot it
#' plot_eire(rivers)
#' }
get_layer <- function(dataset_id, quiet = FALSE) {
 
  # Look up the dataset in the registry
  row <- eire_datasets[eire_datasets$dataset_id == dataset_id, ]
 
  if (nrow(row) == 0) {
    cli::cli_abort(c(
      "Dataset {.val {dataset_id}} not found in registry.",
      "i" = "Use {.fn eire_catalogue} to browse available datasets."
    ))
  }
 
  url  <- row$download_url[1]
  name <- row$name[1]
 
  if (is.na(url)) {
    cli::cli_abort(c(
      "{.val {name}} has no download URL.",
      "i" = "Service type: {row$service_type[1]}",
      "i" = "Some datasets require special access - check {.field notes}: {row$notes[1]}"
    ))
  }
 
  if (!quiet) cli::cli_inform("Downloading: {.val {name}}")
 
  # Read the layer - handles both /vsicurl/ and normal URLs
  layer <- if (startsWith(url, "/vsicurl/")) {
    sf::read_sf(url)
  } else {
    sf::read_sf(fetch_cached(url, paste0(dataset_id, ".geojson"), quiet = quiet))
  }
 
  # Reproject to ITM
  to_itm(layer)
}
