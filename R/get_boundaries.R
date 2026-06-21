# =============================================================================
# R/get_boundaries.R
# User-facing functions for administrative boundary data.
#
# HOW URL LOOKUP WORKS:
#   URLs are looked up from eire_datasets (built by data-raw/build_registry.R).
#   This means if a provider changes a URL, only the registry needs updating.
#
# HOW DOWNLOADING WORKS:
#   Two approaches depending on the data source:
#   - ArcGIS REST / WFS URLs → fetch_cached() downloads and caches locally
#   - /vsicurl/ URLs (OpenDataNI CKAN) → sf::read_sf() streams via GDAL
#
# WHY TWO APPROACHES?
#   OpenDataNI blocks httr2's default HTTP requests with a 403 Forbidden error.
#   Prepending /vsicurl/ routes the request through GDAL's own HTTP client,
#   which OpenDataNI accepts. This is stored in the registry URL itself so
#   get_boundaries.R handles it with a simple startsWith() check.
# =============================================================================
 
#' @importFrom sf read_sf st_area
#' @importFrom dplyr select mutate filter any_of
#' @importFrom stringr str_detect
NULL
 
 
#' Get all 32 counties of Ireland
#'
#' Downloads and harmonises county boundary data for the Republic of Ireland
#' (26 counties) and Northern Ireland (6 counties) into a single sf object
#' reprojected to ITM (EPSG:2157).
#'
#' @param region Filter the output. Accepts:
#'   - `NULL` — whole island (default)
#'   - `"ROI"` or `"NI"` — one jurisdiction only
#'   - County name(s) e.g. `"Donegal"` or `c("Donegal","Fermanagh")`
#'   - Numeric bbox `c(xmin, ymin, xmax, ymax)` in ITM metres
#'   - An `sf` object to clip to
#'   - A `bbox` from `sf::st_bbox()`
#' @param quiet Suppress messages. Default `FALSE`.
#' @return An `sf` object with columns: `county`, `jurisdiction`, `area_km2`.
#' @export
#' @examples
#' \dontrun{
#' # All 32 counties
#' counties <- get_counties()
#' plot(counties["jurisdiction"])
#'
#' # One jurisdiction
#' ni <- get_counties(region = "NI")
#'
#' # Border counties from both sides
#' border <- get_counties(region = c("Donegal","Fermanagh","Tyrone",
#'                                    "Armagh","Down","Louth","Monaghan"))
#' }
get_counties <- function(region = NULL, quiet = FALSE) {
 
  # --- Look up URLs from registry ---
  # ROI: OSi 2019 ungeneralised counties from GeoHive (ArcGIS REST)
  roi_url <- eire_datasets[
  str_detect(tolower(eire_datasets$name), "counties - national statutory boundaries - 2019") &
  eire_datasets$jurisdiction == "ROI",
  "download_url"
][[1]][1]
 
  # NI: OSNI 50K county boundaries from OpenDataNI (CKAN, /vsicurl/)
ni_url <- eire_datasets[
  str_detect(tolower(eire_datasets$name), "ni counties") &
  eire_datasets$jurisdiction == "NI",
  "download_url"
][[1]][1]
 
  # --- Read spatial data ---
if (!quiet) cli::cli_inform("Reading ROI counties...")
roi_raw <- sf::read_sf(fetch_cached(roi_url, "roi_counties.geojson", quiet = quiet))

if (!quiet) cli::cli_inform("Reading NI counties...")
ni_raw  <- sf::read_sf(ni_url)  # /vsicurl/ URL — GDAL streams directly
 
  # --- Standardise column names ---
  # Each provider uses different column names for the county name field.
  # any_of() tries each name in order and uses the first one it finds.
roi <- roi_raw |>
  dplyr::select(county = COUNTY) |>
  dplyr::mutate(county = tools::toTitleCase(tolower(county)))

ni <- ni_raw |>
  dplyr::select(county = CountyName) |>
  dplyr::mutate(county = tools::toTitleCase(tolower(county)))
 
  # --- Harmonise ---
  # bind_jurisdictions() reprojects both to ITM and adds jurisdiction column
  # standardise_county_names() fixes Derry/Londonderry and other variants
  combined <- bind_jurisdictions(roi, ni) |>
    standardise_county_names(col = "county") |>
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6)
 
  # --- Filter by region if specified ---
  apply_region(combined, region = region, county_col = "county")
}
 
 
#' Get sub-county administrative boundaries for Ireland
#'
#' Returns Local Electoral Areas (ROI) and Local Government Districts (NI)
#' harmonised into a single sf object in ITM (EPSG:2157).
#'
#' @param region See [get_counties()] for full region options.
#' @param quiet Suppress messages. Default `FALSE`.
#' @return An `sf` object with columns: `district_name`, `jurisdiction`,
#'   `area_km2`.
#' @export
#' @examples
#' \dontrun{
#' districts <- get_districts()
#' ni_districts <- get_districts(region = "NI")
#' }
get_districts <- function(region = NULL, quiet = FALSE) {
 
  # ROI: Local Electoral Areas from GeoHive
  roi_url <- eire_datasets[
    str_detect(tolower(eire_datasets$name), "local electoral|electoral area") &
    eire_datasets$jurisdiction == "ROI",
    "download_url"
  ][[1]][1]
 
  # NI: Local Government Districts from OpenDataNI (CKAN, /vsicurl/)
  ni_url <- eire_datasets[
    str_detect(tolower(eire_datasets$name), "local government district") &
    eire_datasets$jurisdiction == "NI",
    "download_url"
  ][[1]][1]
 
if (!quiet) cli::cli_inform("Reading ROI districts...")
roi_raw <- sf::read_sf(fetch_cached(roi_url, "roi_districts.geojson", quiet = quiet))

if (!quiet) cli::cli_inform("Reading NI districts...")
ni_raw <- if (startsWith(ni_url, "/vsicurl/")) {
  sf::read_sf(ni_url)
} else {
  sf::read_sf(fetch_cached(ni_url, "ni_lgd.geojson", quiet = quiet))
}
 
  roi <- roi_raw |>
    dplyr::select(district_name = dplyr::any_of(
      c("ENGLISH", "English", "NAME", "name", "LEA_NAME")
    ))
 
  ni <- ni_raw |>
    dplyr::select(district_name = dplyr::any_of(
      c("LGDNAME", "LGDName", "NAME", "name")
    ))
 
  combined <- bind_jurisdictions(roi, ni) |>
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6)
 
  apply_region(combined, region = region)
}
