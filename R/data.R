# =============================================================================
# R/data.R
# Documentation for eire_datasets + the eire_catalogue() browse function.
# =============================================================================

#' EireR dataset catalogue
#'
#' A table of all 1023 geospatial datasets available through EireR,
#' auto-discovered from EPA GeoServer (WFS), GeoHive (ROI ArcGIS Hub),
#' DAERA (NI ArcGIS Hub), and OpenDataNI (CKAN).
#'
#' @format A data frame with columns: `dataset_id`, `name`, `jurisdiction`,
#'   `provider`, `provider_url`, `download_url`, `licence`, `licence_url`,
#'   `layer_type`, `theme`, `service_type`, `service_url`, `service_layer`,
#'   `output_format`, `crs_original`, `date_acquired`, `last_update`, `notes`.
#' @seealso [eire_catalogue()] to filter and search interactively.
"eire_datasets"

utils::globalVariables(c(
  "eire_datasets", "name", "notes",
  "COUNTY", "CountyName", "county", "geometry",
  "jurisdiction", ".data"
))

#' Browse the EireR dataset catalogue
#'
#' Filter and search the 1023 datasets available in EireR by jurisdiction,
#' theme, layer type, or keyword.
#'
#' @param jurisdiction `"ROI"`, `"NI"`, or `NULL` for all.
#' @param theme One or more of: `"hydrology"`, `"boundary"`, `"population"`,
#'   `"land_cover"`, `"elevation"`, `"transport"`, `"placenames"`,
#'   `"environment"`, `"marine"`, `"geology"`, `"recreation"`, `"other"`.
#' @param layer_type One of `"point"`, `"line"`, `"polygon"`, `"vector"`.
#' @param search Search string matched against `name` and `notes` columns.
#' @param provider Partial match against provider name e.g. `"EPA"`.
#' @return A filtered data frame.
#' @export
#' @examples
#' \dontrun{
#' eire_catalogue()
#' eire_catalogue(theme = "hydrology")
#' eire_catalogue(jurisdiction = "NI", theme = "boundary")
#' eire_catalogue(search = "river")
#' eire_catalogue(provider = "EPA")
#' View(eire_catalogue())
#' }
eire_catalogue <- function(jurisdiction = NULL,
                            theme        = NULL,
                            layer_type   = NULL,
                            search       = NULL,
                            provider     = NULL) {
  out <- eire_datasets

  if (!is.null(jurisdiction)) out <- dplyr::filter(out, jurisdiction %in% !!jurisdiction)
  if (!is.null(theme))        out <- dplyr::filter(out, theme %in% !!theme)
  if (!is.null(layer_type))   out <- dplyr::filter(out, layer_type %in% !!layer_type)

  if (!is.null(provider)) {
    out <- dplyr::filter(out, grepl(tolower(provider), tolower(provider), fixed = TRUE))
  }

  if (!is.null(search)) {
    p <- tolower(search)
    out <- dplyr::filter(
      out,
      grepl(p, tolower(name),  fixed = TRUE) |
      grepl(p, tolower(notes), fixed = TRUE)
    )
  }

  cli::cli_inform("{nrow(out)} dataset{?s} found.")
  out
}
