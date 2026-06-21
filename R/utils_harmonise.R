# =============================================================================
# R/utils_harmonise.R
# CRS standardisation and data harmonisation helpers.
# Called by all get_*() functions — never called directly by users.
# =============================================================================

#' @importFrom sf st_crs st_transform st_area
#' @importFrom dplyr bind_rows mutate
NULL

#' The coordinate reference system used throughout EireR
#'
#' All `get_*()` functions return data in ETRS89 / Irish Transverse Mercator
#' EPSG:2157. Distances and areas in this CRS are in metres.
#'
#' @return Integer `2157L`
#' @export
eirer_crs <- function() 2157L

# Reproject any sf object to ITM (EPSG:2157)
to_itm <- function(x) {
  if (!identical(as.integer(sf::st_crs(x)$epsg), eirer_crs())) {
    x <- sf::st_transform(x, eirer_crs())
  }
  x
}

# Stack ROI and NI sf objects into one, adding a jurisdiction column
bind_jurisdictions <- function(roi, ni,
                                roi_label = "ROI",
                                ni_label  = "NI") {
  roi$jurisdiction <- roi_label
  ni$jurisdiction  <- ni_label
  roi <- to_itm(roi)
  ni  <- to_itm(ni)
  dplyr::bind_rows(roi, ni)
}

# Standardise county names — handles Derry/Londonderry, "Co. Cork" etc
standardise_county_names <- function(x, col = "county") {
  if (!col %in% names(x)) return(x)

  cleaned <- tolower(trimws(x[[col]]))
  cleaned <- gsub("^(co\\. |co |county )", "", cleaned)

  name_map <- c(
    "londonderry" = "Derry", "l'derry" = "Derry", "derry" = "Derry",
    "antrim" = "Antrim", "armagh" = "Armagh", "down" = "Down",
    "fermanagh" = "Fermanagh", "tyrone" = "Tyrone", "dublin" = "Dublin",
    "cork" = "Cork", "galway" = "Galway", "limerick" = "Limerick",
    "waterford" = "Waterford", "tipperary" = "Tipperary",
    "kilkenny" = "Kilkenny", "wexford" = "Wexford", "wicklow" = "Wicklow",
    "kildare" = "Kildare", "meath" = "Meath", "louth" = "Louth",
    "monaghan" = "Monaghan", "cavan" = "Cavan", "donegal" = "Donegal",
    "leitrim" = "Leitrim", "sligo" = "Sligo", "mayo" = "Mayo",
    "roscommon" = "Roscommon", "longford" = "Longford",
    "westmeath" = "Westmeath", "offaly" = "Offaly", "laois" = "Laois",
    "carlow" = "Carlow", "clare" = "Clare", "kerry" = "Kerry"
  )

  x[[col]] <- ifelse(
    cleaned %in% names(name_map),
    name_map[cleaned],
    tools::toTitleCase(cleaned)
  )
  x
}
