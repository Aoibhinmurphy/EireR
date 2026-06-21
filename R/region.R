# =============================================================================
# R/region.R
#
# PURPOSE:
#   Implements the region= argument system used by every get_*() function.
#
# THE PROBLEM IT SOLVES:
#   A researcher studying the Erne catchment doesn't want all of Ireland —
#   just counties Fermanagh, Cavan, and Donegal. Without region=, they'd
#   have to download everything and clip it themselves every single time.
#
# WHAT DOES region= ACCEPT?
#   NULL or "all"          -> whole island, no filter (default behaviour)
#   "ROI" or "NI"          -> one jurisdiction only
#   "Donegal"              -> single county (fuzzy matched)
#   c("Donegal","Cavan")   -> multiple counties
#   c(xmin,ymin,xmax,ymax) -> bounding box as numbers (ITM coordinates)
#   sf object              -> clip to that geometry
#   bbox object            -> from sf::st_bbox(), clip to that box
#
# FUNCTIONS IN THIS FILE:
#   apply_region()   [INTERNAL] -> the main filter function
#   ireland_bbox()   [EXPORTED] -> convenience helper for bounding boxes
# =============================================================================

#' @importFrom sf st_filter st_transform st_bbox st_as_sfc st_union st_crs
#' @importFrom dplyr filter
#' @importFrom cli cli_warn cli_abort cli_inform
NULL


# -----------------------------------------------------------------------------
# apply_region()   [INTERNAL]
# -----------------------------------------------------------------------------
# Called at the END of every get_*() function.
# Takes the full-island sf object and the user's region= value.
# Returns a filtered or clipped version.

apply_region <- function(x, region = NULL, county_col = "county") {

  # ------------------------------------------------------------------
  # Case 1: No filter — return everything
  # NULL means the user didn't specify a region.
  # "all" is an explicit way to say the same thing.
  # ------------------------------------------------------------------
  if (is.null(region) || identical(tolower(region), "all")) {
    return(x)
  }

  # ------------------------------------------------------------------
  # Case 2: Jurisdiction filter — "ROI" or "NI"
  # ------------------------------------------------------------------
  # length(region) == 1 checks it's a single value, not a vector
  # region %in% c("ROI","NI") checks it's one of those two strings
  if (length(region) == 1 && region %in% c("ROI", "NI")) {

    # Check the layer actually has a jurisdiction column
    # (all our get_*() functions add one, but custom layers might not)
    if (!"jurisdiction" %in% names(x)) {
      cli::cli_warn(c(
        "Cannot filter by jurisdiction: no {.field jurisdiction} column found.",
        "i" = "Returning the full layer."
      ))
      return(x)
    }

    # dplyr::filter() keeps only rows where the condition is TRUE.
    # !!region unquotes the variable so dplyr compares to its value,
    # not to the variable name "region".
    return(dplyr::filter(x, jurisdiction == !!region))
  }

  # ------------------------------------------------------------------
  # Case 3: Numeric bounding box — c(xmin, ymin, xmax, ymax)
  # In ITM coordinates (metres). Example:
  #   get_rivers(region = c(200000, 800000, 370000, 950000))
  # ------------------------------------------------------------------
  if (is.numeric(region) && length(region) == 4) {

    # Build an sf bounding box polygon from the four numbers
    # st_bbox() needs a named vector with xmin/ymin/xmax/ymax
    bbox_sfc <- sf::st_as_sfc(
      sf::st_bbox(
        c(xmin = region[1], ymin = region[2],
          xmax = region[3], ymax = region[4]),
        crs = eirer_crs()  # from utils_harmonise.R
      )
    )

    # sf::st_filter() keeps only features that intersect the bbox polygon
    return(sf::st_filter(x, bbox_sfc))
  }

  # ------------------------------------------------------------------
  # Case 4: sf or sfc object — clip to that geometry
  # A user might pass in a catchment polygon, for example:
  #   erne_catchment <- get_catchments(region = "Fermanagh")
  #   get_population(region = erne_catchment)
  # ------------------------------------------------------------------
  if (inherits(region, c("sf", "sfc"))) {

    # Reproject the clip geometry to match our data's CRS
    region_itm <- sf::st_transform(region, eirer_crs())

    # st_union() merges all features in region into one shape
    # so we filter against a single polygon, not a collection
    return(sf::st_filter(x, sf::st_union(region_itm)))
  }

  # ------------------------------------------------------------------
  # Case 5: bbox class — from sf::st_bbox()
  # Users might save a bbox and reuse it:
  #   my_box <- sf::st_bbox(some_layer)
  #   get_rivers(region = my_box)
  # ------------------------------------------------------------------
  if (inherits(region, "bbox")) {

    bbox_sfc <- sf::st_as_sfc(region) |>
      sf::st_transform(eirer_crs())

    return(sf::st_filter(x, bbox_sfc))
  }

  # ------------------------------------------------------------------
  # Case 6: Character vector — county name(s)
  # "Donegal" or c("Donegal", "Fermanagh", "Tyrone")
  # ------------------------------------------------------------------
  if (is.character(region)) {

    # Make sure there is a county column to filter on
    if (!county_col %in% names(x)) {
      cli::cli_abort(c(
        "Cannot filter by county name.",
        "x" = "Layer has no {.field {county_col}} column.",
        "i" = "Use an sf object or bounding box for {.arg region} instead."
      ))
    }

    # Get all available county names from the data
    available <- unique(x[[county_col]])

    # Normalise: lowercase + remove whitespace for comparison
    norm <- function(s) tolower(trimws(s))

    matched_counties <- character(0)  # empty vector to collect matches

    # Loop over each requested county name
    # seq_along() generates 1, 2, 3... for the length of region
    for (i in seq_along(region)) {
      req <- region[i]

      # Try exact match (case-insensitive)
      hits <- available[norm(available) == norm(req)]

      # If no exact match, try starts-with (partial match)
      # e.g. "don" would match "Donegal"
      if (length(hits) == 0) {
        hits <- available[startsWith(norm(available), norm(req))]
      }

      if (length(hits) == 0) {
        # Warn but continue — don't crash if one name is wrong
        cli::cli_warn("No county matching {.val {req}} found. Skipping.")
        next  # jump to the next iteration of the loop
      }

      # c() concatenates vectors: c(c("A","B"), "C") = c("A","B","C")
      matched_counties <- c(matched_counties, hits)

      # Inform the user which county was matched (helpful for partial matches)
      if (norm(hits[1]) != norm(req)) {
        cli::cli_inform("Matched {.val {req}} to {.val {hits[1]}}.")
      }
    }

    # If nothing matched at all, throw an error
    if (length(matched_counties) == 0) {
      cli::cli_abort(c(
        "No valid counties matched in {.arg region}.",
        "i" = "Available counties: {.val {sort(available)}}"
      ))
    }

    # %in% returns TRUE for each row where county is in our matched list
    return(dplyr::filter(x, .data[[county_col]] %in% matched_counties))
  }

  # ------------------------------------------------------------------
  # Fallback: nothing matched — give a clear error
  # ------------------------------------------------------------------
  cli::cli_abort(c(
    "Don't know how to use a {.cls {class(region)}} as {.arg region}.",
    "i" = "Accepted: NULL, 'ROI'/'NI', county name(s), sf object, bbox, or numeric c(xmin,ymin,xmax,ymax)."
  ))
}


# -----------------------------------------------------------------------------
# ireland_bbox()   [EXPORTED]
# -----------------------------------------------------------------------------

#' Pre-built bounding boxes for Ireland
#'
#' Returns a named numeric vector `c(xmin, ymin, xmax, ymax)` in ITM
#' coordinates (EPSG:2157, metres). Useful as a quick `region=` value
#' without having to look up coordinate values.
#'
#' @param area One of:
#'   - `"island"` — the whole island of Ireland (default)
#'   - `"ROI"`    — Republic of Ireland only
#'   - `"NI"`     — Northern Ireland only
#'
#' @return A named numeric vector with elements `xmin`, `ymin`, `xmax`, `ymax`.
#' @export
#'
#' @examples
#' ireland_bbox()           # whole island
#' ireland_bbox("NI")       # Northern Ireland only
#'
#' # Use directly as a region argument:
#' \dontrun{
#' get_rivers(region = ireland_bbox("NI"))
#' }
ireland_bbox <- function(area = c("island", "ROI", "NI")) {

  # match.arg() checks that the user supplied one of the allowed values
  # and helpfully suggests the correct ones if they made a typo.
  area <- match.arg(area)

  # switch() is like a lookup table: given area, return the matching vector.
  # These are approximate ITM (EPSG:2157) bounding boxes in metres.
  switch(area,
    island = c(xmin =  10000, ymin = 500000, xmax = 370000, ymax = 950000),
    ROI    = c(xmin =  10000, ymin = 500000, xmax = 370000, ymax = 840000),
    NI     = c(xmin =  60000, ymin = 820000, xmax = 370000, ymax = 950000)
  )
}
