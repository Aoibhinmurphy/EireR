# =============================================================================
# R/plot.R
# Map visualisation for EireR sf objects.
# plot_eire() is the main user-facing function — it handles points, lines
# and polygons automatically, with optional basemap, proportional sizing,
# flexible colour palettes, and legend positioning.
# =============================================================================
 
#' @importFrom ggplot2 ggplot geom_sf aes theme_minimal theme element_text element_line element_rect unit scale_fill_viridis_c scale_fill_viridis_d scale_colour_viridis_c scale_colour_viridis_d scale_fill_manual scale_colour_manual scale_size_continuous coord_sf labs
#' @importFrom sf st_geometry_type st_union st_cast st_bbox
#' @importFrom dplyr filter
NULL
 
#' Plot an EireR catalogue layer over extent of Ireland
#'
#' Produces a map of any `sf` object returned by EireR, automatically
#' detecting geometry type (point, line, polygon) and applying the
#' appropriate aesthetics. The map zooms to the data extent with 20%
#' padding and optionally draws county outlines underneath.
#'
#' @param x An `sf` object from any `get_*()` or `get_layer()` call.
#' @param fill_by Column name to colour features by.
#' @param size_by Column name to scale point sizes by.
#'   Only applies to point geometry — larger values produce larger points.
#' @param title for Map title. Enter `NULL` for no title.
#' @param palette Colour palette. Viridis options:
#'   `"A"` (magma), `"B"` (inferno), `"C"` (plasma), `"D"` (viridis, default),
#'   `"E"` (cividis), `"F"` (rocket), `"G"` (mako), `"H"` (turbo).
#'   Use `"random"` for distinct colours — best for categorical
#'   data with many categories.
#' @param basemap Draw county outlines underneath the data
#'   Default `TRUE`. Set to `FALSE` when note needed, e.g., when `x` is itself a county layer
#'   to avoid drawing counties twice.
#' @param legend_pos Legend position: `"bottom"` (default) or `"right"`.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' counties <- get_counties()
#'
#' # Colour by jurisdiction
#' plot_eire(counties, fill_by = jurisdiction,
#'           title = "Island of Ireland", basemap = FALSE)
#'
#' # All 32 counties (ROI and NI) with distinct colours
#' plot_eire(counties, fill_by = county, title = "32 Counties of Ireland",
#'           palette = "random", basemap = FALSE, legend_pos = "right")
#'
#' # River ecological status on top of county basemap
#' rivers <- get_layer("epa_highstatusobjective_rivers")
#' plot_eire(rivers, fill_by = ECO_Status,
#'           title = "High Ecological Status Rivers")
#'
#' }
plot_eire <- function(x,
                      fill_by    = NULL,
                      size_by    = NULL,
                      title      = NULL,
                      palette    = "D",
                      basemap    = TRUE,
                      legend_pos = "bottom") {
 
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Please install {.pkg ggplot2}")
  }
 
  # Capture column names as strings from unquoted arguments
  fill_col <- tryCatch(deparse(substitute(fill_by)), error = function(e) NULL)
  size_col <- tryCatch(deparse(substitute(size_by)), error = function(e) NULL)
  if (identical(fill_col, "NULL") || is.null(fill_col)) fill_col <- NULL
  if (identical(size_col, "NULL") || is.null(size_col)) size_col <- NULL
 
  # 32 maximally distinct colours for categorical data with many categories
  random <- c(
    "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4",
    "#42d4f4", "#f032e6", "#bfef45", "#fabed4", "#469990", "#dcbeff",
    "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1",
    "#000075", "#a9a9a9", "#4e8c72", "#c0392b", "#2980b9", "#8e44ad",
    "#f39c12", "#27ae60", "#d35400", "#2c3e50", "#e74c3c", "#1abc9c",
    "#7f8c8d", "#f1c40f"
  )
 
  # Zoom to the data extent with 30% padding on each side
  bbox  <- sf::st_bbox(x)
  x_buf <- (bbox["xmax"] - bbox["xmin"]) * 0.3
  y_buf <- (bbox["ymax"] - bbox["ymin"]) * 0.3
  xlim  <- c(bbox["xmin"] - x_buf, bbox["xmax"] + x_buf)
  ylim  <- c(bbox["ymin"] - y_buf, bbox["ymax"] + y_buf)
 
  gg <- ggplot2::ggplot()
 
  # Draw county outlines as a neutral background layer.
  # Useful when plotting rivers, points etc — gives geographic context.
  # Turn off with basemap=FALSE when x is already a county/boundary layer.
  if (basemap) {
    bg <- tryCatch(get_counties(quiet = TRUE), error = function(e) NULL)
    if (!is.null(bg)) {
      gg <- gg + ggplot2::geom_sf(
        data = bg, fill = "#f5f0e8", colour = "#cccccc", linewidth = 0.15
      )
      # Add the ROI/NI border as a dashed red line
      border <- tryCatch({
        bg |>
          dplyr::filter(jurisdiction == "NI") |>
          sf::st_union() |>
          sf::st_cast("MULTILINESTRING")
      }, error = function(e) NULL)
 
      if (!is.null(border)) {
        gg <- gg + ggplot2::geom_sf(
          data = border, colour = "#c0392b",
          linewidth = 0.4, linetype = "dashed", alpha = 0.6
        )
      }
    }
  }
 
  # Detect the dominant geometry type to choose the right aesthetic mapping.
  # Polygons use fill=, lines and points use colour=.
  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  is_point  <- grepl("POINT", geom_type, ignore.case = TRUE)
  is_line   <- grepl("LINE",  geom_type, ignore.case = TRUE)
 
  if (!is.null(fill_col) && fill_col %in% names(x)) {
    is_num <- is.numeric(x[[fill_col]])
 
    if (is_point) {
      # Points — optionally scale size by a numeric column
      if (!is.null(size_col) && size_col %in% names(x)) {
        gg <- gg + ggplot2::geom_sf(
          data  = x,
          ggplot2::aes(colour = .data[[fill_col]],
                       size   = .data[[size_col]]),
          alpha = 0.8
        ) +
        # range = c(min, max) controls the smallest and largest point size
        ggplot2::scale_size_continuous(name = size_col, range = c(1, 8))
      } else {
        gg <- gg + ggplot2::geom_sf(
          data  = x,
          ggplot2::aes(colour = .data[[fill_col]]),
          size  = 2, alpha = 0.8
        )
      }
    } else if (is_line) {
      gg <- gg + ggplot2::geom_sf(
        data      = x,
        ggplot2::aes(colour = .data[[fill_col]]),
        linewidth = 0.8, alpha = 0.85
      )
    } else {
      # Polygons — white borders between features for clarity
      gg <- gg + ggplot2::geom_sf(
        data      = x,
        ggplot2::aes(fill = .data[[fill_col]]),
        colour    = "#ffffff", linewidth = 0.1, alpha = 0.9
      )
    }
 
    # Apply colour scale based on data type and palette choice
    if (is_num) {
      gg <- gg +
        ggplot2::scale_fill_viridis_c(option = palette, name = fill_col) +
        ggplot2::scale_colour_viridis_c(option = palette, name = fill_col)
    } else if (palette == "random") {
      gg <- gg +
        ggplot2::scale_fill_manual(values = random, name = fill_col) +
        ggplot2::scale_colour_manual(values = random, name = fill_col)
    } else {
      gg <- gg +
        ggplot2::scale_fill_viridis_d(option = palette, name = fill_col) +
        ggplot2::scale_colour_viridis_d(option = palette, name = fill_col)
    }
 
  } else {
    # No fill_by provided — draw everything in a single neutral green
    gg <- gg + ggplot2::geom_sf(
      data = x, fill = "#4e8c72", colour = "#4e8c72", alpha = 0.8
    )
  }
 
  gg +
    ggplot2::coord_sf(crs = eirer_crs(), xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 13, face = "bold", colour = "#2c2c2c"),
      panel.grid        = ggplot2::element_line(colour = "#e8e4dc", linewidth = 0.2),
      axis.text         = ggplot2::element_text(size = 8, colour = "#999"),
      legend.position   = legend_pos,
      legend.key.height = ggplot2::unit(0.35, "cm"),
      legend.key.width  = ggplot2::unit(1.5, "cm"),
      legend.text       = ggplot2::element_text(size = 7),
      plot.background   = ggplot2::element_rect(fill = "#fafaf7", colour = NA)
    )
}
 
`%||%` <- function(a, b) if (!is.null(a) && !identical(a, "unknown")) a else b

