# =============================================================================
# data-raw/build_registry.R
# Builds eire_datasets — the EireR dataset catalogue.
# Run once with: source("data-raw/build_registry.R")
# Output: data/eire_datasets.rda (ships inside the package)
#
# THREE DATA SOURCES — each requires a different discovery approach:
#
# SECTION 1: EPA GeoServer (WFS protocol)
#   Standard OGC Web Feature Service. sf::st_layers() queries capabilities.
#   No authentication needed. Default httr2 requests work.
#
# SECTION 2: ArcGIS Hub (GeoHive ROI + DAERA NI)
#   Both portals run ArcGIS Hub software with identical REST APIs.
#   One discover_hub() function handles both jurisdictions.
#   Default httr2 requests work — no special headers needed.
#
# SECTION 3: OpenDataNI (CKAN portal)
#   A different platform to Sections 1 and 2.
#   CKAN is open source software used by many government portals worldwide.
#   OpenDataNI's server rejects the default httr2 User-Agent string (403).
#   Solution: provide a transparent EireR User-Agent instead.
#   File downloads also return 403 via httr2 — stored as /vsicurl/ URLs
#   so sf::read_sf() can stream them directly via GDAL instead.
# =============================================================================
 
library(sf)
library(dplyr)
library(tibble)
library(stringr)
library(usethis)
library(cli)
library(httr2)
library(purrr)
 
# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
 
`%||%` <- function(a, b) if (!is.null(a)) a else b
 
# Build a permanent WFS download URL, encoding spaces in layer names
build_wfs_url <- function(base_url, layer_name) {
  paste0(
    base_url,
    "?service=WFS&version=1.0.0&request=GetFeature",
    "&typeName=", utils::URLencode(layer_name, reserved = FALSE),
    "&outputFormat=application/json",
    "&srsName=EPSG:4326"
  )
}
 
# Build a permanent ArcGIS REST FeatureServer GeoJSON download URL
build_arcgis_url <- function(service_url, layer_id = 0) {
  paste0(service_url, "/", layer_id, "/query?outFields=*&where=1%3D1&f=geojson")
}
 
# Convert a layer name to a clean snake_case dataset_id
make_dataset_id <- function(prefix, layer_name) {
  paste0(prefix, "_", layer_name |>
    str_remove("^[A-Za-z]+:") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("_$"))
}
 
# Normalise geometry type strings across different server types
classify_geometry <- function(geom_type) {
  g <- tolower(geom_type %||% "")
  case_when(
    str_detect(g, "point")                ~ "point",
    str_detect(g, "line|curve|polyline")  ~ "line",
    str_detect(g, "polygon|surface|area") ~ "polygon",
    TRUE                                   ~ "vector"
  )
}
 
# Assign a theme from layer name, categories and tags
classify_theme <- function(text) {
  n <- tolower(text %||% "")
  case_when(
    str_detect(n, "river|lake|canal|catchment|basin|flood|groundwater|wfd|stream|wetland") ~ "hydrology",
    str_detect(n, "county|district|boundary|electoral|townland|province|lgd|lea|statutory") ~ "boundary",
    str_detect(n, "population|census|deprivation|demographic|household")                    ~ "population",
    str_detect(n, "corine|land.cover|habitat|natura|protected|forest|peat|vegetation")      ~ "land_cover",
    str_detect(n, "elevation|dem|dtm|contour|terrain|lidar|height|topograph")               ~ "elevation",
    str_detect(n, "road|rail|transport|route|motorway|cycle|ferry|traffic")                  ~ "transport",
    str_detect(n, "placename|gazetteer|locality|address|postcode")                           ~ "placenames",
    str_detect(n, "air|noise|soil|waste|ippc|discharge|radon|emission|pollution")           ~ "environment",
    str_detect(n, "coast|sea|marine|ocean|tide|shore|offshore|maritime|ospar")              ~ "marine",
    str_detect(n, "geolog|rock|sediment|mineral|borehole|aquifer")                          ~ "geology",
    str_detect(n, "trail|walk|recreation|amenity|park|tourism|heritage")                    ~ "recreation",
    TRUE                                                                                      ~ "other"
  )
}
 
# Returns raw sf_layers object — avoids tibble conversion which causes
# column length errors due to sf internals (driver col = length 1)
query_wfs_layers <- function(url, service_name) {
  cli::cli_inform("Querying {service_name}...")
  tryCatch(
    sf::st_layers(url),
    error = function(e) {
      cli::cli_warn("Could not reach {service_name}: {e$message}")
      NULL
    }
  )
}
 
# Query an ArcGIS Hub Search API and return registry rows.
# Used for both GeoHive (ROI) and DAERA (NI) — same API, different URLs.
# GeoHive uses 1-based startindex pagination (startindex=0 returns 400).
discover_hub <- function(base_url, jurisdiction) {
  page_size  <- 100
  all_items  <- list()
 
  total <- tryCatch({
    request(base_url) |>
      req_url_query(limit = 1) |>
      req_headers(`User-Agent` = "EireR/0.1.0 R-package (github.com/Aoibhinmurphy/EireR)") |>
      req_perform() |>
      resp_body_json() |>
      pluck("numberMatched") %||% 500
  }, error = function(e) 500)
 
  cli::cli_inform("Querying {jurisdiction} Hub ({total} datasets)...")
 
  startindex <- 1
  for (page in seq_len(ceiling(total / page_size))) {
    resp <- tryCatch(
      request(base_url) |>
        req_url_query(limit = page_size, startindex = startindex) |>
        req_headers(`User-Agent` = "EireR/0.1.0 R-package (github.com/Aoibhinmurphy/EireR)") |>
        req_error(is_error = \(r) FALSE) |>
        req_perform(),
      error = function(e) {
        cli::cli_warn("Request failed page {page}: {e$message}")
        NULL
      }
    )
 
    if (is.null(resp)) break
    if (resp_status(resp) >= 400) {
      cli::cli_warn("HTTP {resp_status(resp)} — stopping.")
      break
    }
 
    items <- resp_body_json(resp) |> pluck("features")
    if (is.null(items) || length(items) == 0) break
 
    all_items  <- c(all_items, items)
    startindex <- startindex + page_size
    cli::cli_inform("  {length(all_items)}/{total} retrieved...")
    if (length(all_items) >= total) break
  }
 
  if (length(all_items) == 0) {
    cli::cli_warn("No items from {jurisdiction} Hub.")
    return(tibble())
  }
 
  rows <- map_dfr(all_items, \(item) {
    service_url <- pluck(item, "properties", "url")
    if (is.null(service_url) ||
        !str_detect(service_url, "FeatureServer|MapServer")) return(NULL)
 
    layer_name  <- pluck(item, "properties", "title")    %||% ""
    licence_raw <- pluck(item, "properties", "license")  %||% "See provider"
    source      <- pluck(item, "properties", "source")   %||% "Unknown"
    crs_raw     <- pluck(item, "properties", "spatialReference") %||% "4326"
    modified_ms <- pluck(item, "properties", "modified")
    categories  <- pluck(item, "properties", "categories") %||% list()
    tags        <- pluck(item, "properties", "tags")       %||% list()
 
    last_update <- if (!is.null(modified_ms)) {
      format(as.POSIXct(modified_ms / 1000, origin = "1970-01-01"), "%Y-%m-%d")
    } else NA_character_
 
    theme_input <- paste(
      layer_name,
      paste(tolower(unlist(categories)), collapse = " "),
      paste(tolower(unlist(tags)),       collapse = " ")
    )
 
    tibble(
      dataset_id    = make_dataset_id("hub", layer_name),
      name          = layer_name,
      jurisdiction  = jurisdiction,
      provider      = source,
      provider_url  = if (jurisdiction == "ROI") "https://www.geohive.ie" else "https://www.daera-ni.gov.uk",
      download_url  = build_arcgis_url(service_url, 0),
      licence       = case_when(
        str_detect(tolower(licence_raw), "cc.by.4|cc-by-4") ~ "CC BY 4.0",
        str_detect(tolower(licence_raw), "ogl|open.gov")     ~ "Open Government Licence v3.0",
        TRUE                                                  ~ licence_raw
      ),
      licence_url   = NA_character_,
      layer_type    = "vector",
      theme         = classify_theme(theme_input),
      service_type  = "ArcGIS REST",
      service_url   = service_url,
      service_layer = "0",
      output_format = "GeoJSON",
      crs_original  = if (str_detect(crs_raw, "^EPSG")) crs_raw else paste0("EPSG:", crs_raw),
      date_acquired = as.character(Sys.Date()),
      last_update   = last_update,
      notes         = paste0("Auto-discovered from ArcGIS Hub (", jurisdiction, ").")
    )
  })
 
  cli::cli_inform("{jurisdiction} Hub: {nrow(rows)} spatial layer{?s} from {length(all_items)} items")
  rows
}
 
# Query OpenDataNI CKAN API and return registry rows.
#
# WHY THIS IS DIFFERENT TO discover_hub():
#   OpenDataNI uses CKAN — a completely platform to ArcGIS Hub.
#   The API structure, pagination, and response format are all different:
#     ArcGIS Hub: /api/search/v1/collections/all/items, startindex pagination
#     CKAN:       /api/3/action/package_search,         start pagination
#
# WHY A CUSTOM USER-AGENT:
#   OpenDataNI's server rejects the default httr2 User-Agent with HTTP 403.
#   Rather than using a browser, we send a transparent string
#   identifying EireR as the client. This is standard practice for R packages
#   making HTTP requests and is transparent about what is accessing the data.
#   All data accessed is openly licensed under OGL v3.
#
# WHY /vsicurl/ IN DOWNLOAD URLS:
#   Direct file downloads from OpenDataNI also return 403 via httr2.
#   Prepending /vsicurl/ tells sf::read_sf() to use GDAL's HTTP client
#   instead, which OpenDataNI accepts. This is stored in the registry URL
#   and handled automatically by read_spatial() in utils_cache.R.
discover_ckan <- function(base_url = "https://admin.opendatani.gov.uk",
                           rows_per_page = 100) {
 
  # Honest, descriptive User-Agent identifying EireR as the client
  eirer_ua <- "EireR/0.1.0 R-package (github.com/Aoibhinmurphy/EireR)"
 
  cli::cli_inform("Querying OpenDataNI CKAN API...")
 
  total <- tryCatch({
    request(base_url) |>
      req_url_path_append("api", "3", "action", "package_search") |>
      req_url_query(q = "*:*", rows = 1, `fq` = "res_format:GeoJSON") |>
      req_headers(`User-Agent` = eirer_ua) |>
      req_perform() |>
      resp_body_json() |>
      pluck("result", "count") %||% 0
  }, error = function(e) {
    cli::cli_warn("Could not reach OpenDataNI CKAN: {e$message}")
    0
  })
 
  if (total == 0) {
    cli::cli_warn("No GeoJSON datasets found on OpenDataNI.")
    return(tibble())
  }
 
  cli::cli_inform("OpenDataNI: {total} GeoJSON datasets found")
 
  all_datasets <- list()
  start <- 0
 
  for (page in seq_len(ceiling(total / rows_per_page))) {
    resp <- tryCatch(
      request(base_url) |>
        req_url_path_append("api", "3", "action", "package_search") |>
        req_url_query(
          q     = "*:*",
          rows  = rows_per_page,
          start = start,
          `fq`  = "res_format:GeoJSON"
        ) |>
        req_headers(`User-Agent` = eirer_ua) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform(),
      error = function(e) {
        cli::cli_warn("CKAN request failed page {page}: {e$message}")
        NULL
      }
    )
 
    if (is.null(resp) || resp_status(resp) >= 400) break
 
    datasets <- resp_body_json(resp) |> pluck("result", "results")
    if (is.null(datasets) || length(datasets) == 0) break
 
    all_datasets <- c(all_datasets, datasets)
    start        <- start + rows_per_page
    cli::cli_inform("  {length(all_datasets)}/{total} retrieved...")
    if (length(all_datasets) >= total) break
  }
 
  if (length(all_datasets) == 0) return(tibble())
 
  rows <- map_dfr(all_datasets, \(dataset) {
    resources <- dataset$resources %||% list()
 
    geojson_resources <- Filter(\(r) {
      tolower(r$format %||% "") == "geojson" && !is.null(r$url)
    }, resources)
 
    if (length(geojson_resources) == 0) return(NULL)
 
    map_dfr(geojson_resources, \(resource) {
      layer_name  <- dataset$title %||% ""
      raw_url     <- resource$url  %||% ""
 
      # /vsicurl/ prefix routes the download through GDAL's HTTP client
      # which OpenDataNI accepts, bypassing the 403 httr2 receives
      vsicurl_url <- paste0("/vsicurl/", raw_url)
 
      tibble(
        dataset_id    = make_dataset_id("ckan", layer_name),
        name          = layer_name,
        jurisdiction  = "NI",
        provider      = dataset$organization$title %||% "OpenDataNI",
        provider_url  = "https://www.opendatani.gov.uk",
        download_url  = vsicurl_url,
        licence       = "Open Government Licence v3.0",
        licence_url   = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
        layer_type    = "vector",
        theme         = classify_theme(layer_name),
        service_type  = "CKAN",
        service_url   = base_url,
        service_layer = resource$id %||% NA_character_,
        output_format = "GeoJSON",
        crs_original  = "EPSG:4326",
        date_acquired = as.character(Sys.Date()),
        last_update   = as.character(as.Date(
          dataset$metadata_modified %||% NA_character_
        )),
        notes = paste0(
          "OpenDataNI CKAN. EireR User-Agent + /vsicurl/ access. ",
          str_trunc(dataset$notes %||% "", 150)
        )
      )
    })
  })
 
  cli::cli_inform("OpenDataNI: {nrow(rows)} GeoJSON layer{?s} from {length(all_datasets)} datasets")
  rows
}
 
# =============================================================================
# SECTION 1: EPA GEOSERVER (WFS)
# =============================================================================
 
epa_base <- "https://gis.epa.ie/geoserver/EPA/ows"
 
epa_keywords <- c(
  "river", "lake", "canal", "catchment", "basin", "flood", "groundwater",
  "wfd", "waste", "air", "noise", "soil", "habitat", "natura", "protected",
  "monitoring", "discharge", "ippc", "radon", "wetland", "peat", "coast"
)
 
epa_layers <- query_wfs_layers(paste0("WFS:", epa_base), "EPA GeoServer")
 
if (!is.null(epa_layers)) {
  keyword_pattern <- paste(epa_keywords, collapse = "|")
  keep <- which(
    epa_layers$features > 0 &
    str_detect(tolower(epa_layers$name), keyword_pattern)
  )
  epa_auto <- map_dfr(keep, \(i) tibble(
    dataset_id    = make_dataset_id("epa", epa_layers$name[i]),
    name          = epa_layers$name[i] |>
                      str_remove("^EPA:") |>
                      str_replace_all("_", " ") |>
                      str_to_title(),
    jurisdiction  = "ROI",
    provider      = "Environmental Protection Agency (EPA)",
    provider_url  = "https://www.epa.ie",
    download_url  = build_wfs_url(epa_base, epa_layers$name[i]),
    licence       = "CC BY 4.0",
    licence_url   = "https://creativecommons.org/licenses/by/4.0/",
    layer_type    = classify_geometry(epa_layers$geomtype[[i]][[1]] %||% ""),
    theme         = classify_theme(epa_layers$name[i]),
    service_type  = "WFS",
    service_url   = epa_base,
    service_layer = epa_layers$name[i],
    output_format = "GeoJSON",
    crs_original  = "EPSG:4326",
    date_acquired = as.character(Sys.Date()),
    last_update   = NA_character_,
    notes         = paste0("EPA GeoServer WFS. Feature count: ", epa_layers$features[i], ".")
  ))
  cli::cli_inform("EPA: {nrow(epa_auto)} layer{?s} kept from {length(epa_layers$name)} total")
} else {
  epa_auto <- tibble()
}
 
# =============================================================================
# SECTION 2: ARCGIS HUB — GeoHive (ROI) + DAERA (NI)
# =============================================================================
 
geohive_auto <- discover_hub(
  base_url     = "https://production-geohive.hub.arcgis.com/api/search/v1/collections/all/items",
  jurisdiction = "ROI"
)
 
daera_auto <- discover_hub(
  base_url     = "https://opendata-daerani.hub.arcgis.com/api/search/v1/collections/all/items",
  jurisdiction = "NI"
)
 
# =============================================================================
# SECTION 3: OPENDATA NI (CKAN)
# =============================================================================
 
ckan_auto <- discover_ckan()
 
# =============================================================================
# SECTION 4: COMBINE AND SAVE
# =============================================================================
 
eire_datasets <- bind_rows(epa_auto, geohive_auto, daera_auto, ckan_auto) |>
  distinct(dataset_id, .keep_all = TRUE) |>
  filter(!is.na(download_url) | !is.na(service_url)) |>
  arrange(jurisdiction, theme, dataset_id)
 
usethis::use_data(eire_datasets, overwrite = TRUE, internal = FALSE)
 
cli::cli_inform(c(
  "v" = "Registry built: {nrow(eire_datasets)} datasets total",
  "i" = "EPA: {nrow(epa_auto)}  GeoHive: {nrow(geohive_auto)}  DAERA: {nrow(daera_auto)}  CKAN: {nrow(ckan_auto)}",
  "i" = "Themes: {paste(sort(unique(eire_datasets$theme)), collapse = ', ')}"
))

