# Purpose:
# Define study area geometries for modelling.
# - supports multiple named study areas
# - builds study area from base geometries + optional custom extensions
# - writes outputs to data_intermediate

source("01_setup.R")

message("Running: 02_define_study_area.R")

library(sf)
library(dplyr)
library(tigris)
library(here)

options(tigris_use_cache = TRUE)

# -----------------------------
# Helper: read custom polygon
# -----------------------------
read_custom_polygon <- function(name) {
  path <- here::here("data_raw", paste0(name, ".gpkg"))
  
  if (!file.exists(path)) {
    stop("Custom polygon not found: ", path)
  }
  
  sf::st_read(path, quiet = TRUE) %>%
    sf::st_transform(utm_11)
}

# -----------------------------
# Base geometries
# -----------------------------
county_sb <- tigris::counties(state = "CA", cb = TRUE, year = 2022) %>%
  dplyr::filter(NAME == "Santa Barbara") %>%
  sf::st_transform(utm_11)

city_sb <- tigris::places(state = "CA", cb = TRUE, year = 2022) %>%
  dplyr::filter(NAME == "Santa Barbara") %>%
  sf::st_transform(utm_11)

city_mc <- tigris::places(state = "CA", cb = TRUE, year = 2022) %>%
  dplyr::filter(NAME == "Mission Canyon") %>%
  sf::st_transform(utm_11)

# -----------------------------
# Define study area
# -----------------------------
if (study_area_name == "county") {
  
  study_area <- county_sb
  
} else if (study_area_name == "city_santa_barbara") {
  
  study_area <- city_sb
  
} else if (study_area_name == "city_santa_barbara_north") {
  
  # read custom rural extension polygon
  rural_ext <- read_custom_polygon("sb_rural_extension")
  
  # combine city + rural extension
  study_area <- dplyr::bind_rows(city_sb, city_mc, rural_ext) %>%
    sf::st_union() %>%
    sf::st_make_valid()
  
} else {
  stop("Unknown study_area_name: ", study_area_name)
}

# -----------------------------
# QA
# -----------------------------
message("Study area name: ", study_area_name)
message("Geometry type: ", unique(sf::st_geometry_type(study_area)))
message("CRS: ", sf::st_crs(study_area)$epsg)

# -----------------------------
# Write outputs
# -----------------------------
sf::st_write(
  county_sb,
  here::here("data_intermediate", "study_area_county.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  study_area,
  here::here("data_intermediate", paste0("study_area_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")
