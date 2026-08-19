# Purpose:
# Project setup file.
# Loads packages, defines CRS and run settings, sets options,
# creates core directories, and sources helper functions.

message("Running: 01_setup.R")

# -----------------------------
# Packages
# -----------------------------
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(here)
library(tigris)
library(tidyr)
library(purrr)

# Optional modelling packages (load here only if you want them available everywhere)
# library(tidymodels)
# library(xgboost)
# library(mapview)

# -----------------------------
# OSM download control
# -----------------------------
download_osm <- F
refresh_osm_layer <- T

# -----------------------------
# Replica
# -----------------------------
replica_year <- 2025

replica_aadt_file <- here::here(
  "data_raw", "replica",
  paste0("aadt_", replica_year, "_sb.geojson")
)

replica_speed_file <- here::here(
  "data_raw", "replica",
  paste0("annual_speeds_", replica_year, "_sb.geojson")
)

# -----------------------------
# Project settings
# -----------------------------

# Coordinate reference systems
utm_11 <- 32611
crs_wgs84 <- 4326

# Current study area name
# Change this when switching between prototype areas / full runs
# study_area_name <- "city_santa_barbara_public"

#study_area_name <- "city_santa_barbara_north"
study_area_name <- "county"


# -----------------------------
# Model predictor definitions
# -----------------------------

model_predictors <- c(
  # OSM / infrastructure
  "road_class",
  "highway",
  "lanes",
  "maxspeed",
  "maxspeed_mph",
  "surface_class",
  "is_unpaved",
  "has_any_cycleway",
  "has_lane",
  "has_track",
  "has_bike_buffer",
  "has_bike_separation",
  "bike_route_designated",
  "is_bridge",
  "is_oneway",
  "sidewalk",
  "segregated",
  "traffic_calming",
  "access",
  "is_bike_path",
  "is_paved_bike_path",
  "bike_allowed_explicit",
  
  # Replica
  "replica_vol_aadt",
  "replica_spd_average_speed_mph",
  "replica_missing",
  "replica_traffic_context",
  "replica_log_aadt"
)

comfort_predictors <- c(
  "strava_total_trip_count",
  "strava_total_commute_pct",
  "strava_pct_wmn",
  "strava_id",
  #"strava_dist_m",
  "strava_count_missing",
  "strava_log_count",
  "incident_count_250m",
  "incident_log_count_250m"
)

postprocess_cols <- c(
  "highway",
  "road_class",
  "surface_class",
  "is_bike_path",
  "is_paved_bike_path",
  "is_crossing",
  "is_bike_crossing",
  "has_lane",
  "has_shared_lane",
  "has_track",
  "has_bike_buffer",
  "has_bike_separation",
  "bike_allowed_explicit",
  "replica_traffic_context",
  "has_bike_infra_signal",
  "is_low_speed_volume_candidate",
  "high_speed_context",
  "has_route_signal",
  "traffic_calming_present"
  
)

logical_feature_cols <- c(
  "has_any_cycleway",
  "has_lane",
  "has_shared_lane",
  "has_track",
  "has_bike_buffer",
  "has_bike_separation",
  "is_bike_path",
  "is_paved_bike_path",
  "bike_allowed_explicit",
  "is_unpaved",
  "is_bridge",
  "is_oneway",
  "is_crossing",
  "is_bike_crossing",
  "bike_route_designated",
  "replica_volume_missing",
  "replica_speed_missing",
  "replica_low_stress",
  "traffic_calming_present",
  "has_route_signal",
  "is_low_speed_volume_candidate",
  "high_speed_posted",
  "high_speed_context",
  "has_shared_lane"
)

factor_feature_cols <- c(
  "road_class",
  "highway",
  "surface_class",
  "replica_missing",
  "replica_traffic_context",
  "high_speed_source"
)

numeric_feature_cols <- c(
  "lanes",
  "maxspeed_mph",
  "replica_vol_aadt",
  "replica_spd_average_speed_mph",
  "replica_log_aadt"
)

# -----------------------------
# Model id definitions
# -----------------------------

model_id_cols <- c("SegmentID", "osm_id", "osm_dist_m", "source_wave", "stratum", "block_id", "reviewer")
network_id_cols <- c("osm_id", "name")

# -----------------------------
# tigris options
# -----------------------------
options(tigris_use_cache = TRUE)
options(tigris_cache_dir = here::here("data_raw", "tigris_cache"))
options(stringsAsFactors = FALSE)

# -----------------------------
# Export classes and comfort levels
# -----------------------------
class_lookup <- tibble::tribble(
  ~class_export, ~comfort_class, ~display_order,
  "Paths", "High comfort", 1,
  "Bike lane (physical protection)", "High comfort", 2,
  "Bike lane (painted buffer)", "Medium comfort", 3,
  "Bike boulevard", "Medium comfort", 4,
  "Signed low-speed/low-volume route", "Low comfort", 5,
  "Bike lane (no buffer)", "Low comfort", 6,
  "Low-speed/low-volume street", "Low comfort", 7,
  "Trails (gravel)", "Non-conforming", 8,
  "Non-conforming infrastructure", "Non-comforming", 9,
  "No infrastructure", "Non-conforming", 10
)

final_class_levels <- c(
  "Paths",
  "Bike lane (painted buffer)",
  "Bike lane (physical protection)",
  "Bike lane (no buffer)",
  "Bike boulevard",
  "Low-speed/low-volume signed route",
  "Low-speed/low-volume street",
  "Trails (gravel)",
  "No infrastructure",
  "Non-conforming infrastructure"
) %>%
  unique()

# -----------------------------
# Create core directories if needed
# -----------------------------
dir.create(here::here("data_raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data_intermediate"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data_processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "maps"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "models"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Source helper functions
# -----------------------------
source(here::here("R", "helpers_sf.R"))

# Source these as they become useful
source(here::here("R", "helpers_features.R"))
source(here::here("R", "helpers_sf.R"))
source(here::here("R", "helpers_modeling.R"))
# source(here::here("R", "helpers_io.R"))

# -----------------------------
# Run summary
# -----------------------------
message("Study area: ", study_area_name)
message("Projected CRS (utm_11): ", utm_11)
message("WGS84 CRS: ", crs_wgs84)
message("Setup complete.")
