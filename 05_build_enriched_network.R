# Purpose:
# Build an enriched network edge table for the active study area.
# - read prepared OSM and Replica layers
# - add OSM composite features
# - attach Replica predictors to OSM edges
# - add Replica composite features
# - write enriched network to disk

source("01_setup.R")

message("Running: 05_build_enriched_network.R")

# -----------------------------
# Settings
# -----------------------------
replica_max_dist_m <- 5
replica_aadt_thresh <- 1500
replica_speed_thresh <- 25

# -----------------------------
# Read inputs
# -----------------------------
osm <- sf::st_read(
  here::here("data_intermediate", paste0("osm_", study_area_name, ".gpkg")),
  quiet = TRUE
)

replica_aadt <- sf::st_read(
  here::here("data_intermediate", paste0("replica_aadt_", study_area_name, ".gpkg")),
  quiet = TRUE
)

replica_speed <- sf::st_read(
  here::here("data_intermediate", paste0("replica_speed_", study_area_name, ".gpkg")),
  quiet = TRUE
)

# -----------------------------
# Basic checks
# -----------------------------
if (!"osm_id" %in% names(osm)) {
  stop("OSM layer does not contain 'osm_id'. Add or rename an ID field upstream.")
}

# -----------------------------
# Add OSM composite features
# -----------------------------
network_enriched <- osm %>%
  make_osm_composite_features()

# -----------------------------
# Attach nearest Replica AADT to OSM edges
# -----------------------------
network_enriched <- add_nearest_edge_attributes(
  pts_sf      = network_enriched,
  edges_sf    = replica_aadt,
  attrs       = c("aadt"),
  edge_id_col = "id",
  max_dist_m  = replica_max_dist_m,
  prefix      = "replica_vol",
  keep_dist   = TRUE
)

# -----------------------------
# Attach nearest Replica speeds to OSM edges
# -----------------------------
network_enriched <- add_nearest_edge_attributes(
  pts_sf      = network_enriched,
  edges_sf    = replica_speed,
  attrs       = c("free_flow_speed_mph", "average_speed_mph", "speed_p95_mph"),
  edge_id_col = "id",
  max_dist_m  = replica_max_dist_m,
  prefix      = "replica_spd",
  keep_dist   = TRUE
)

# -----------------------------
# Add Replica composite features
# -----------------------------
network_enriched <- make_replica_composite_features(
  network_enriched,
  aadt_col       = "replica_vol_aadt",
  speed_col      = "replica_spd_average_speed_mph",
  dist_vol_col   = "replica_vol_dist_m",
  dist_spd_col   = "replica_spd_dist_m",
  max_valid_dist = replica_max_dist_m,
  aadt_thresh    = replica_aadt_thresh,
  speed_thresh   = replica_speed_thresh
)

# -----------------------------
# keep a clean subset of model-relevant columns
# (still retaining geometry)
# -----------------------------
network_enriched <- network_enriched %>%
  dplyr::mutate(
    road_class = as.factor(road_class),
    highway = as.factor(highway),
    surface_class = as.factor(surface_class),
    replica_missing = as.factor(replica_missing),
    replica_traffic_context = as.factor(replica_traffic_context),
    
    has_any_cycleway = as.logical(has_any_cycleway),
    has_lane = as.logical(has_lane),
    has_track = as.logical(has_track),
    is_unpaved = as.logical(is_unpaved),
    is_bridge = as.logical(is_bridge),
    is_oneway = as.logical(is_oneway),
    is_crossing = as.logical(is_crossing),
    bike_route_designated = as.logical(bike_route_designated),
    replica_volume_missing = as.logical(replica_volume_missing),
    replica_speed_missing = as.logical(replica_speed_missing),
    replica_low_stress = as.logical(replica_low_stress),
    
    lanes = suppressWarnings(as.numeric(lanes)),
    maxspeed = suppressWarnings(as.numeric(maxspeed))
  )

# -----------------------------
# Quick QA
# -----------------------------
message("Rows in enriched network: ", nrow(network_enriched))

message("Replica traffic context:")
print(table(network_enriched$replica_traffic_context, useNA = "ifany"))

message("Replica volume distance summary:")
print(summary(network_enriched$replica_vol_dist_m))

message("Replica speed distance summary:")
print(summary(network_enriched$replica_spd_dist_m))

message("Road class distribution:")
print(table(network_enriched$road_class, useNA = "ifany"))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  network_enriched,
  here::here("data_intermediate", paste0("network_enriched_", study_area_name, ".rds"))
)

sf::st_write(
  network_enriched,
  here::here("data_intermediate", paste0("network_enriched_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")
