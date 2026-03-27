# Purpose:
# Add OSM predictors to reference points.
# Workflow:
# 1. attach nearest osm_id and match distance
# 2. join selected OSM attributes by osm_id

source("01_setup.R")

message("Running: 05_join_predictors_osm.R")

# -----------------------------
# Read inputs
# -----------------------------
reference_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_", study_area_name, ".rds"))
)

osm <- sf::st_read(
  here::here("data_intermediate", paste0("osm_", study_area_name, ".gpkg")),
  quiet = TRUE
)

# -----------------------------
# Check required field
# -----------------------------
if (!"osm_id" %in% names(osm)) {
  stop("OSM layer does not contain 'osm_id'. Add or rename an ID field in 04_prepare_network_layers.R.")
}

# -----------------------------
# Attach nearest osm_id only
# -----------------------------
reference_pts_osm <- add_nearest_edge_attributes(
  pts_sf      = reference_pts,
  edges_sf    = osm,
  attrs       = c("osm_id"),
  edge_id_col = "osm_id",
  max_dist_m  = 10,
  prefix      = "osm",
  keep_dist   = TRUE,
  id_name     = "osm_id"
)

# -----------------------------
# Build OSM lookup table
# -----------------------------
osm_lookup <- osm %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    osm_id,
    name,
    Can_BICS,
    sb_class,
    road_class,
    highway,
    bicycle,
    bridge,
    cycleway,
    cycleway.left,
    cycleway.right,
    cycleway.both,
    foot,
    footway,
    hgv,
    horse,
    junction,
    lanes,
    lanes.backward,
    lanes.forward,
    lcn,
    lcn_ref,
    lit,
    maxspeed,
    motor_vehicle,
    oneway,
    oneway.bicycle,
    path,
    railway,
    segregated,
    sidewalk,
    sidewalk.both.surface,
    surface,
    surface_class,
    is_unpaved,
    traffic_calming,
    width,
    local_cycle_network,
    local_cycle_network_name,
    regional_cycle_network,
    regional_cycle_network_name,
    national_cycle_network,
    national_cycle_network_name,
    bike_route_designated,
    has_any_cycleway,
    has_lane,
    has_track,
    is_bridge,
    is_oneway,
    access,
    natural,
    length_km
  )

# -----------------------------
# Join OSM attributes by osm_id
# -----------------------------
reference_pts_osm <- reference_pts_osm %>%
  dplyr::left_join(osm_lookup, by = "osm_id")

# -----------------------------
# Quick QA
# -----------------------------
message("OSM join complete.")
message("Rows with matched osm_id: ", sum(!is.na(reference_pts_osm$osm_id)), " / ", nrow(reference_pts_osm))

message("OSM distance summary:")
print(summary(reference_pts_osm$osm_dist_m))

message("Road class distribution:")
print(table(reference_pts_osm$road_class, useNA = "ifany"))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  reference_pts_osm,
  here::here("data_intermediate", paste0("reference_points_osm_", study_area_name, ".rds"))
)

sf::st_write(
  reference_pts_osm,
  here::here("data_intermediate", paste0("reference_points_osm_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")