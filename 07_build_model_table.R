# Purpose:
# Build a clean modelling table from enriched reference points.
# - select outcome, IDs, and candidate predictors
# - apply light type cleaning
# - save model-ready table

source("01_setup.R")

message("Running: 08_build_model_table.R")

# -----------------------------
# Read inputs
# -----------------------------
model_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_osm_replica_", study_area_name, ".rds"))
)

# -----------------------------
# Select fields for modelling + QA
# -----------------------------
model_table <- model_pts %>%
  dplyr::select(
    # IDs / metadata
    SegmentID,
    source_wave,
    stratum,
    block_id,
    reviewer,
    
    # outcome
    class,
    
    # OSM linkage / QA
    osm_id,
    osm_dist_m,
    
    # OSM predictors
    Can_BICS,
    sb_class,
    road_class,
    highway,
    lanes,
    maxspeed,
    bicycle,
    oneway,
    surface,
    surface_class,
    is_unpaved,
    bike_route_designated,
    has_any_cycleway,
    has_lane,
    has_track,
    is_bridge,
    is_oneway,
    sidewalk,
    segregated,
    traffic_calming,
    access,
    length_km,
    
    # Replica predictors
    replica_vol_aadt,
    replica_spd_average_speed_mph,
    replica_spd_free_flow_speed_mph,
    replica_spd_speed_p95_mph,
    replica_vol_dist_m,
    replica_spd_dist_m,
    replica_volume_missing,
    replica_speed_missing,
    replica_missing,
    replica_low_stress,
    replica_traffic_context,
    replica_log_aadt,
    
    # keep geometry
    geom
  ) %>%
  # -----------------------------
# Light type cleaning
# -----------------------------
dplyr::mutate(
  class = factor(class),
  
  source_wave = factor(source_wave),
  stratum = factor(stratum),
  reviewer = factor(reviewer),
  block_id = as.character(block_id),
  
  road_class = factor(road_class),
  highway = factor(highway),
  surface_class = factor(surface_class),
  replica_missing = factor(replica_missing),
  replica_traffic_context = factor(replica_traffic_context),
  
  has_any_cycleway = as.logical(has_any_cycleway),
  has_lane = as.logical(has_lane),
  has_track = as.logical(has_track),
  is_unpaved = as.logical(is_unpaved),
  is_bridge = as.logical(is_bridge),
  is_oneway = as.logical(is_oneway),
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
message("Rows in model table: ", nrow(model_table))

message("Outcome distribution:")
print(table(model_table$class, useNA = "ifany"))

message("Missingness summary:")
print(colSums(is.na(sf::st_drop_geometry(model_table))))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  model_table,
  here::here("data_intermediate", paste0("model_table_", study_area_name, ".rds"))
)

# optional non-spatial copy for quick inspection
readr::write_csv(
  sf::st_drop_geometry(model_table),
  here::here("data_intermediate", paste0("model_table_", study_area_name, ".csv"))
)

message("Done.")