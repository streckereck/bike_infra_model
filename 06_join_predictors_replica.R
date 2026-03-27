# Purpose:
# Add Replica predictors to reference points.
# Workflow:
# 1. attach nearest Replica AADT and speed attributes
# 2. create simple Replica-derived features
# 3. write output

source("01_setup.R")

message("Running: 06_join_predictors_replica.R")

# -----------------------------
# Read inputs
# -----------------------------
reference_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_osm_", study_area_name, ".rds"))
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
# Join nearest AADT
# -----------------------------
reference_pts_rep <- add_nearest_edge_attributes(
  pts_sf      = reference_pts,
  edges_sf    = replica_aadt,
  attrs       = c("aadt"),
  edge_id_col = "id",
  max_dist_m  = 10,
  prefix      = "replica_vol",
  keep_dist   = TRUE
)

# -----------------------------
# Join nearest speed attributes
# -----------------------------
reference_pts_rep <- add_nearest_edge_attributes(
  pts_sf      = reference_pts_rep,
  edges_sf    = replica_speed,
  attrs       = c("free_flow_speed_mph", "average_speed_mph", "speed_p95_mph"),
  edge_id_col = "id",
  max_dist_m  = 10,
  prefix      = "replica_spd",
  keep_dist   = TRUE
)

# -----------------------------
# Create Replica composite features
# -----------------------------
reference_pts_rep <- make_replica_composite_features(
  reference_pts_rep,
  aadt_col = "replica_vol_aadt",
  speed_col = "replica_spd_average_speed_mph",
  aadt_thresh = 1500,
  speed_thresh = 25
)

# -----------------------------
# Quick QA
# -----------------------------
message("Replica join complete.")

message("AADT missing:")
print(table(reference_pts_rep$replica_volume_missing, useNA = "ifany"))

message("Speed missing:")
print(table(reference_pts_rep$replica_speed_missing, useNA = "ifany"))

message("Replica missingness summary:")
print(table(reference_pts_rep$replica_missing, useNA = "ifany"))

message("Replica traffic context:")
print(table(reference_pts_rep$replica_traffic_context, useNA = "ifany"))

message("Replica AADT distance summary:")
print(summary(reference_pts_rep$replica_vol_dist_m))

message("Replica speed distance summary:")
print(summary(reference_pts_rep$replica_spd_dist_m))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  reference_pts_rep,
  here::here("data_intermediate", paste0("reference_points_osm_replica_", study_area_name, ".rds"))
)

sf::st_write(
  reference_pts_rep,
  here::here("data_intermediate", paste0("reference_points_osm_replica_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")

# debug
st_write(reference_pts_rep, "C:/working/points_check.gpkg", delete_dsn = T)

reference_pts_rep[which(reference_pts_rep$SegmentID == 1092),]$replica_traffic_context

table(reference_pts_rep$replica_traffic_context, useNA = "ifany")
table(reference_pts_rep$class, reference_pts_rep$replica_traffic_context, useNA = "ifany")
