# Purpose:
# Attach enriched network predictors to labelled reference points.
# - read cleaned reference points
# - attach nearest osm_id and match distance
# - join predictor fields from enriched network by osm_id
# - write enriched reference points to disk

source("01_setup.R")

message("Running: 06_attach_network_predictors_to_points.R")

# -----------------------------
# Settings
# -----------------------------
osm_match_dist_m <- 10

# -----------------------------
# Read inputs
# -----------------------------
reference_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_", study_area_name, ".rds"))
)

network_enriched <- readRDS(
  here::here("data_intermediate", paste0("network_enriched_", study_area_name, ".rds"))
)

# -----------------------------
# Basic checks
# -----------------------------
if (!"osm_id" %in% names(network_enriched)) {
  stop("network_enriched does not contain 'osm_id'.")
}

# -----------------------------
# Attach nearest osm_id to reference points
# -----------------------------
reference_pts_enriched <- add_nearest_edge_attributes(
  pts_sf      = reference_pts,
  edges_sf    = network_enriched,
  attrs       = c("osm_id"),
  edge_id_col = "osm_id",
  max_dist_m  = osm_match_dist_m,
  prefix      = "osm",
  keep_dist   = TRUE,
  id_name     = "osm_id"
)

# -----------------------------
# Build lookup table of predictors from enriched network
# -----------------------------
network_lookup <- network_enriched %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    osm_id,
    dplyr::all_of(unique(c(model_predictors, postprocess_cols)))
  )

# -----------------------------
# Join predictors to points by osm_id
# -----------------------------
reference_pts_enriched <- reference_pts_enriched %>%
  dplyr::left_join(network_lookup, by = "osm_id")

# -----------------------------
# Quick QA
# -----------------------------
message("Rows in enriched reference points: ", nrow(reference_pts_enriched))
message("Matched osm_id: ", sum(!is.na(reference_pts_enriched$osm_id)), " / ", nrow(reference_pts_enriched))

message("OSM match distance summary:")
print(summary(reference_pts_enriched$osm_dist_m))

message("Outcome distribution:")
print(table(reference_pts_enriched$class, useNA = "ifany"))

message("Replica traffic context by class:")
if ("replica_traffic_context" %in% names(reference_pts_enriched)) {
  print(table(reference_pts_enriched$class,
              reference_pts_enriched$replica_traffic_context,
              useNA = "ifany"))
}

message("Check for missing predictors:")
missing_predictors <- setdiff(model_predictors, names(network_enriched))

if (length(missing_predictors) > 0) {
  stop("Missing predictors in network_enriched: ",
       paste(missing_predictors, collapse = ", "))
} else {
  print(paste0("All ", 
               length(model_predictors),
               " predictors in network_enriched."))
}

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  reference_pts_enriched,
  here::here("data_intermediate", paste0("reference_points_enriched_", study_area_name, ".rds"))
)

sf::st_write(
  reference_pts_enriched,
  here::here("data_intermediate", paste0("reference_points_enriched_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")
