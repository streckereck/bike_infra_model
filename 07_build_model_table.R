# Purpose:
# Build a clean modelling table from enriched reference points.
# - define modelling outcome
# - keep IDs, metadata, and model predictors
# - apply light type cleaning
# - save model-ready table

source("01_setup.R")

message("Running: 07_build_model_table.R")

# -----------------------------
# Read inputs
# -----------------------------
reference_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_enriched_", study_area_name, ".rds"))
)

# -----------------------------
# Basic checks
# -----------------------------
required_cols <- c("SegmentID", "class", "block_id", "osm_id")
missing_cols <- setdiff(required_cols, names(reference_pts))

if (length(missing_cols) > 0) {
  stop("Missing required columns in enriched reference points: ",
       paste(missing_cols, collapse = ", "))
}

missing_predictors <- setdiff(model_predictors, names(reference_pts))
if (length(missing_predictors) > 0) {
  stop("Missing predictors in enriched reference points: ",
       paste(missing_predictors, collapse = ", "))
}

# -----------------------------
# Define modelling outcome
# -----------------------------
model_vars <- unique(c(model_predictors, postprocess_cols))

model_table <- reference_pts %>%
  dplyr::mutate(
    class_raw = class,
    
    # Temporary prototype-specific class collapse
    class = dplyr::case_when(
      study_area_name == "city_santa_barbara" &
        class_raw == "Connecting gravel path" ~ "Trail",
      
      study_area_name == "city_santa_barbara_north" &
        class_raw == "Connecting gravel path" ~ "Trail",
      
        class_raw == "Paved shoulder" ~ "Road, no infrastructure",
      
      TRUE ~ class_raw
    )
  ) %>%
  dplyr::select(
    # IDs / metadata
    dplyr::all_of(model_id_cols),
    
    # outcome
    class,
    class_raw,
    
    dplyr::all_of(model_vars)

  )

# -----------------------------
# Light type cleaning
# -----------------------------
model_table <- model_table %>%
  dplyr::mutate(
    class = factor(class),
    class_raw = factor(class_raw),
    
    source_wave = factor(source_wave),
    stratum = factor(stratum),
    reviewer = factor(reviewer),
    block_id = as.character(block_id),
    
    road_class = if ("road_class" %in% names(.)) factor(road_class) else NULL,
    highway = if ("highway" %in% names(.)) factor(highway) else NULL,
    surface_class = if ("surface_class" %in% names(.)) factor(surface_class) else NULL,
    replica_missing = if ("replica_missing" %in% names(.)) factor(replica_missing) else NULL,
    replica_traffic_context = if ("replica_traffic_context" %in% names(.)) factor(replica_traffic_context) else NULL,
    
    has_any_cycleway = if ("has_any_cycleway" %in% names(.)) as.logical(has_any_cycleway) else NULL,
    has_lane = if ("has_lane" %in% names(.)) as.logical(has_lane) else NULL,
    has_track = if ("has_track" %in% names(.)) as.logical(has_track) else NULL,
    is_unpaved = if ("is_unpaved" %in% names(.)) as.logical(is_unpaved) else NULL,
    is_bridge = if ("is_bridge" %in% names(.)) as.logical(is_bridge) else NULL,
    is_oneway = if ("is_oneway" %in% names(.)) as.logical(is_oneway) else NULL,
    bike_route_designated = if ("bike_route_designated" %in% names(.)) as.logical(bike_route_designated) else NULL,
    replica_volume_missing = if ("replica_volume_missing" %in% names(.)) as.logical(replica_volume_missing) else NULL,
    replica_speed_missing = if ("replica_speed_missing" %in% names(.)) as.logical(replica_speed_missing) else NULL,
    replica_low_stress = if ("replica_low_stress" %in% names(.)) as.logical(replica_low_stress) else NULL,
    
    lanes = if ("lanes" %in% names(.)) suppressWarnings(as.numeric(lanes)) else NULL,
    maxspeed = if ("maxspeed" %in% names(.)) suppressWarnings(as.numeric(maxspeed)) else NULL
  )

# -----------------------------
# Quick QA
# -----------------------------
message("Rows in model table: ", nrow(model_table))

message("Outcome distribution:")
print(table(model_table$class, useNA = "ifany"))

message("Original class distribution:")
print(table(model_table$class_raw, useNA = "ifany"))

message("Blocks represented: ", dplyr::n_distinct(model_table$block_id))

message("Missingness summary (non-geometry):")
print(colSums(is.na(sf::st_drop_geometry(model_table))))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  model_table,
  here::here("data_intermediate", paste0("model_table_", study_area_name, ".rds"))
)

readr::write_csv(
  sf::st_drop_geometry(model_table),
  here::here("data_intermediate", paste0("model_table_", study_area_name, ".csv"))
)

message("Done.")