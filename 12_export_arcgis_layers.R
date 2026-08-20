# -----------------------------
# Purpose:
# Prepare clean layers for ArcGIS Online upload
# -----------------------------

source("01_setup.R")

message("Running: 12_export_arcgis_layers.R")

# -----------------------------
# Read inputs
# -----------------------------
network_pred <- readRDS(
  here::here("outputs", "models", paste0("network_predictions_rf_", study_area_name, ".rds"))
)

reference_context <- readRDS(
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".rds"))
)

# -----------------------------
# Clean + relabel network
# -----------------------------
network_export <- network_pred %>%
  dplyr::mutate(
    class_export = as.character(pred_class_final),
    
    comfort_class = dplyr::case_when(
      class_export %in% c(
        "Paths",
        "Bike lane (physical protection)"
      ) ~ "High comfort",
      
      class_export %in% c(
        "Bike lane (painted buffer)",
        "Bike boulevard"
      ) ~ "Medium comfort",
      
      class_export %in% c(
        "Bike lane (painted buffer: high-speed street)",
        "Bike lane (no buffer)",
        "Signed low-speed/low-volume route",
        "Low-speed/low-volume street",
        "Trails (gravel)"
      ) ~ "Low comfort",
      
      class_export == "Non-conforming infrastructure" ~ "Non-conforming infrastructure",
      TRUE ~ "No infrastructure"
    ),
    
    class_export = factor(
      class_export,
      levels = c(
        "Paths",
        "Bike lane (physical protection)",
        "Bike lane (painted buffer)",
        "Bike boulevard",
        "Signed low-speed/low-volume route",
        "Bike lane (painted buffer: high-speed street)",
        "Bike lane (no buffer)",
        "Low-speed/low-volume street",
        "Trails (gravel)",
        "Non-conforming infrastructure",
        "No infrastructure"
      )
    ),
    
    comfort_class = factor(
      comfort_class,
      levels = c(
        "High comfort",
        "Medium comfort",
        "Low comfort",
        "Non-conforming",
        "Check classification"
      )
    )
  ) %>%
  dplyr::select(
    osm_id,
    name,
    class_export,
    comfort_class,
    
    road_class,
    highway,
    surface_class,
    replica_traffic_context,
    replica_vol_aadt,
    replica_spd_average_speed_mph,
    replica_missing,
    #high_speed_source,
    
    geom
  )

missing_classes <- setdiff(
  unique(as.character(network_pred$pred_class_final)),
  class_lookup$class_export
)

if (length(missing_classes) > 0) {
  stop("Missing classes in class_lookup: ", paste(missing_classes, collapse = ", "))
}

# -----------------------------
# Clean reference points
# -----------------------------
reference_export <- reference_context %>%
  dplyr::mutate(
    class_export = dplyr::case_when(
      class == "Road, no infrastructure" ~ "Non-conforming",
      TRUE ~ as.character(class)),
    class_export = factor(
      class_export,
      levels = c(
        "Paths",
        "Bike lane (buffer)",
        "Bike lane (no buffer)",
        "Non-conforming"))
    ) %>%
  dplyr::select(
    SegmentID,
    osm_id,
    class_export,
    #rating,
    
    # context
    # strava_total_trip_count,
    # incident_count_250m,
    
    geom
  )

# -----------------------------
# Write GeoPackage
# -----------------------------
export_path <- here::here(
  "outputs", "gis",
  paste0("arcgis_export_", study_area_name, "_19Aug2026", ".gpkg")
)

if (file.exists(export_path)) file.remove(export_path)

sf::st_write(
  network_export,
  export_path,
  layer = "network",
  quiet = TRUE
)

sf::st_write(
  reference_export,
  export_path,
  layer = "reference_points",
  append = TRUE,
  quiet = TRUE
)

message("Export complete: ", export_path)

# -----------------------------
# Write Geodatabase
# -----------------------------

sf::st_write(
  network_export,
  dsn = here::here("outputs", "gis", "replica_county_18June2026.gdb"),
  layer = "bike_network",
  driver = "OpenFileGDB",
  delete_dsn = TRUE
)

# ref points for yuyan
reference_context_yuyan <- reference_context %>%
  st_transform(4326) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

st_write(
  reference_context_yuyan,
  dsn = here::here("outputs", "gis", "reference_points.shp"),

)

write_csv(reference_context_yuyan,
          "outputs/gis/reference_points.csv")
