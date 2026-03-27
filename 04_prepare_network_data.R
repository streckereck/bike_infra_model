# Purpose:
# Prepare network layers for the active study area.
# - read raw OSM and Replica layers
# - transform to project CRS
# - clip to study area
# - create OSM composite features
# - write prepared layers to disk

source("01_setup.R")

message("Running: 04_prepare_network_layers.R")

# -----------------------------
# Read inputs
# -----------------------------
study_area <- sf::st_read(
  here::here("data_intermediate", paste0("study_area_", study_area_name, ".gpkg")),
  quiet = TRUE
)

# -----------------------------
# Read and prepare OSM network
# -----------------------------
osm_raw <- sf::st_read(
  here::here("data_raw", "highways_output_all_sb_2025.gpkg"),
  quiet = TRUE
)

osm <- osm_raw %>%
  sf::st_transform(utm_11) %>%
  sf::st_intersection(sf::st_geometry(study_area)) %>%
  make_osm_composite_features()

message("OSM rows in study area: ", nrow(osm))

# -----------------------------
# Read and prepare Replica AADT
# -----------------------------
replica_aadt_24 <- sf::st_read(
  here::here("data_raw", "aadt_2024_sb.geojson"),
  quiet = TRUE
) %>%
  sf::st_transform(utm_11) %>%
  sf::st_intersection(sf::st_geometry(study_area))

message("Replica AADT rows in study area: ", nrow(replica_aadt_24))

# -----------------------------
# Read and prepare Replica speed
# -----------------------------
replica_avg_speed_24 <- sf::st_read(
  here::here("data_raw", "annual-speeds_2024_sb.geojson"),
  quiet = TRUE
) %>%
  sf::st_transform(utm_11) %>%
  sf::st_intersection(sf::st_geometry(study_area))

message("Replica speed rows in study area: ", nrow(replica_avg_speed_24))

# -----------------------------
# Land use placeholder
# -----------------------------
# landuse <- sf::st_read(
#   here::here("data_raw", "landuse_file.gpkg"),
#   quiet = TRUE
# ) %>%
#   sf::st_transform(utm_11) %>%
#   sf::st_intersection(sf::st_geometry(study_area))
#
# message("Land use rows in study area: ", nrow(landuse))

# -----------------------------
# Write outputs
# -----------------------------
sf::st_write(
  osm,
  here::here("data_intermediate", paste0("osm_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  replica_aadt_24,
  here::here("data_intermediate", paste0("replica_aadt_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  replica_avg_speed_24,
  here::here("data_intermediate", paste0("replica_speed_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

# Uncomment when ready
# sf::st_write(
#   landuse,
#   here::here("data_intermediate", paste0("landuse_", study_area_name, ".gpkg")),
#   delete_dsn = TRUE,
#   quiet = TRUE
# )

message("Saved:")
message("  - osm_", study_area_name, ".gpkg")
message("  - replica_aadt_", study_area_name, ".gpkg")
message("  - replica_speed_", study_area_name, ".gpkg")
message("Done.")