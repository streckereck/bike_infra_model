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
# Define paths
# -----------------------------

osm_dir <- here::here("data_raw", "osm")
dir.create(osm_dir, recursive = TRUE, showWarnings = FALSE)

osm_pbf_path <- file.path(osm_dir, "socal-latest.osm.pbf")
osm_gpkg_path <- file.path(osm_dir, "socal-latest.gpkg")

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
if (download_osm || !file.exists(osm_gpkg_path)) {
  
  message("Downloading latest Southern California OSM extract...")
  
  # download.file(
  #   url = "https://download.geofabrik.de/north-america/us/california/socal-latest.osm.pbf",
  #   destfile = osm_pbf_path,
  #   mode = "wb"
  # )
  
  tmp_file <- paste0(osm_pbf_path, ".download")
  
  # download.file(
  #   url = "https://download.geofabrik.de/north-america/us/california/socal-latest.osm.pbf",
  #   destfile = tmp_file,
  #   mode = "wb"
  # )
  
  curl::curl_download(
    url = "https://download.geofabrik.de/north-america/us/california/socal-latest.osm.pbf",
    destfile = tmp_file,
    quiet = FALSE
  )
  
  file.rename(tmp_file, osm_pbf_path)
  }

if (refresh_osm_layer || download_osm){
  
  osm_pbf_path
  file.exists(osm_pbf_path)
  normalizePath(osm_pbf_path, mustWork = FALSE)
  
  osm_raw <- osmextract::oe_read(
    file_path = osm_pbf_path,
    layer = "lines",
    extra_tags = c(
      "bicycle", "bridge", "cycleway", "cycleway:left", "cycleway:right",
      "cycleway:both", "foot", "footway", "hgv", "horse", "junction",
      "lanes", "lanes:backward", "lanes:forward", 
      
      # crossings
      "crossing",
      "crossing:island",
      "crossing:markings",
      "crossing:signals",
      "crossing:traffic:signals",
      "crossing:traffic_signals",
      
      # cycleway buffer / separation
      "cycleway:buffer",
      "cycleway:left:buffer",
      "cycleway:right:buffer",
      "cycleway:both:buffer",
      "cycleway:separation",
      "cycleway:left:separation",
      "cycleway:right:separation",
      "cycleway:both:separation",
      
      # cycle networks
      "lcn", "rcn", "ncn",
      "lcn_ref", "rcn_ref", "ncn_ref",
      "lcn_name", "rcn_name", "ncn_name",
      
      "lit", "maxspeed", "motor:vehicle", "oneway", "oneway:bicycle",
      "path", "railway", "segregated", "sidewalk", "sidewalk:both:surface",
      "surface", "traffic_calming", "width", "access", "natural"
    ),
    quiet = TRUE
  )
  
  osm_raw %>%
    sf::st_drop_geometry() %>%
    dplyr::summarise(
      traffic_calming_n = sum(!is.na(traffic_calming)),
      motor_vehicle_n = sum(!is.na(motor_vehicle)),
      lcn_ref_n = sum(!is.na(lcn_ref)),
      rcn_ref_n = sum(!is.na(rcn_ref)),
      ncn_ref_n = sum(!is.na(ncn_ref))
    )
  
  osm <- osm_raw %>%
    sf::st_transform(utm_11) %>%
    dplyr::filter(
      highway %in% c(
        "motorway", "trunk", "primary", "secondary", "tertiary",
        "unclassified", "residential", "living_street", "service",
        "track", "path", "cycleway", "footway", "pedestrian"
      )
    ) %>%
    sf::st_intersection(sf::st_geometry(study_area))
  
  sf::st_write(
    osm,
    osm_gpkg_path,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  writeLines(as.character(Sys.Date()),
             here::here("data_raw", "osm", "osm_download_date.txt"))
  
} else {
  
  message("Using existing OSM data...")
  
  osm <- sf::st_read(osm_gpkg_path, quiet = TRUE)
  
}

# -----------------------------
# Read and prepare Replica
# -----------------------------

# manual download of AADT and free flow speed from replicahq as geojson
# rename to match other files (sbcc to sb)

if (!file.exists(replica_aadt_file)) {
  stop("Replica AADT file not found: ", replica_aadt_file)
}

if (!file.exists(replica_speed_file)) {
  stop("Replica speed file not found: ", replica_speed_file)
}

replica_aadt <- sf::st_read(replica_aadt_file, quiet = TRUE) %>%
  sf::st_transform(utm_11) %>%
  sf::st_intersection(sf::st_geometry(study_area))

replica_speed <- sf::st_read(replica_speed_file, quiet = TRUE) %>%
  sf::st_transform(utm_11) %>%
  sf::st_intersection(sf::st_geometry(study_area))

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
  replica_aadt,
  here::here("data_intermediate", paste0("replica_aadt_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  replica_speed,
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
