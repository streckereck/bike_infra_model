# -----------------------------
# Purpose:
# Prepare clean layers for ArcGIS Online upload
# -----------------------------

source("01_setup.R")

message("Running: 21_cv_sample.R")

# -----------------------------
# Read inputs
# -----------------------------
network_pred <- readRDS(
  here::here("outputs", "models", paste0("network_predictions_rf_", study_area_name, ".rds"))
)

osm_dir <- here::here("data_raw", "osm")

osm <- sf::st_read(
  osm_gpkg_path <- file.path(osm_dir, "socal-latest.gpkg"),
  quiet = TRUE
)

network <- network_pred %>%
  left_join(osm %>% st_drop_geometry(), by = "osm_id")

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

write_csv(reference_context_yuyan,
          "outputs/gis/reference_points.csv")

#-----------------------------------------------------------------------------
# take a random sample of 
# 250 painted lane
# 250 buffered lane
# 250 shoulder

# shoulders: shoulder=yes and cycleway=shoulder
# lanes: cycleway=lane or cycleway:right=lane

# What shoulder-related fields actually made it through?
grep(
  "shoulder|cycleway",
  names(osm),
  value = TRUE
)

osm %>%
  st_drop_geometry() %>%
  count(shoulder, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(shoulder_left, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(shoulder_right, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(shoulder_both, sort = TRUE)

# now shoulders in cycleways

osm %>%
  st_drop_geometry() %>%
  count(cycleway, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(cycleway_left, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(cycleway_right, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  count(cycleway_both, sort = TRUE)

osm %>%
  st_drop_geometry() %>%
  select(
    osm_id,
    name,
    highway,
    surface,
    starts_with("shoulder"),
    starts_with("cycleway")
  ) %>%
  pivot_longer(
    cols = c(starts_with("shoulder"), starts_with("cycleway")),
    names_to = "tag",
    values_to = "value"
  ) %>%
  filter(
    !is.na(value),
    str_detect(str_to_lower(value), "shoulder")
  ) %>%
  count(tag, value, sort = TRUE)

shoulder_tagged <- osm %>%
  mutate(
    has_shoulder_tag =
      shoulder %in% c("yes", "both") |
      shoulder_left == "yes" |
      shoulder_right == "yes" |
      shoulder_both == "yes",
    
    has_cycleway_shoulder =
      cycleway == "shoulder" |
      cycleway_left == "shoulder" |
      cycleway_right == "shoulder" |
      cycleway_both == "shoulder",
    
    has_any_shoulder =
      has_shoulder_tag | has_cycleway_shoulder
  )

shoulder_tagged %>%
  st_drop_geometry() %>%
  filter(has_any_shoulder) %>%
  count(
    highway,
    has_shoulder_tag,
    has_cycleway_shoulder,
    sort = TRUE
  )

# candidates
shoulder_candidates <- shoulder_tagged %>%
  filter(
    has_any_shoulder,
    highway %in% c(
      "primary",
      "secondary",
      "tertiary",
      "unclassified"
    )
  ) %>%
  select(
    osm_id,
    name,
    highway,
    surface,
    shoulder,
    shoulder_left,
    shoulder_right,
    shoulder_both,
    cycleway,
    cycleway_left,
    cycleway_right,
    cycleway_both,
    has_shoulder_tag,
    has_cycleway_shoulder
  )

mapview::mapview(
  shoulder_candidates,
  zcol = "highway",
  lwd = 4
)
 # samples

painted_pool <- network_pred %>%
  filter(pred_class_final == "Bike lane (no buffer)")

buffered_pool <- network_pred %>%
  filter(pred_class_final == "Bike lane (painted buffer)")

shoulder_pool <- osm %>%
  mutate(
    has_shoulder =
      shoulder %in% c("yes", "both", "left", "right") |
      cycleway == "shoulder" |
      cycleway_left == "shoulder" |
      cycleway_right == "shoulder" |
      cycleway_both == "shoulder"
  ) %>%
  filter(has_shoulder)

sample_network_points <- function(
    pool,
    class_label = NULL,
    n = 250,
    spacing_m = 500,
    seed = 123
) {
  
  set.seed(seed)
  
  lines <- pool %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("LINESTRING") %>%
    sf::st_cast("LINESTRING", warn = FALSE)
  
  candidate_pts <- lines %>%
    sf::st_geometry() %>%
    sf::st_line_sample(density = 1 / spacing_m) %>%
    sf::st_cast("POINT") %>%
    sf::st_as_sf()
  
  if (nrow(candidate_pts) == 0) {
    return(candidate_pts)
  }
  
  sample_pts <- candidate_pts %>%
    dplyr::slice_sample(
      n = min(n, nrow(candidate_pts))
    )
  
  attrs <- pool %>%
    dplyr::select(
      osm_id,
      name,
      highway,
      pred_class_final
    )
  
  if (!is.null(class_label)) {
    attrs <- attrs %>%
      dplyr::mutate(
        pred_class_final = class_label
      )
  }
  
  sample_pts %>%
    sf::st_join(
      attrs,
      join = sf::st_nearest_feature
    ) %>%
    dplyr::filter(!is.na(osm_id))
}

painted_sample <- sample_network_points(
  painted_pool,
  n = 250,
  spacing_m = 100,
  seed = 123
)

buffered_sample <- sample_network_points(
  buffered_pool,
  n = 250,
  spacing_m = 100,
  seed = 456
)

shoulder_sample <- sample_network_points(
  shoulder_pool %>%
    mutate(
      pred_class_final = "Paved shoulder"
    ),
  class_label = "Paved shoulder",
  n = 250,
  spacing_m = 100,
  seed = 789
)


samples <- rbind(
  painted_sample,
  buffered_sample,
  shoulder_sample
)

samples <- samples %>%
  dplyr::group_by(pred_class_final) %>%
  dplyr::mutate(
    sample_id = paste0(
      dplyr::case_when(
        pred_class_final == "Bike lane (no buffer)" ~ "PAINT",
        pred_class_final == "Bike lane (painted buffer)" ~ "BUFFER",
        pred_class_final == "Paved shoulder" ~ "SHOULDER"
      ),
      "_",
      sprintf("%03d", dplyr::row_number())
    )
  ) %>%
  dplyr::ungroup()

samples %>%
  sf::st_drop_geometry() %>%
  dplyr::count(pred_class_final)

table(sf::st_geometry_type(samples))

samples_ll <- samples %>%
  sf::st_transform(4326) 

coords <- sf::st_coordinates(samples_ll)

samples_ll <- samples_ll %>%
  dplyr::mutate(
    lon = coords[, "X"],
    lat = coords[, "Y"],
    streetview_link = paste0(
      "https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=",
      lat, ",", lon
    )
  )

samples_ll %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    lat,
    lon,
    streetview_link,
    dplyr::everything()
  ) %>%
  head()

samples_export <- samples_ll %>%
  sf::st_drop_geometry()

readr::write_csv(
  samples_export,
  "outputs/extra_samples.csv"
)

samples %>%
  sf::st_drop_geometry() %>%
  dplyr::count(pred_class_final)

table(sf::st_geometry_type(samples))
