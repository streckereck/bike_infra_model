# Purpose:
# Quick visual QA for baseline network predictions.
# - read predicted network
# - read enriched reference points / split table
# - optionally read test-set predictions
# - create mapview layers for rapid inspection

source("01_setup.R")

message("Running: 11_review_network_predictions.R")

library(mapview)

# -----------------------------
# Read inputs
# -----------------------------
study_area <- sf::st_read(
  here::here("data_intermediate", paste0("study_area_", study_area_name, ".gpkg")),
  quiet = TRUE
)

network_pred <- readRDS(
  here::here("outputs", "models", paste0("network_predictions_rf_", study_area_name, ".rds"))
)

reference_pts <- readRDS(
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".rds"))
)

pred_tbl_path <- here::here(
  "outputs", "tables", paste0("rf_predictions_", study_area_name, ".csv")
)

has_pred_tbl <- file.exists(pred_tbl_path)

if (has_pred_tbl) {
  pred_tbl <- readr::read_csv(pred_tbl_path, show_col_types = FALSE)
  
  reference_pts <- reference_pts %>%
    dplyr::left_join(
      pred_tbl %>% dplyr::select(SegmentID, pred_class_final),
      by = "SegmentID"
    ) %>%
    dplyr::mutate(
      match_flag = dplyr::case_when(
        split != "test" ~ "train_point",
        is.na(pred_class_final) ~ "no_prediction",
        class == pred_class_final ~ "match",
        TRUE ~ "mismatch"
      )
    )
} else {
  message("Prediction table not found; mismatch layer will not be created.")
}

# -----------------------------
# Palettes
# -----------------------------
# -----------------------------
# Shared comfort palette
# -----------------------------
comfort_pal <- c(
  "Paths" = "#33a02c",
  "Bike lane (physical protection)" = "#8856a7",
  "Bike lane (painted buffer)" = "#1f78b4",
  "Bike lane (no buffer)" = "#a6cee3",
  # "Paved shoulder" = "#e78ac3",
  "Bike boulevard" = "#f03b20",
  "Signed low-speed/low-volume route" = "#d95f0e",
  "Low-speed/low-volume street" = "#fdbf6f",
  "No infrastructure" = "#bdbdbd",
  "Non-conforming infrastructure" = "#ffeda0",
  "Trails (gravel)" = "#a6761d"
)

match_pal <- c(
  "match" = "#1a9850",
  "mismatch" = "#d73027"#,
  # "train_point" = "#7570b3",
  # "no_prediction" = "#bdbdbd"
)

# -----------------------------
# Build map layers
# -----------------------------

class_levels <- names(comfort_pal)

network_pred <- network_pred %>%
  dplyr::mutate(
    pred_class_final = factor(pred_class_final, levels = class_levels)
  )

reference_pts <- reference_pts %>%
  dplyr::mutate(
    class = translate_reference_class(class),
    class = factor(class, levels = class_levels)
  )

reference_pts_popup <- reference_pts %>%
  dplyr::mutate(
    match_class = dplyr::case_when(
      split != "test" ~ "train_point",
      is.na(pred_class_final) ~ "no_prediction",
      class == pred_class_final ~ "match",
      TRUE ~ "mismatch"
    )
  ) %>%
  dplyr::select(
    SegmentID,
    osm_id,
    match_class,
    observed = class,
    predicted = pred_class_final,
    source = source_wave,
    osm_match_dist = osm_dist_m
    ) %>%
  mutate(
    observed = factor(observed, levels = names(comfort_pal))
  ) %>%
  st_transform(4326) %>%
  dplyr::mutate(
    lon = sf::st_coordinates(geom)[,1],
    lat = sf::st_coordinates(geom)[,2],
    streetview_link = paste0(
      '<a href="https://www.google.com/maps?q=&layer=c&cbll=',
      lat, ',', lon,
      '" target="_blank">Open Street View</a>'
    )
  )

reference_check <- reference_pts_popup %>%
  dplyr::mutate(
    observed_eval = collapse_to_reference_class(observed),
    predicted_eval = collapse_to_reference_class(predicted),
    match_eval = observed_eval == predicted_eval
  )

network_pred_lines <- network_pred %>%
  sf::st_make_valid() %>%
  sf::st_collection_extract("LINESTRING") %>%
  sf::st_cast("MULTILINESTRING", warn = FALSE)

network_pred_popup <- network_pred_lines %>%
  mutate(source = case_when(
    as.character(pred_class) == as.character(pred_class_final) ~ "model",
    T ~ "postprocessing"
  )) %>%
  dplyr::select(
    osm_id,
    predicted = pred_class_final,
    source,
    road_class,
    highway,
    has_lane,
    has_track,
    has_bike_buffer,
    bike_allowed_explicit,
    is_crossing,
    # has_bike_separation,
    surface_class,
    replica_traffic_context,
    replica_vol_aadt,
    replica_spd_average_speed_mph
  ) %>%
  dplyr::mutate(
    predicted = factor(predicted, levels = names(comfort_pal))
  )

m_boundary <- mapview::mapview(
  sf::st_boundary(study_area),
  color = "grey60",
  lwd = 2,
  alpha = 0.8,
  layer.name = "Study area"
)

pal_ordered <- unname(comfort_pal[class_levels])

m_network <- mapview::mapview(
  #network_pred_popup %>% filter(! predicted %in% "Road, no infrastructure"),
  network_pred_popup,
  zcol = "predicted",
  color = pal_ordered,
  lwd = 4,
  layer.name = "Predicted network"
)

m_reference <- mapview::mapview(
  reference_check,
  zcol = "observed",
  col.regions = comfort_pal,
  cex = 4,
  alpha.regions = 0.8,
  layer.name = "Observed training points"
)

m_mismatch <- mapview::mapview(
  reference_pts_popup %>% filter(! match_class %in% c("train_point")),
  zcol = "match_class",
  col.regions = match_pal,
  cex = 5,
  alpha.regions = 0.9,
  layer.name = "Test point mismatch"
)

pts_match <- reference_pts_popup %>% dplyr::filter(match_class == "match")
pts_mismatch <- reference_pts_popup %>% dplyr::filter(match_class == "mismatch")

m_match <- mapview(pts_match,
                   col.regions = "green",
                   pch = 16,
                   cex = 5,
                   alpha = 0.75,
                   layer.name = "Match")
if(nrow(pts_mismatch) >0){
  m_mismatch <- mapview(pts_mismatch,
                      col.regions = "darkgrey",
                      pch = 4,
                      cex = 8,
                      alpha = 0.75,
                      layer.name = "Mismatch")

}

# Test-point mismatch layer if available
# if (has_pred_tbl) {
#   m_mismatch <- mapview::mapview(
#     reference_pts,
#     zcol = "match_flag",
#     col.regions = match_pal,
#     cex = 5,
#     alpha.regions = 0.9,
#     layer.name = "Test-point mismatch"
#   )
# }

# -----------------------------
# Quick summaries
# -----------------------------
message("Predicted network class distribution:")
print(table(network_pred$pred_class_final, useNA = "ifany"))

message("Observed reference class distribution:")
print(table(reference_pts$class, useNA = "ifany"))

if (has_pred_tbl) {
  message("Test-point match / mismatch summary:")
  print(table(reference_pts$match_flag, useNA = "ifany"))
}

network_pred %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    pred_class_model %in% c(
      "Bike lane (no buffer)",
      "Bike lane (painted buffer)",
      "Bike lane (physical protection)"
    ),
    !has_bike_infra_signal
  ) %>%
  dplyr::count(pred_class_model, pred_class_final)

# -----------------------------
# Return map objects
# -----------------------------
# Main review map
if (has_pred_tbl) {
  review_map <- m_network + m_reference + m_match + m_mismatch + m_boundary  
} else {
  review_map <- m_network + m_reference + m_boundary
}

network_pred %>%
  st_drop_geometry() %>%
  count(pred_class_final, sort = TRUE)

review_map


