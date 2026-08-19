# Purpose:
# Compare reviewed feedback points against latest network predictions.
# This acts as a regression/QA check as the model is scaled or updated.

source("01_setup.R")

message("Running: 1_evaluate_feedback.R")

feedback_raw <- read_csv(
  here::here("data_intermediate", "feedback.csv"),
  show_col_types = FALSE
) 

feedback_pts <- feedback_raw %>%
  tidyr::extract(
    geometry,
    into = c("x", "y"),
    regex = "c\\(([^,]+),\\s*([^\\)]+)\\)"
  ) %>%
  dplyr::mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = 3857
  ) %>%
  sf::st_transform(utm_11)

feedback_pts_clean <- feedback_pts %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "")
    )
  )

mapview::mapview(feedback_pts_clean)

network_pred <- readRDS(
  here::here("outputs", "models", paste0("network_predictions_rf_", study_area_name, ".rds"))
)

feedback_pts_clean %>%
  count(Expected, sort = TRUE)

feedback_joined <- add_nearest_edge_attributes(
  pts_sf = feedback_pts_clean,
  edges_sf = network_pred %>%
    mutate(
      pred_class_final = as.character(pred_class_final)
    ),
  attrs = c(
    "osm_id",
    "pred_class_final"
  ),
  edge_id_col = "osm_id",
  max_dist_m = 20,
  prefix = "network",
  keep_dist = TRUE
)

mapview::mapview(feedback_joined)

feedback_qa <- feedback_joined %>%
  dplyr::mutate(
    expected_class = as.character(Expected),
    predicted_class = as.character(network_pred_class_final),
    qa_result = dplyr::case_when(
      is.na(expected_class) ~ "no_expected_class",
      is.na(predicted_class) ~ "no_network_match",
      expected_class == predicted_class ~ "match",
      TRUE ~ "mismatch"
    )
  )



# -----------------------------
# Feedback QA map
# -----------------------------

library(mapview)

comment_col <- "Comments"   # change if your field has a different name

feedback_qa_popup <- feedback_qa %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2],
    
    streetview_url = paste0(
      "https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=",
      lat, ",", lon
    ),
    
    osm_url = dplyr::if_else(
      !is.na(network_id),
      paste0("https://www.openstreetmap.org/way/", network_id),
      NA_character_
    ),
    
    popup_text = paste0(
      "<b>Expected:</b> ", .data[["expected_class"]], "<br/>",
      "<b>Predicted:</b> ", .data[["predicted_class"]], "<br/>",
      "<b>Result:</b> ", .data[["qa_result"]], "<br/>",
      "<b>Comment:</b> ", .data[[comment_col]], "<br/><br/>",
      "<a href='", streetview_url, "' target='_blank'>Open Street View</a>",
      "<br/>",
      "<a href='", osm_url, "' target='_blank'>Open OSM Way</a>"
    )
  ) %>%
  sf::st_transform(utm_11)

qa_pal <- c(
  "Match" = "#1b9e77",
  "Mismatch" = "#d95f02",
  "No expected class" = "#7570b3",
  "No network match" = "#bdbdbd"
)

comfort_pal <- c(
  "Bike boulevard" = "#f03b20",
  
  "Bike lane (no buffer)" = "#a6cee3",
  "Bike lane (painted buffer)" = "#1f78b4",
  "Bike lane (physical protection)" = "#8856a7",
  
  "Low-speed/low-volume street" = "#fdbf6f",
  
  "No infrastructure" = "#bdbdbd",
  "Non-conforming infrastructure" ="#ffeda0",
  
  "Paths" = "#33a02c",
  "Signed low-speed/low-volume route" = "#d95f0e",
  
  "Trails (gravel)" = "#a6761d"
  # "Paved shoulder" = "#e78ac3",
)

network_pred_lines <- network_pred %>%
  sf::st_make_valid() %>%
  sf::st_collection_extract("LINESTRING") %>%
  sf::st_cast("MULTILINESTRING", warn = FALSE)

replica_review <- network_pred_lines %>%
  mutate(length_m = st_length(geom) %>% as.numeric()) %>%
  filter(
    length_m < 30,
    replica_vol_aadt > 5000,
    highway %in% c("residential", "living_street")
  )

buffer_high_speed_qa <- network_pred %>%
  dplyr::filter(
    dplyr::coalesce(has_lane, FALSE),
    (
      dplyr::coalesce(has_bike_buffer, FALSE) |
        dplyr::coalesce(has_bike_separation, FALSE)
    ),
    dplyr::coalesce(high_speed_context, FALSE)
  ) %>%
  dplyr::mutate(
    length_m = as.numeric(sf::st_length(geom)),
    osm_link = dplyr::if_else(
      !is.na(osm_id),
      paste0("https://www.openstreetmap.org/way/", osm_id),
      NA_character_
    )
  )

m_network <- mapview::mapview(
  network_pred_lines %>%
    mutate(
      osm_link = dplyr::if_else(
        !is.na(osm_id),
        paste0("https://www.openstreetmap.org/way/", osm_id),
        NA_character_
      ),
    ),
  zcol = "pred_class_final",
  color = comfort_pal,
  lwd = 3,
  layer.name = "Predicted network"
) +
  mapview::mapview(
    buffer_high_speed_qa,
    color = "deeppink",
    lwd = 8,
    layer.name = "QA buffered lane high speed"
  )



m_feedback <- mapview::mapview(
  feedback_qa_popup,
  zcol = "qa_result",
  color = qa_pal,
  cex = 6,
  popup = feedback_qa_popup$popup_text,
  layer.name = "Feedback QA points"
)



# m_feedback_mismatch <- mapview::mapview(
#   feedback_qa_popup %>% dplyr::filter(qa_result == "Mismatch"),
#   zcol = "qa_result",
#   color = qa_pal,
#   cex = 8,
#   popup = feedback_qa_popup$popup_text,
#   layer.name = "Mismatches"
# )

table(feedback_qa_popup$qa_result, useNA = "ifany")

table(
  expected = feedback_qa_popup$expected_class,
  predicted = feedback_qa_popup$predicted_class,
  useNA = "ifany"
)

m_network + m_feedback #+ m_feedback_mismatch

known_segments <- c(
  70360362,    # Sola
  1430942423,  # Las Positas crossing
  899990785    # Concrete bridge path
)


network_pred %>%
  filter(osm_id %in% known_segments) %>%
  select(osm_id, name, pred_class_final)

# investigate buffers
buffered <- network_pred %>%
  filter(pred_class_final == "Bike lane (painted buffer)")

buffered %>%
  st_drop_geometry() %>%
  count(high_speed_context, sort = TRUE)

buffered %>%
  st_drop_geometry() %>%
  count(
    highway,
    high_speed_context
  ) %>%
  tidyr::pivot_wider(
    names_from = high_speed_context,
    values_from = n,
    values_fill = 0,
    names_prefix = "high_speed_"
  )

buffered %>%
  st_drop_geometry() %>%
  count(maxspeed_mph, sort = TRUE)

buffered %>%
  st_drop_geometry() %>%
  count(
    highway,
    high_speed_context,
    maxspeed_mph
  ) %>%
  tidyr::pivot_wider(
    names_from = high_speed_context,
    values_from = n,
    values_fill = 0,
    names_prefix = "high_speed_"
  )

# missing speed
buffered %>%
  st_drop_geometry() %>%
  summarise(
    n = n(),
    known_speed = sum(!is.na(maxspeed_mph)),
    high_speed = sum(high_speed_context %in% TRUE, na.rm = TRUE),
    missing_speed = sum(is.na(maxspeed_mph)),
    pct_speed_known = mean(!is.na(maxspeed_mph))
  )

# compare with replica
buffered <- buffered %>%
  mutate(
    posted_speed_group = case_when(
      high_speed_context %in% TRUE ~ ">35 mph",
      high_speed_context %in% FALSE ~ "<=35 mph",
      TRUE ~ "Missing"
    ),
    replica_vol_valid =
      !is.na(replica_vol_aadt),
    
    replica_spd_valid =
      !is.na(replica_spd_average_speed_mph),
    
    # Keep only valid matched values
    aadt_valid = dplyr::if_else(
      replica_vol_valid,
      as.numeric(replica_vol_aadt),
      NA_real_
    ),
    
    speed_valid = dplyr::if_else(
      replica_spd_valid,
      as.numeric(replica_spd_average_speed_mph),
      NA_real_
    ),
    
    replica_volume_missing = is.na(aadt_valid),
    replica_speed_missing  = is.na(speed_valid)) 

buffered %>%
  ggplot(
    aes(
      x = posted_speed_group,
      y = speed_valid
    )
  ) +
  geom_boxplot() +
  labs(
    x = "OSM posted speed",
    y = "Replica average speed (mph)"
  )

buffered %>%
  st_drop_geometry() %>%
  mutate(
    posted_speed_group = case_when(
      maxspeed_mph > 35 ~ ">35",
      !is.na(maxspeed_mph) ~ "<=35",
      TRUE ~ "missing"
    ),
    replica_speed_group = case_when(
      is.na(speed_valid) ~ "missing",
      speed_valid <= 25 ~ "<=25",
      speed_valid <= 35 ~ "25-35",
      TRUE ~ ">35"
    )
  ) %>%
  count(
    highway,
    posted_speed_group,
    replica_speed_group
  )

buffered %>%
  filter(highway == "secondary") %>%
  st_drop_geometry() %>%
  select(
    name,
    maxspeed_mph,
    speed_valid,
    aadt_valid,
    replica_traffic_context
  ) %>%
  arrange(maxspeed_mph, desc(speed_valid))

buffered %>%
  st_drop_geometry() %>%
  mutate(
    posted_speed_group = case_when(
      maxspeed_mph > 35 ~ ">35",
      !is.na(maxspeed_mph) ~ "<=35",
      TRUE ~ "missing"
    )
  ) %>%
  count(
    posted_speed_group,
    replica_traffic_context
  )

buffered %>%
  st_drop_geometry() %>%
  mutate(
    aadt_group = case_when(
      is.na(aadt_valid) ~ "missing",
      aadt_valid <= 2500 ~ "<=2,500",
      aadt_valid <= 6000 ~ "2,501-6,000",
      TRUE ~ ">6,000"
    )
  ) %>%
  count(
    posted_speed_group,
    aadt_group
  )
