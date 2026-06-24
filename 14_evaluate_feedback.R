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
