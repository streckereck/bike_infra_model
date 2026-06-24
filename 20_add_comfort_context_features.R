# Purpose:
# Add county-wide comfort/context predictors to the enriched network and reference points.
# This is separate from the infrastructure classification pipeline.
# Current features:
# - Strava Metro counts
# - incident KDE / safety context

source("01_setup.R")

message("Running: 20_prepare_comfort_context.R")

# -----------------------------
# Safety check
# -----------------------------
if (study_area_name != "county") {
  warning("This script is intended for county-wide context. Current study_area_name = ", study_area_name)
}

# -----------------------------
# Read inputs
# -----------------------------
network_enriched <- readRDS(
  here::here("data_intermediate", paste0("network_enriched_", study_area_name, ".rds"))
)

reference_pts <- readRDS(
  here::here("data_intermediate", paste0("reference_points_enriched_", study_area_name, ".rds"))
)

# -----------------------------
# Read Strava Metro counts
# -----------------------------
strava_counts <- readr::read_csv(
  here::here(
    "data_raw", "strava",
    "b71d5522f92b8b394ffbb3dc3ccf6c3f5bf644a45b22193ec6905edbb24e0ed7-1777330617454.csv"
  ),
  show_col_types = FALSE
)

# -----------------------------
# Read Strava edge geometry
# -----------------------------
strava_edges <- sf::st_read(
  here::here("data_raw", "strava", "b71d5522f92b8b394ffbb3dc3ccf6c3f5bf644a45b22193ec6905edbb24e0ed7-1777330617454.shp"),
  quiet = TRUE
) %>%
  sf::st_transform(utm_11)

# -----------------------------
# Join counts to geometry
# -----------------------------
strava_counts <- strava_counts %>%
  rename(edgeUID = edge_uid) %>%
  dplyr::mutate(edgeUID = as.character(edgeUID))

strava_edges <- strava_edges %>%
  dplyr::mutate(edgeUID = as.character(edgeUID)) %>%
  dplyr::left_join(strava_counts, by = "edgeUID") %>%
  mutate(
    total_commute_pct =
      (
        forward_commute_trip_count +
          reverse_commute_trip_count
      ) /
      total_trip_count,
    
    pct_wmn =
      (
        forward_female_people_count +
          reverse_female_people_count
      ) /
      (
        forward_people_count +
          reverse_people_count
      )
  )


message("Strava edges: ", nrow(strava_edges))
message("Strava counts rows: ", nrow(strava_counts))

message("Rows with joined counts:")
print(sum(!is.na(strava_edges[[setdiff(names(strava_counts), "edgeUID")[1]]])))

# incidents should be a point layer
# -----------------------------
# Read crash data (CSV → points)
# -----------------------------
crashes_raw <- readr::read_csv(
  here::here("data_raw", "incidents", "Crashes.csv"),
  show_col_types = FALSE
)

# basic QA
crashes_raw <- crashes_raw %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE)
  )

# convert to sf (WGS84)
incidents <- crashes_raw %>%
  sf::st_as_sf(
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326,   # important: input is lat/lon
    remove = FALSE
  ) %>%
  sf::st_transform(utm_11)
 

# strava
strava_by_osm <- strava_edges %>%
  rename(osm_id = osmId) %>%
  dplyr::mutate(
    osm_id = as.character(osm_id),
    strava_len_m = as.numeric(sf::st_length(.))
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(osm_id) %>%
  dplyr::summarise(
    strava_total_trip_count = stats::weighted.mean(
      total_trip_count, strava_len_m, na.rm = TRUE
    ),
    strava_total_commute_pct = stats::weighted.mean(
      total_commute_pct, strava_len_m, na.rm = TRUE
    ),
    strava_pct_wmn = stats::weighted.mean(
      pct_wmn, strava_len_m, na.rm = TRUE
    ),
    strava_n_segments = dplyr::n(),
    .groups = "drop"
  )

network_context <- network_enriched %>%
  dplyr::mutate(osm_id = as.character(osm_id)) %>%
  dplyr::left_join(strava_by_osm, by = "osm_id") %>%
  dplyr::mutate(
    strava_join_method = dplyr::if_else(
      !is.na(strava_total_trip_count),
      "osm_id_weighted",
      NA_character_
    )
  )

needs_spatial <- network_context %>%
  dplyr::filter(is.na(strava_total_trip_count)) %>%
  dplyr::select(
    -dplyr::any_of(c(
      "strava_total_trip_count",
      "strava_total_commute_pct",
      "strava_pct_wmn"
    ))
  )

spatial_matched <- add_best_overlap_attributes(
  target_lines = needs_spatial,
  source_lines = strava_edges,
  attrs = c(
    "total_trip_count",
    "total_commute_pct",
    "pct_wmn"
  ),
  target_id_col = "osm_id",
  source_id_col = "edgeUID",
  prefix = "strava"
) %>%
  dplyr::mutate(strava_join_method = "spatial_overlap")

network_context <- dplyr::bind_rows(
  network_context %>% dplyr::filter(!is.na(strava_total_trip_count)),
  spatial_matched
) %>%
  dplyr::mutate(
    strava_count_missing = is.na(strava_total_trip_count),
    strava_log_count = dplyr::if_else(
      !is.na(strava_total_trip_count),
      log1p(as.numeric(strava_total_trip_count)),
      NA_real_
    )
  )

# Count incidents within 15 m of each edge
incident_join <- sf::st_is_within_distance(
  network_context,
  incidents,
  dist = 15
)

network_context <- network_context %>%
  dplyr::mutate(
    incident_count_250m = lengths(incident_join),
    incident_log_count_250m = log1p(incident_count_250m)
  )


comfort_predictors <- c(
  "strava_total_trip_count",
  "strava_total_commute_pct",
  "strava_pct_wmn",
  "strava_id",
  #"strava_dist_m",
  "strava_count_missing",
  "strava_log_count",
  "incident_count_250m",
  "incident_log_count_250m"
)


# mapview checks
library(mapview)

# m_strava_vol <- mapview(
#   network_context,
#   zcol = "strava_total_trip_count",
#   at = quantile(network_context$strava_total_trip_count, probs = seq(0, 1, 0.1), na.rm = TRUE),
#   layer.name = "Strava total trips"
# )
# 
# mapview(strava_edges,
#         zcol = "total_trip_count")
# 
# m_strava_missing <- mapview(
#   network_context,
#   zcol = "strava_count_missing",
#   col.regions = c("FALSE" = "#1b9e77", "TRUE" = "#bdbdbd"),
#   layer.name = "Strava missing"
# )
# 
# m_commute <- mapview(
#   network_context,
#   zcol = "strava_total_commute_pct",
#   at = seq(0, 1, by = 0.1),
#   layer.name = "Commute %"
# )
# 
# m_wmn <- mapview(
#   network_context,
#   zcol = "strava_pct_wmn",
#   at = seq(0, 1, by = 0.1),
#   layer.name = "Women %"
# )
# 
# m_dist <- mapview(
#   network_context,
#   zcol = "strava_dist_m",
#   at = c(0, 1, 2, 5, 10, 20, 50),
#   layer.name = "Strava match distance (m)"
# )
# 
# 
# m_incident <- mapview(
#   network_context,
#   zcol = "incident_count_250m",
#   at = c(0, 1, 2, 5, 10, 20),
#   layer.name = "Incident count (250m)"
# )
# 
# m_incident_log <- mapview(
#   network_context,
#   zcol = "incident_log_count_250m",
#   layer.name = "Incident log count"
# )



context_lookup <- network_context %>%
  sf::st_drop_geometry() %>%
  dplyr::select(osm_id, dplyr::all_of(comfort_predictors))

reference_context <- reference_pts %>%
  dplyr::left_join(context_lookup, by = "osm_id")

saveRDS(
  network_context,
  here::here("data_intermediate", paste0("network_comfort_context_", study_area_name, ".rds"))
)

sf::st_write(
  network_context %>% st_transform(4326),
  here::here("data_intermediate", paste0("network_comfort_context_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

saveRDS(
  reference_context,
  here::here("data_intermediate", paste0("reference_points_comfort_context_", study_area_name, ".rds"))
)

sf::st_write(
  reference_context %>% st_transform(4326),
  here::here("data_intermediate", paste0("reference_points_comfort_context_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

# Clean and export
network_context_export <- network_context %>%
  dplyr::select(
    osm_id,
    name,
    #pred_class = dplyr::any_of("pred_class"),
    
    # Replica
    replica_vol_aadt,
    replica_log_aadt,
    replica_vol_id,
    replica_spd_free_flow_speed_mph,
    replica_spd_average_speed_mph,
    replica_spd_speed_p95_mph,
    replica_spd_id,
    replica_traffic_context,
    replica_low_stress,
    
    # Strava
    strava_total_trip_count,
    strava_total_commute_pct,
    strava_pct_wmn,
    strava_join_method,
    # strava_overlap_len_m,
    # strava_overlap_ratio,
    
    # Incidents
    incident_count_250m,
    incident_log_count_250m,
    
    # Useful context
    road_class,
    highway,
    cycleway,
    traffic_calming,
    surface_class,
    has_any_cycleway,
    has_lane,
    has_track,
    bike_route_designated,
    is_bike_path,
    surface_class,
    is_unpaved,
    is_paved_bike_path,
    is_bridge,
    is_oneway,
    
    
    #geometry = dplyr::everything()
  ) %>%
  dplyr::select(
    -dplyr::ends_with(".x"),
    -dplyr::ends_with(".y")
  )

# -----------------------------
# Extract comfort/context features to reference points
# -----------------------------

comfort_fields <- c(
  "osm_id",

  # replica
  # "replica_vol_aadt",
  # "replica_log_aadt",
  # "replica_vol_id",
  "replica_spd_free_flow_speed_mph",
  #"replica_spd_average_speed_mph",
  "replica_spd_speed_p95_mph",
  # "replica_spd_id",
  # "replica_traffic_context",
  "replica_low_stress",
  
  # Strava
  "strava_total_trip_count",
  "strava_total_commute_pct",
  "strava_pct_wmn",
  "strava_join_method",

  # Incidents
  "incident_count_250m",
  "incident_log_count_250m"
)

comfort_lookup <- network_context_export %>%
  sf::st_drop_geometry() %>%
  dplyr::select(dplyr::all_of(comfort_fields))

reference_export <- reference_pts %>%
  dplyr::left_join(comfort_lookup, by = "osm_id") 

reference_export <- reference_export %>%
  dplyr::select(
    osm_id,
    #name,
    rating,
    class,
    reviewer,
    #pred_class = dplyr::any_of("pred_class"),
    
    # Replica
    replica_vol_aadt,
    replica_log_aadt,
    replica_spd_free_flow_speed_mph,
    replica_spd_average_speed_mph,
    replica_spd_speed_p95_mph,
    replica_traffic_context,
    replica_low_stress,
    
    # Strava
    strava_total_trip_count,
    strava_total_commute_pct,
    strava_pct_wmn,
    strava_join_method,
    # strava_overlap_len_m,
    # strava_overlap_ratio,
    
    # Incidents
    incident_count_250m,
    incident_log_count_250m,
    
    # Useful context
    road_class,
    highway,
    traffic_calming,
    surface_class,
    has_any_cycleway,
    has_lane,
    has_track,
    bike_route_designated,
    is_bike_path,
    surface_class,
    is_unpaved,
    is_paved_bike_path,
    is_bridge,
    is_oneway
  ) %>%
  dplyr::select(
    -dplyr::ends_with(".x"),
    -dplyr::ends_with(".y")
  )

sf::st_write(network_context_export, "outputs/gis/network_comfort.gpkg", layer = "network", delete_layer = TRUE)
sf::st_write(reference_export, "outputs/gis/reference_pts_comfort.gpkg", layer = "points", delete_layer = T)

message("Reference points exported: ", nrow(reference_export))
print(summary(reference_export$strava_total_trip_count))
print(table(is.na(reference_export$strava_total_trip_count)))

table(reference_export$rating, useNA = "ifany")
prop.table(table(reference_export$rating))

library(ggplot2)

ggplot(reference_export, aes(x = factor(rating))) +
  geom_bar() +
  labs(
    title = "Distribution of rating",
    x = "Rating",
    y = "Count"
  ) +
  theme_minimal()

xtabs(~ class + rating, data = reference_export)

prop.table(
  xtabs(~ class + rating, data = reference_export),
  margin = 1
)

ggplot(reference_export, aes(x = factor(rating), fill = class)) +
  geom_bar(position = "fill") +
  labs(
    title = "Rating distribution by class",
    x = "Rating",
    y = "Proportion"
  ) +
  theme_minimal()

ggplot(reference_export, aes(
  x = rating,
  y = log1p(strava_total_trip_count)
)) +
  geom_jitter(alpha = 0.4, width = 0.2, height = 0) +
  labs(
    title = "Rating vs Strava volume",
    x = "Rating",
    y = "log(Strava trip count + 1)"
  ) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal()

ggplot(reference_export, aes(
  x = rating,
  y = log1p(strava_total_trip_count)
)) +
  geom_jitter(alpha = 0.4, width = 0.2) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ class) +
  theme_minimal()

ggplot(reference_export, aes(x = class, fill = factor(rating))) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    title = "Ratings within each comfort class",
    x = "Comfort class",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

reference_export <- reference_export %>%
  dplyr::mutate(
    class = forcats::fct_reorder(class, rating, .fun = median, na.rm = TRUE)
  )

ggplot(reference_export, aes(x = class, fill = factor(rating))) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    title = "Ratings within each comfort class",
    x = "Comfort class",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

reference_export %>%
  st_drop_geometry() %>%
  group_by(class) %>%
  summarise(comfort = median(rating, na.rm = T))

reference_export %>%
  st_drop_geometry() %>%
  group_by(class, reviewer) %>%
  summarise(comfort = median(rating, na.rm = T)) %>%
  pivot_wider(values_from = comfort,
              names_from = c(reviewer))

library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)

plot_df <- reference_export %>%
  filter(!is.na(rating), !is.na(class), !is.na(reviewer)) %>%
  mutate(
    rating = as.numeric(rating),
    class = fct_reorder(class, rating, .fun = median, na.rm = TRUE),
    reviewer = factor(reviewer)
  )

ggplot(plot_df, aes(x = rating, y = class, color = reviewer)) +
  geom_density_ridges(
    aes(group = interaction(class, reviewer)),
    fill = NA,
    linewidth = 0.8,
    scale = 1.1,
    rel_min_height = 0.01
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = sort(unique(plot_df$rating))) +
  labs(
    title = "Preference ratings by infrastructure type and reviewer",
    x = "Preference rating",
    y = "Infrastructure type",
    color = "Reviewer"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
  

ggplot(plot_df, aes(x = rating, y = class, color = reviewer)) +
  geom_jitter(height = 0.15, width = 0.08, alpha = 0.35) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Preference ratings by infrastructure type and reviewer",
    x = "Preference rating",
    y = "Infrastructure type",
    color = "Reviewer"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

message("Done.")

