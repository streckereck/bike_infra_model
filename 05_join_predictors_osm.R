# add predictors to reference points from OSM network data

osm <- osm %>%
  dplyr::mutate(
    road_class = dplyr::case_when(
      highway %in% c("motorway", "trunk") ~ "arterial_high",
      highway %in% c("primary", "secondary") ~ "arterial",
      highway %in% c("tertiary") ~ "collector",
      highway %in% c("residential", "living_street") ~ "local",
      TRUE ~ "other"
    )
  )

reference_study_area <- reference_study_area %>% 
  add_nearest_edge_attributes(
    osm,
    c(
      "osm_id",
      "name",
      "Can_BICS",
      "bicycle",
      "bridge",
      "cycleway",
      "cycleway.left",
      "cycleway.right",
      "cycleway.both",
      "foot",
      "footway",
      "hgv",
      "highway",
      "road_class",
      "horse",
      "junction",
      "lanes",
      "lanes.backward",
      "lanes.forward",
      "lcn",
      "lcn_ref",
      "lit",
      "maxspeed",
      "motor_vehicle",
      "oneway",
      "oneway.bicycle",
      "path",
      "railway",
      "segregated",
      "sidewalk",
      "sidewalk.both.surface",
      "surface",
      "traffic_calming",
      "width",
      "local_cycle_network",
      "local_cycle_network_name",
      "regional_cycle_network",
      "regional_cycle_network_name",
      "national_cycle_network",
      "national_cycle_network_name",
      "mtb.scale.imba",
      "mtb.scale",
      "mtb",
      "sac_scale",
      "access",
      "natural"
    ),
    max_dist_m = 10
  )
