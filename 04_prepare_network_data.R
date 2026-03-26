# OSM
osm <- st_read("data_raw/highways_output_all_sb_2025.gpkg") %>%
  st_transform(utm_11) %>%
  mutate(
    sb_class = get_sb_comfort_from_Can_BICS(Can_BICS),
    length_km = as.numeric(st_length(.)) / 1000
  ) %>%
  st_intersection(city_sb)

# replica
replica_aadt_24 <- st_read("data_raw/aadt_2024_sb.geojson") %>% 
  st_transform(utm_11) %>%
  st_intersection(city_sb)

replica_avg_speed_24 <- st_read("data_raw/annual-speeds_2024_sb.geojson") %>%
  st_transform(utm_11) %>%
  st_intersection(city_sb)

# landuse