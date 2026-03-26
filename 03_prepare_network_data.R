# OSM
osm <- st_read("data/national/highways_output_all_sb_2025.gpkg") %>%
  st_transform(utm_11) %>%
  mutate(
    sb_class = case_when(
      Can_BICS %in% c("Multi-Use Path",
                      "Cycle Track",
                      "Bike Path",
                      "Local Street Bikeway") ~ "High comfort",
      Can_BICS %in% c("Painted Bike Lane") ~ "Medium/Low Comfort",
      Can_BICS_class %in% c("Non-Conforming") ~ "Non-conforming"
    ),
    length_km = as.numeric(st_length(.)) / 1000
  ) %>%
  st_interstection(places)

# replica
replica_aadt_24 <- st_read("../replica/aadt_2024_sb.geojson") %>% 
  st_transform(utm_11)

replica_avg_speed_24 <- st_read("../replica/annual-speeds_2024_sb.geojson") %>%
  st_transform(utm_11)

# landuse