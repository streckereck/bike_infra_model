# add replica volumes and speeds

reference_study_area <- reference_study_area %>% 
  add_nearest_edge_attributes(
    replica_aadt_24,
    "aadt",
    max_dist_m = 10
  )

reference_study_area <- reference_study_area %>% 
  add_nearest_edge_attributes(
    replica_avg_speed_24,
    c("free_flow_speed_mph", "average_speed_mph", "speed_p95_mph"),
    max_dist_m = 10
  )

reference_study_area <- reference_study_area %>%
  mutate(
    replica_missing = case_when(
      is.na(aadt) & is.na(average_speed_mph) ~ "Speed and volume missing",
      is.na(aadt) ~ "Volume missing",
      is.na(average_speed_mph) ~ "Speed missing",
      T ~ "Speed and volume available"
    )
  )
