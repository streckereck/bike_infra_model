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
    replica_speed_missing = is.na(average_speed_mph),
    replica_volume_missing = is.na(aadt),
    replica_missing = case_when(
      replica_speed_missing & replica_volume_missing ~ "Speed and volume missing",
      replica_volume_missing ~ "Volume missing",
      replica_speed_missing ~ "Speed missing",
      T ~ "Speed and volume available"
    )
  )

table(reference_study_area$replica_speed_missing)
table(reference_study_area$replica_volume_missing)
table(reference_study_area$replica_missing)
