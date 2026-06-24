apply_infra_postprocess_rules <- function(df,
                                          pred_col = "pred_class",
                                          out_col = "pred_class_final") {
  
  df %>%
    dplyr::mutate(
      pred_class_model = .data[[pred_col]],
      
      "{out_col}" := dplyr::case_when(
        
        # Exclusions / no infrastructure
        # Exclude ordinary crossings, but keep bike/path crossings for continuity
        dplyr::coalesce(is_crossing, FALSE) &
          !dplyr::coalesce(is_bike_crossing, FALSE) ~
          "No infrastructure",
        
        surface_class == "sand" ~ "No infrastructure",
        
        highway == "footway" &
          !dplyr::coalesce(bike_allowed_explicit, FALSE) ~
          "No infrastructure",
        
        # Paved bike-accessible paths
        dplyr::coalesce(is_paved_bike_path, FALSE) ~
          "Paths",
        
        # Gravel / unpaved bike-accessible paths
        dplyr::coalesce(is_bike_path, FALSE) &
          surface_class == "unpaved" ~
          "Trails (gravel)",
        
        # Preserve model-predicted gravel trails only if not paved
        as.character(.data[[pred_col]]) == "Trails (gravel)" &
          surface_class != "paved_smooth" &
          surface_class != "paved_rough" ~
          "Trails (gravel)",
        
        # High comfort

        dplyr::coalesce(has_track, FALSE) ~
          "Bike lane (physical protection)",
        
        # Medium comfort
        dplyr::coalesce(has_lane, FALSE) &
          (
            dplyr::coalesce(has_bike_buffer, FALSE) |
              dplyr::coalesce(has_bike_separation, FALSE)
          ) ~
          "Bike lane (painted buffer)",
        
        # Bike boulevard: special medium-comfort case
        dplyr::coalesce(is_low_speed_volume_candidate, FALSE) &
          dplyr::coalesce(has_route_signal, FALSE) &
          dplyr::coalesce(traffic_calming_present, FALSE) ~
          "Bike boulevard",
        
        # Low comfort
        dplyr::coalesce(has_lane, FALSE) ~
          "Bike lane (no buffer)",
        
        # Signed low-speed/low-volume route
        dplyr::coalesce(is_low_speed_volume_candidate, FALSE) &
          dplyr::coalesce(has_route_signal, FALSE) ~
          "Signed low-speed/low-volume route",
        
        # Low-speed/low-volume street
        dplyr::coalesce(is_low_speed_volume_candidate, FALSE) ~
          "Low-speed/low-volume street",
        
        
        # Bike signal exists, but does not meet comfort rules
        dplyr::coalesce(has_bike_infra_signal, FALSE) ~
          "Non-conforming infrastructure",
        
        # Final fallback
        TRUE ~ "No infrastructure"
      ),
      
      "{out_col}" := factor(.data[[out_col]])
    )
}

collapse_to_reference_class <- function(x) {
  dplyr::case_when(
    x %in% c(
      "No infrastructure",
      "Low-speed/low-volume street",
      "Signed low-speed/low-volume route",
      "Bike boulevard",
      "Non-conforming infrastructure"
    ) ~ "Road, no infrastructure",
    
    TRUE ~ as.character(x)
  )
}

translate_reference_class <- function(x) {
  dplyr::case_when(
    x %in% c(
      "Road, no infrastructure",
      "Traffic calming low volume street"
    ) ~ "No infrastructure",
    
    TRUE ~ as.character(x)
  )
}