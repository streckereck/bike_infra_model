make_osm_composite_features <- function(osm) {
  
  yes_vals <- c("yes", "designated", "permissive")
  lane_vals <- c("lane", "shared_lane")
  track_vals <- c("track")
  
  osm %>%
    dplyr::mutate(
      # -----------------------------
      # Basic road hierarchy
      # -----------------------------
      road_class = dplyr::case_when(
        highway %in% c("motorway", "trunk") ~ "arterial_high",
        highway %in% c("primary", "secondary") ~ "arterial",
        highway %in% c("tertiary") ~ "collector",
        highway %in% c("residential", "living_street") ~ "local",
        highway %in% c("service", "unclassified") ~ "local_low",
        TRUE ~ "other"
      ),
      
      # -----------------------------
      # Existing comfort / source variables
      # -----------------------------
      sb_class = get_sb_comfort_from_Can_BICS(Can_BICS),
      length_km = as.numeric(sf::st_length(.)) / 1000,
      
      # -----------------------------
      # Bike facility indicators
      # -----------------------------
      has_any_cycleway =
        !is.na(cycleway) |
        !is.na(cycleway.left) |
        !is.na(cycleway.right) |
        !is.na(cycleway.both),
      
      has_lane =
        cycleway %in% lane_vals |
        cycleway.left %in% lane_vals |
        cycleway.right %in% lane_vals |
        cycleway.both %in% lane_vals,
      
      has_track =
        cycleway %in% track_vals |
        cycleway.left %in% track_vals |
        cycleway.right %in% track_vals |
        cycleway.both %in% track_vals,
      
      bike_route_designated =
        bicycle %in% yes_vals |
        lcn %in% yes_vals |
        local_cycle_network %in% yes_vals |
        regional_cycle_network %in% yes_vals |
        national_cycle_network %in% yes_vals,
      
      # -----------------------------
      # Surface quality
      # -----------------------------
      surface_class = dplyr::case_when(
        surface %in% c("asphalt", "concrete") ~ "paved_smooth",
        surface %in% c("paving_stones", "sett", "cobblestone") ~ "paved_rough",
        surface %in% c("gravel", "dirt", "ground", "earth", "fine_gravel") ~ "unpaved",
        TRUE ~ "unknown"
      ),
      
      is_unpaved = surface_class == "unpaved",
      
      # -----------------------------
      # Other useful flags
      # -----------------------------
      is_bridge = bridge %in% yes_vals,
      is_oneway = oneway %in% yes_vals
    )
}

get_sb_comfort_from_Can_BICS <- function(Can_BICS){
  case_when(
    Can_BICS %in% c("Multi-Use Path",
                    "Cycle Track",
                    "Bike Path",
                    "Local Street Bikeway") ~ "High comfort",
    Can_BICS %in% c("Painted Bike Lane") ~ "Medium/Low Comfort",
    T ~ "Non-conforming"
  )
}

make_replica_composite_features <- function(
    df,
    aadt_col = "replica_vol_aadt",
    speed_col = "replica_spd_average_speed_mph",
    dist_vol_col = "replica_vol_dist_m",
    dist_spd_col = "replica_spd_dist_m",
    max_valid_dist = 5,
    aadt_thresh = 1500,
    speed_thresh = 25
) {
  
  df %>%
    dplyr::mutate(
      # treat joined values as valid only if close enough
      replica_vol_valid = !is.na(.data[[aadt_col]]) & .data[[dist_vol_col]] <= max_valid_dist,
      replica_spd_valid = !is.na(.data[[speed_col]]) & .data[[dist_spd_col]] <= max_valid_dist,
      
      # keep only valid matched values
      aadt_valid = dplyr::if_else(
        replica_vol_valid,
        as.numeric(.data[[aadt_col]]),
        NA_real_
      ),
      
      speed_valid = dplyr::if_else(
        replica_spd_valid,
        as.numeric(.data[[speed_col]]),
        NA_real_
      ),
      
      replica_volume_missing = is.na(aadt_valid),
      replica_speed_missing  = is.na(speed_valid),
      
      replica_missing = dplyr::case_when(
        replica_volume_missing & replica_speed_missing ~ "Speed and volume missing",
        replica_volume_missing ~ "Volume missing",
        replica_speed_missing ~ "Speed missing",
        TRUE ~ "Speed and volume available"
      ),
      
      # strict low-stress flag when both are available
      replica_low_stress = dplyr::if_else(
        !is.na(aadt_valid) &
          !is.na(speed_valid) &
          aadt_valid < aadt_thresh &
          speed_valid < speed_thresh,
        TRUE,
        FALSE,
        missing = FALSE
      ),
      
      # composite traffic context
      replica_traffic_context = dplyr::case_when(
        is.na(aadt_valid) & is.na(speed_valid) ~ "no_replica",
        
        !is.na(aadt_valid) & !is.na(speed_valid) &
          aadt_valid < aadt_thresh & speed_valid < speed_thresh ~ "low_speed_low_volume",
        
        !is.na(aadt_valid) & is.na(speed_valid) &
          aadt_valid < aadt_thresh ~ "low_volume",
        
        !is.na(aadt_valid) & !is.na(speed_valid) ~ "higher_speed_or_volume",
        
        # just in case: speed available but no volume
        is.na(aadt_valid) & !is.na(speed_valid) &
          speed_valid < speed_thresh ~ "low_speed",
        
        is.na(aadt_valid) & !is.na(speed_valid) ~ "higher_speed_or_volume",
        
        TRUE ~ "no_replica"
      ),
      
      replica_log_aadt = dplyr::if_else(
        !is.na(aadt_valid),
        log1p(aadt_valid),
        NA_real_
      )
    )
}