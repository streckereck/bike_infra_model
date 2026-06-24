make_osm_composite_features <- function(osm) {
  
  tag_present <- function(x) {
    x <- as.character(x)
    !is.na(x) & x != ""
  }
  
  get_tag <- function(data, col) {
    if (col %in% names(data)) {
      data[[col]]
    } else {
      rep(NA_character_, nrow(data))
    }
  }
  
  get_col <- function(data, col) {
    if (col %in% names(data)) {
      data[[col]]
    } else {
      rep(NA_character_, nrow(data))
    }
  }
  
  footway_tag <- get_tag(osm, "footway")
  crossing_tag <- get_tag(osm, "crossing")
  cycleway_tag <- get_tag(osm, "cycleway")
  
  
  yes_vals <- c("yes", "designated", "permissive")
  lane_vals <- c("lane")
  shared_lane_vals <- c("shared_lane", "share_busway", "shared")
  track_vals <- c("track")
  
  osm %>%
    dplyr::mutate(
 
      
      footway_tag = as.character(footway_tag),
      crossing_tag = as.character(crossing_tag),
      cycleway_tag = as.character(cycleway_tag),
      
      
      is_crossing =
        stringr::str_to_lower(footway_tag) == "crossing" |
        tag_present(crossing_tag),
      
      is_bike_crossing =
        dplyr::coalesce(is_crossing, FALSE) &
        (
          highway == "cycleway" |
            cycleway_tag == "crossing" |
            bicycle == "designated"
        ),

      bike_allowed_explicit =
        bicycle %in% c("yes", "designated", "permissive") |
        highway == "cycleway",
      
      is_crossing =
        dplyr::coalesce(
          stringr::str_to_lower(footway_tag) == "crossing" |
            tag_present(crossing_tag),
          FALSE
        ),
      
      surface_class = dplyr::case_when(
        surface %in% c("asphalt", "concrete", "paved") ~ "paved_smooth",
        surface %in% c("paving_stones", "sett", "cobblestone") ~ "paved_rough",
        surface %in% c("gravel", "dirt", "ground", "earth", "fine_gravel", "compacted", "unpaved") ~ "unpaved",
        surface %in% c("sand") ~ "sand",
        TRUE ~ "unknown"
      ),
      
      is_unpaved = surface_class == "unpaved",
      
      is_bike_path =
        (
          !dplyr::coalesce(is_crossing, FALSE) |
            dplyr::coalesce(is_bike_crossing, FALSE)
        ) &
        (
          highway == "cycleway" |
            (highway %in% c("path", "footway", "pedestrian") & bike_allowed_explicit)
        ),
      
      is_paved_bike_path =
        is_bike_path &
        surface_class %in% c("paved_smooth", "paved_rough"),
      
      road_class = dplyr::case_when(
        highway %in% c("motorway", "trunk") ~ "arterial_high",
        highway %in% c("primary", "secondary") ~ "arterial",
        highway %in% c("tertiary") ~ "collector",
        highway %in% c("residential", "living_street") ~ "local",
        highway %in% c("service", "unclassified") ~ "local_low",
        
        is_bike_path ~ "Path",
        
        highway == "footway" ~ "footway_no_bike",
        highway == "path" ~ "path_no_bike",
        highway == "pedestrian" ~ "pedestrian_no_bike",
        
        TRUE ~ "other"
      ),
      
      traffic_calming_present =
        !is.na(traffic_calming),

      # -----------------------------
      # Bike facility indicators
      # -----------------------------

      has_any_cycleway =
        !is.na(cycleway) |
        !is.na(cycleway_left) |
        !is.na(cycleway_right) |
        !is.na(cycleway_both),
      
      has_lane =
        cycleway %in% lane_vals |
        cycleway_left %in% lane_vals |
        cycleway_right %in% lane_vals |
        cycleway_both %in% lane_vals,
      
      has_shared_lane =
        cycleway %in% shared_lane_vals |
        cycleway_left %in% shared_lane_vals |
        cycleway_right %in% shared_lane_vals |
        cycleway_both %in% shared_lane_vals,
      
      has_bike_buffer =
        cycleway_buffer %in% c("yes") |
        cycleway_left_buffer %in% c("yes") |
        cycleway_right_buffer %in% c("yes") |
        cycleway_both_buffer %in% c("yes") |
        !is.na(cycleway_buffer) |
        !is.na(cycleway_left_buffer) |
        !is.na(cycleway_right_buffer) |
        !is.na(cycleway_both_buffer),
      
      has_bike_separation =
        !is.na(cycleway_separation) |
        !is.na(cycleway_left_separation) |
        !is.na(cycleway_right_separation) |
        !is.na(cycleway_both_separation),
      
      has_track =
        cycleway %in% track_vals |
        cycleway_left %in% track_vals |
        cycleway_right %in% track_vals |
        cycleway_both %in% track_vals,
      
      bike_route_designated =
        bicycle == "designated" |
        lcn %in% yes_vals |
        rcn %in% yes_vals |
        ncn %in% yes_vals,
      
      has_route_signal =
        has_shared_lane |
        traffic_calming_present |
        bicycle == "designated" |
        lcn %in% yes_vals |
        rcn %in% yes_vals |
        ncn %in% yes_vals,
      
      has_bike_infra_signal =
        dplyr::coalesce(is_bike_path, FALSE) |
        dplyr::coalesce(has_lane, FALSE) |
        dplyr::coalesce(has_track, FALSE) |
        dplyr::coalesce(has_bike_buffer, FALSE) |
        dplyr::coalesce(has_bike_separation, FALSE) |
        dplyr::coalesce(has_shared_lane, FALSE) |
        dplyr::coalesce(bike_route_designated, FALSE),
      
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
        replica_speed_missing & replica_volume_missing ~ "no_replica",
        
        !replica_volume_missing &
          replica_vol_aadt <= 1500 &
          replica_speed_missing ~ "low_volume_only",
        
        !replica_volume_missing &
          !replica_speed_missing &
          replica_vol_aadt <= 1500 &
          replica_spd_average_speed_mph < 25 ~ "low_speed_low_volume",
        
        TRUE ~ "higher_speed_or_volume"
      ),
      
      is_low_speed_volume_candidate =
        replica_traffic_context %in% c(
          "low_speed_low_volume",
          "low_volume_only",
          "no_replica"
        ) &
        highway %in% c(
          "residential",
          "living_street",
          "unclassified"
        ),
      
      replica_log_aadt = dplyr::if_else(
        !is.na(aadt_valid),
        log1p(aadt_valid),
        NA_real_
      )
    )
}