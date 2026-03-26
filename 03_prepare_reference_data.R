# Purpose:
# Prepare the cleaned reference dataset for modelling.
# Includes deduplication, classification cleanup, CRS transform,
# block assignment, and clipping to the study area.

reference <- st_read("data_raw/ref_data_spatial.gpkg") %>%
  st_transform(utm_11) %>%
  select(
    "SegmentID",                           
    "Status",
    "stratum",                             
    "block_id" = "block_id_final",
    "class"= "classification",
    "reviewer",
    "bks_desg"= "bikes_desig_raw",
    "rd_pth" = "Road.or.path",                        
    "pth_sfc" = "path_surface_raw",
    "ped_sep" = "ped_sep_raw",
    "n_lanes" = "lanes_raw",
    "tfc_clm" = "calming_raw",
    "bk_lane" = "painted_lane_raw",
    "bk_buff" = "lane_buffer_raw",
    "pvd_shldr" = "shoulder_raw",
    "prkd_crs" = "Parked.cars",
    "rating" = "Is.this.a.good.place.to.ride.a.bike.",
    "rvw_cmmt" = "Comment..optional."                  
  )

reference_study_area <- reference %>%
  st_intersection(city_sb) %>%
  filter(
    ! class %in% c("UNKNOWN",
                   "No streetview",
                   "Not enough evidence to evaluate",
                   NA)
  )
