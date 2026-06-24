# Purpose:
# Prepare the cleaned reference dataset for modelling.
# - read raw spatial reference data
# - transform CRS
# - keep and rename modelling fields
# - filter out unusable classes
# - clip to study area
# - save outputs for downstream scripts

source("01_setup.R")

message("Running: 03_prepare_reference_data.R")

# -----------------------------
# Read inputs
# -----------------------------
reference_raw <- sf::st_read(
  here::here("data_raw", "ref_data_spatial_04May2026.gpkg"),
  quiet = TRUE
)

study_area <- sf::st_read(
  here::here("data_intermediate", paste0("study_area_", study_area_name, ".gpkg")),
  quiet = TRUE
)

# -----------------------------
# Prepare full cleaned reference data
# -----------------------------
reference <- reference_raw %>%
  sf::st_transform(utm_11) %>%
  dplyr::select(
    SegmentID,
    source_wave = Status,
    stratum,
    block_id = block_id_final,
    class_raw = classification,
    class = classification,
    reviewer,
    
    # raw survey fields kept for QA / later use
    bks_desg  = bikes_desig_raw,
    rd_pth    = Road.or.path,
    pth_sfc   = path_surface_raw,
    ped_sep   = ped_sep_raw,
    n_lanes   = lanes_raw,
    tfc_clm   = calming_raw,
    bk_lane   = painted_lane_raw,
    bk_buff   = lane_buffer_raw,
    pvd_shldr = shoulder_raw,
    prkd_crs  = Parked.cars,
    rating    = Is.this.a.good.place.to.ride.a.bike.,
    rvw_cmmt  = Comment..optional.
  ) %>%
  dplyr::mutate(
    class = dplyr::case_when(
      class %in% c("Bike-only trails", "Multi-use trails") ~ "Paths",
      class %in% c("Connecting gravel path") ~ "Trails (gravel)",
      bk_lane == "Yes" &
        bk_buff == "Physical barrier" ~ "Bike lane (physical protection)",
      class == "Bike lane (buffer)" ~ "Bike lane (painted buffer)",
      TRUE ~ class
    ))

# quick point fix 
reference[which(reference$SegmentID %in% c(575, 682)), ]$class <- "Bike lane (painted buffer)"
reference[which(reference$SegmentID %in% c(641, 743, 1285,
                                           1365)), ]$class <- "Bike lane (physical protection)"

reference[which(reference$SegmentID %in% c(74, 201, 271, 1159, 1175, 1268, 1392, 1511, 1530, 1220)), ]$class <- "Bike lane (no buffer)"

reference[which(reference$SegmentID %in% c(168, 1465, 1216)), ]$class <- "Paths"
reference[which(reference$SegmentID %in% c(1258)), ]$class <- "Trails (gravel)"

reference[which(reference$SegmentID %in% c(1287, 1492, 1440, 1037, 1097, 1291, 1583, 1601, 1466, 1100, 1625)), ]$class <- "Paved shoulder"
reference[which(reference$SegmentID %in% c(85, 289, 1244, 1400)), ]$class <- "Road, no infrastructure"




# -----------------------------
# Filter to usable modelling classes
# -----------------------------
reference_model <- reference %>%
  dplyr::filter(
    !is.na(class),
    !class %in% c(
      NA,
      "UNKNOWN",
      "No streetview",
      "Not enough evidence to evaluate"
    )
  )

# -----------------------------
# Clip to study area
# -----------------------------
reference_study_area <- reference_model %>%
  sf::st_filter(study_area)

# -----------------------------
# Quick QA
# -----------------------------
message("Reference rows (full cleaned): ", nrow(reference))
message("Reference rows (usable for modelling): ", nrow(reference_model))
message("Reference rows in study area: ", nrow(reference_study_area))
message("Duplicate SegmentID values: ", sum(duplicated(reference$SegmentID)))

message("Class distribution in study area:")
print(table(reference_study_area$class, useNA = "ifany"))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  reference,
  here::here("data_intermediate", paste0("reference_full_", study_area_name, ".rds"))
)

saveRDS(
  reference_model,
  here::here("data_intermediate", paste0("reference_model_full_", study_area_name, ".rds"))
)

saveRDS(
  reference_study_area,
  here::here("data_intermediate", paste0("reference_points_", study_area_name, ".rds"))
)

sf::st_write(
  reference_study_area,
  here::here("data_intermediate", paste0("reference_points_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")
