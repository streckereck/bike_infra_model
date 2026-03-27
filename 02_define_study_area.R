# Purpose:
# Download and save boundary files used for modelling.
# Current outputs:
# - Santa Barbara County boundary
# - City of Santa Barbara boundary
# - study area boundary corresponding to study_area_name

source("01_setup.R")

message("Running: 02_define_study_area.R")

# -----------------------------
# Download and prepare boundaries
# -----------------------------
county_sb <- tigris::counties(state = "CA", cb = TRUE, year = 2022) %>%
  dplyr::filter(NAME == "Santa Barbara") %>%
  sf::st_transform(utm_11)

city_sb <- tigris::places(state = "CA", cb = TRUE, year = 2022) %>%
  dplyr::filter(NAME == "Santa Barbara") %>%
  sf::st_transform(utm_11)

# -----------------------------
# Write stable boundary files
# -----------------------------
sf::st_write(
  county_sb,
  here::here("data_intermediate", "study_area_county.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  city_sb,
  here::here("data_intermediate", "study_area_city_santa_barbara.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# -----------------------------
# Define active study area
# -----------------------------
if (study_area_name == "county") {
  study_area <- county_sb
} else if (study_area_name == "city_santa_barbara") {
  study_area <- city_sb
} else {
  stop("study_area_name not recognized: ", study_area_name)
}

# -----------------------------
# Write active study area file
# -----------------------------
sf::st_write(
  study_area,
  here::here("data_intermediate", paste0("study_area_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

# -----------------------------
# Quick QA
# -----------------------------
message("Saved boundaries:")
message("  - study_area_county.gpkg")
message("  - study_area_city_santa_barbara.gpkg")
message("  - study_area_", study_area_name, ".gpkg")

message("Done.")