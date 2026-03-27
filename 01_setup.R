# Purpose:
# Project setup file.
# Loads packages, defines CRS and run settings, sets options,
# creates core directories, and sources helper functions.

message("Running: 01_setup.R")

# -----------------------------
# Packages
# -----------------------------
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(here)
library(tigris)
library(tidyr)
library(purrr)

# Optional modelling packages (load here only if you want them available everywhere)
# library(tidymodels)
# library(xgboost)
# library(mapview)

# -----------------------------
# Project settings
# -----------------------------

# Coordinate reference systems
utm_11 <- 32611
crs_wgs84 <- 4326

# Current study area name
# Change this when switching between prototype areas / full runs
study_area_name <- "city_santa_barbara_north"

# -----------------------------
# tigris options
# -----------------------------
options(tigris_use_cache = TRUE)
options(tigris_cache_dir = here::here("data_raw", "tigris_cache"))
options(stringsAsFactors = FALSE)

# -----------------------------
# Create core directories if needed
# -----------------------------
dir.create(here::here("data_raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data_intermediate"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data_processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "maps"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("outputs", "models"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Source helper functions
# -----------------------------
source(here::here("R", "helpers_sf.R"))

# Source these as they become useful
source(here::here("R", "helpers_features.R"))
source(here::here("R", "helpers_sf.R"))
# source(here::here("R", "helpers_modeling.R"))
# source(here::here("R", "helpers_io.R"))

# -----------------------------
# Run summary
# -----------------------------
message("Study area: ", study_area_name)
message("Projected CRS (utm_11): ", utm_11)
message("WGS84 CRS: ", crs_wgs84)
message("Setup complete.")
