library(sf)
library(tigris)
library(tidyverse)
library(here)
library(xgboost)
library(mapview)

options(tigris_use_cache = TRUE)
options(tigris_cache_dir = here::here("data_raw", "tigris_cache"))

utm_11 <- 32611
study_area_name <- "Santa Barbara County"
subset_name <- "Santa Barbara city"

source("R/helpers_io.R")
source("R/helpers_sf.R")
source("R/helpers_features.R")
source("R/helpers_modeling.R")