library(tidyverse)
library(sf)

feedback <- st_read("data_raw/network_feedback_points.shp")

feedback$id <- 1:nrow(feedback)

feedback %>%
  write_csv("data_intermediate/feedback.csv")

feedback %>%
  st_write("data_intermediate/feedback_annotated.gpkg")
