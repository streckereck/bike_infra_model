# create the city of Santa Barbara subset (testing) and the Santa Barbara County
# boundaries for the full run

counties <- st_read(
  "C:/Users/16043/Documents/basemap/tiger/counties/tl_2025_us_county.shp") %>%
  filter(NAMELSAD %in% study_area_name) %>%
  st_transform(utm_11)

# places are municipal boundaries in TIGER
places <- st_read(
  "C:/Users/16043/Documents/basemap/tiger/places/tl_2025_06_place.shp") %>%
  filter(NAMELSAD %in% subset_name) %>%
  st_transform(utm_11)