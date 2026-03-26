# create the city of Santa Barbara subset (testing) and the Santa Barbara County
# boundaries for the full run

county_sb <- counties(state = "CA", cb = TRUE, year = 2022) %>%
  filter(NAME == "Santa Barbara") %>%
  st_transform(utm_11)

city_sb <- places(state = "CA", cb = TRUE, year = 2022) %>%
  filter(NAME == "Santa Barbara") %>%
  st_transform(utm_11)

st_write(county_sb,
         here("data_intermediate", "study_area_county.gpkg"),
         delete_dsn = TRUE)

st_write(city_sb,
         here("data_intermediate", "study_area_city_santa_barbara.gpkg"),
         delete_dsn = TRUE)
