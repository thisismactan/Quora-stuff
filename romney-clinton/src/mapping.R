source("src/analysis.R")

# Shapefiles
counties_sf <- us_counties() %>%
  select(fips_code = geoid, everything())

states_sf <- us_states()

counties_1216_sf <- counties_sf %>%
  left_join(results_1216.wide, by = c("fips_code", "state_abbr")) %>%
  mutate(color_2012 = case_when(dem_2012 < 0 ~ "firebrick",
                                dem_2012 >= 0 ~ "dodgerblue4"),
         color_2016 = case_when(dem_2016 < 0 ~ "firebrick",
                                dem_2016 >= 0 ~ "dodgerblue4"),
         alpha_2012 = abs(dem_2012)^0.4,
         alpha_2016 = abs(dem_2016)^0.4,
         color_2012 = col2hex(color_2012),
         color_2016 = col2hex(color_2016))

romney_clinton_sf <- counties_sf %>%
  left_join(romney_clinton, by = c("fips_code", "state_abbr")) %>%
  mutate(color = case_when(is.na(dem_2016) ~ "#AAAAAA",
                           !is.na(dem_2016) ~ "#880088"))

# Maps for the two years
## 2012
leaflet() %>%
  addPolygons(data = counties_1216_sf, color = "#444444", fillColor = ~color_2012, weight = 1, opacity = 1, fillOpacity = ~alpha_2012) %>%
  addPolygons(data = states_sf, color = "#000000", weight = 1, opacity = 1, fillOpacity = 0)

## 2016
leaflet() %>%
  addPolygons(data = counties_1216_sf, color = "#444444", fillColor = ~color_2016, weight = 1, opacity = 1, fillOpacity = ~alpha_2016) %>%
  addPolygons(data = states_sf, color = "#000000", weight = 1, opacity = 1, fillOpacity = 0)

# Map for Romney-Clinton counties
leaflet(romney_clinton_sf) %>%
  addPolygons(data = romney_clinton_sf, color = "#444444", fillColor = ~color, weight = 1, opacity = 1, fillOpacity = 1) %>%
  addPolygons(data = states_sf, color = "#000000", weight = 1, opacity = 1, fillOpacity = 0)
