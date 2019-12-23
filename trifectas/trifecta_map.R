library(tidyverse)
library(USAboundaries)
library(sf)
library(leaflet)

trifectas <- read_csv("trifectas/trifectas.csv")
trifectas

trifectas %>%
  filter(!is.na(trifecta)) %>%
  group_by(year = as.integer(year), trifecta) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n, col = trifecta)) +
  geom_line(size = 2) +
  scale_colour_manual(name = "Trifecta", values = c("Democratic" = "blue", "Republican" = "red", "None" = "purple")) +
  scale_x_continuous(breaks = 2009:2019) +
  scale_y_continuous(breaks = 2*(0:13), limits = c(0, 26)) +
  labs(title = "State government trifectas over time", x = "Year", y = "Number of trifectas",
       caption = "Trifectas do not apply to Nebraska, which has a non-partisan unicameral legislature")

# Maps
## Post-2010 trifectas
trifectas_sf_2010 <- USAboundaries::us_states() %>%
  left_join(trifectas %>% select(name = state, everything()) %>% filter(year == 2010), by = "name") %>%
  mutate(trifecta_color = case_when(trifecta == "Democratic" ~ "blue",
                                    trifecta == "Republican" ~ "red",
                                    trifecta == "None" ~ "purple"),
         redistrict_control = case_when(trifecta == "Democratic" & redistricting_control == "Legislature" ~ "blue",
                                        trifecta == "Republican" & redistricting_control == "Legislature" ~ "red"))

leaflet(trifectas_sf_2010) %>%
  addPolygons(color = "#444444", fillColor = ~trifecta_color, weight = 1, fillOpacity = 0.75)

## Post-2019 trifectas
trifectas_sf_2019 <- USAboundaries::us_states() %>%
  left_join(trifectas %>% select(name = state, everything()) %>% filter(year == 2019), by = "name") %>%
  mutate(trifecta_color = case_when(trifecta == "Democratic" ~ "blue",
                                    trifecta == "Republican" ~ "red",
                                    trifecta == "None" ~ "purple"),
         redistrict_control = case_when(trifecta == "Democratic" & redistricting_control == "Legislature" ~ "blue",
                                        trifecta == "Republican" & redistricting_control == "Legislature" ~ "red"))

leaflet(trifectas_sf_2019) %>%
  addPolygons(color = "#444444", fillColor = ~trifecta_color, weight = 1, fillOpacity = 0.75)
