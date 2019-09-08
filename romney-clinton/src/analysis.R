library("src/lib.R")

results_1216 <- read_csv("data/county_results_12-16.csv")
results_1216.wide <- results_1216 %>%
  melt(id.vars = c("state", "state_abbr", "county", "fips_code", "year"), variable.name = "party", value.name = "votes") %>%
  spread(party, votes) %>%
  mutate(dem_mov_2party = (dem - gop) / (dem + gop)) %>%
  select(-dem, -gop, -oth) %>%
  spread(year, dem_mov_2party) %>%
  select(state, state_abbr, county, fips_code, dem_2012 = `2012`, dem_2016 = `2016`) %>%
  as.tbl()

romney_clinton <- results_1216.wide %>%
  filter(dem_2012 < 0 & dem_2016 > 0)

romney_clinton

table(romney_clinton$state_abbr)

