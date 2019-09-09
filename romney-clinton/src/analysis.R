library("src/lib.R")

# Identify Romney-Clinton counties
results_1216 <- read_csv("data/county_results_12-16.csv")
results_1216.wide <- results_1216 %>%
  melt(id.vars = c("state", "state_abbr", "county", "fips_code", "year"), variable.name = "party", value.name = "votes") %>%
  spread(party, votes) %>%
  mutate(dem_mov_2party = (dem - gop) / (dem + gop)) %>%
  select(-dem, -gop, -oth) %>%
  spread(year, dem_mov_2party) %>%
  select(state, state_abbr, county, fips_code, dem_2012 = `2012`, dem_2016 = `2016`) %>%
  mutate(result = case_when(dem_2012 < 0 & dem_2016 < 0 ~ "Republican",
                            dem_2012 < 0 & dem_2016 >= 0 ~ "Romney-Clinton",
                            dem_2012 >= 0 & dem_2016 < 0 ~ "Obama-Trump",
                            dem_2012 >= 0 & dem_2016 >= 0 ~ "Democratic"))
  as.tbl() 

romney_clinton <- results_1216.wide %>%
  filter(dem_2012 < 0 & dem_2016 > 0)

romney_clinton

table(romney_clinton$state_abbr)

## Romney counties which swung toward Clinton
swung_clinton <- results_1216.wide %>%
  mutate(result = case_when(dem_2012 < 0 & dem_2016 < dem_2012 ~ "Trump > Romney",
                                  dem_2012 < 0 & dem_2016 >= dem_2012 ~ "Trump < Romney",
                                  dem_2012 >= 0 & dem_2016 < dem_2012 ~ "Clinton < Obama",
                                  dem_2012 >= 0 & dem_2016 >= dem_2012 ~ "Clinton > Obama"))

swung_clinton

table(swung_clinton$state_abbr)

acs_2017_metadata <- listCensusMetadata(name = "acs/acs5", vintage = 2017)

acs_2017_metadata %>%
  filter(grepl("internet", concept, ignore.case = TRUE)) %>%
  View()


acs_2017_metadata %>%
  filter(grepl("B03002_003E", name, ignore.case = TRUE)) %>%
  View()


# Get census data
acs_2017_counties <- getCensus("acs/acs5", vintage = 2017, region = "county:*", regionin = "state:*",
                               vars = c("B03002_001E", "B15003_001E", "B15003_022E", "B15003_023E", "B15003_024E", "B15003_025E", "B03002_003E", 
                                        "B03002_004E", "B03002_012E", "B03002_006E", "B07012_002E", "B19013_001E", "C18120_002E", "C18120_006E", 
                                        "B16005_024E", "B28008_004E", "C18130_016E")) %>%
  mutate(fips_code = paste0(state, county),
         college = B15003_022E + B15003_023E + B15003_024E + B15003_025E) %>%
  select(fips_code, total_pop = B03002_001E, age25 = B15003_001E, college, white_pct = B03002_003E, black_pct = B03002_004E,
         latino_pct = B03002_012E, asian_pct = B03002_006E, poverty_rate = B07012_002E, median_income = B19013_001E, labor_force = C18120_002E,
         unemployed = C18120_006E, foreign_born_pct = B16005_024E, broadband_pct = B28008_004E, age65_pct = C18130_016E) %>%
  filter(as.numeric(fips_code) < 72000) %>%
  melt(id.vars = c("fips_code", "total_pop")) %>%
  mutate(value = value / total_pop) %>%
  spread(variable, value) %>%
  mutate(unemployment_rate = unemployed / labor_force,
         college_pct = college / age25,
         median_income = median_income * total_pop) %>%
  as.tbl() 

acs_2017_counties

# Filter to Romney-Clinton counties
acs_2017_counties %>%
  left_join(results_1216.wide, by = "fips_code") %>%
  group_by(result) %>%
  summarise(college = wtd.mean(white_pct, total_pop))

acs_2017_counties %>%
  left_join(results_1216.wide, by = "fips_code") %>%
  melt(id.vars = c("fips_code", "state", "state_abbr", "county", "result", "total_pop")) %>%
  group_by(result, variable) %>%
  summarise(avg = wtd.mean(value, total_pop)) %>%
  filter(!is.na(result)) %>%
  spread(result, avg)
