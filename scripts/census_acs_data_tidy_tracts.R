# setup ----
# load package libraries 
library(here)
library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

# set options 
options(tigris_use_cache = TRUE)

# collect desired data from ACS ---- 
# find variables 
acs_variables_profile <- load_variables(year = 2019, dataset = "acs5/profile", cache = TRUE)
acs_variables_subject <- load_variables(year = 2019, dataset = "acs5/subject", cache = TRUE)

# load data from census to environment 
# percent age 65 and over 
age_65 <- get_acs(geography = "tract",
                 state = "az",
                 variables = c("65_years" = "DP05_0024P"),
                 cache_table = TRUE,
                 year = 2019,
                 survey = "acs5",
                 geometry = TRUE)

# save to disk
st_write(obj = age_65,
         dsn = "gis/age_65/age_65.shp",
         layer = "estimate")

# percent age 65 and over 
female_percent <- get_acs(geography = "tract",
                  state = "az",
                  variables = c("female_percent" = "DP05_0003P"),
                  cache_table = TRUE,
                  year = 2019,
                  survey = "acs5",
                  geometry = TRUE)

# save to disk
st_write(obj = female_percent,
         dsn = "gis/female_percent/female_percent.shp",
         layer = "estimate")

# percent hispanic or latino of any race
hispanic <- get_acs(geography = "tract",
                 state = "az",
                 variables = c("hispanic" = "DP05_0071P"),
                 cache_table = TRUE,
                 year = 2019,
                 survey = "acs5",
                 geometry = TRUE)

# save to disk
st_write(obj = hispanic,
         dsn = "gis/hispanic/hispanic.shp",
         layer = "estimate")

# percent Native American of any race
native_american <- get_acs(geography = "tract",
                    state = "az",
                    variables = c("native_american" = "DP05_0039P"),
                    cache_table = TRUE,
                    year = 2019,
                    survey = "acs5",
                    geometry = TRUE)

# save to disk
st_write(obj = native_american,
         dsn = "gis/native_american/native_american.shp",
         layer = "estimate")

# percent no health insurance coverage
no_health <- get_acs(geography = "tract",
                    state = "az",
                    variables = c("no_health" = "DP03_0099P"),
                    cache_table = TRUE,
                    year = 2019,
                    survey = "acs5",
                    geometry = TRUE)

# save to disk
st_write(obj = no_health,
         dsn = "gis/no_health/no_health.shp",
         layer = "estimate")

# Estimate!!Percent below poverty level!!Population for whom poverty status is determined	
percent_poverty <- get_acs(geography = "tract",
                     state = "az",
                     variables = c("percent_poverty" = "S1701_C03_001"),
                     cache_table = TRUE,
                     year = 2019,
                     survey = "acs5",
                     geometry = TRUE)

# save to disk
st_write(obj = percent_poverty,
         dsn = "gis/percent_poverty/percent_poverty.shp",
         layer = "estimate")


# get primary roads from tigris 
tigris_primary_roads <- primary_roads()

# save to disk 
st_write(obj = tigris_primary_roads,
         dsn = "gis/tigris_primary_roads/primary_roads.shp",
         layer = "MTFCC")

# get primary roads from tigris 
tigris_primary_secondary_roads <- primary_secondary_roads(state = "AZ")

# save to disk 
st_write(obj = tigris_primary_secondary_roads,
         dsn = "gis/tigris_primary_secondary_roads/primary_secondary_roads.shp",
         layer = "MTFCC")

# congressional district boundaries
tigris_cd <- congressional_districts() %>%
  filter(STATEFP == "04")

# inspect
plot(tigris_cd)
st_crs(tigris_cd)

# save to disk
st_write(obj = tigris_cd,
         dsn = "gis/tigris_congressional/tigris_congressional_districts.shp",
         layer = "NAMELSAD")

# state legislative district boundaries 
tigris_ld <- state_legislative_districts(state = "04")

# inspect
plot(tigris_ld)
st_crs(tigris_ld)

# save to disk
st_write(obj = tigris_cd,
         dsn = "gis/tigris_state_legislative/tigris_state_legislative_districts.shp",
         layer = "NAMELSAD")

# state legislative district boundaries 
az_counties <- counties(state = "04")

# inspect
plot(az_counties)
st_crs(az_counties)

# save to disk
st_write(obj = az_counties,
         dsn = "gis/tigris_az_counties/tigris_az_counties.shp",
         layer = "NAMELSAD")
