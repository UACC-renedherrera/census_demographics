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
acs_variables <- load_variables(year = 2019, dataset = "acs5/profile", cache = TRUE)

# load data from census to environment 
# percent age 65 and over 
age_65 <- get_acs(geography = "tract",
                 state = "az",
                 county = "Pima",
                 variables = c("65_years" = "DP05_0029P"),
                 cache_table = TRUE,
                 year = 2019,
                 survey = "acs5",
                 geometry = TRUE)

# save to disk
st_write(obj = age_65,
         dsn = "gis/age_65/age_65.shp",
         layer = "estimate")

# percent hispanic or latino of any race
hispanic <- get_acs(geography = "tract",
                 state = "az",
                 county = "Pima",
                 variables = c("hispanic" = "DP05_0071P"),
                 cache_table = TRUE,
                 year = 2019,
                 survey = "acs5",
                 geometry = TRUE)

# save to disk
st_write(obj = hispanic,
         dsn = "gis/hispanic/hispanic.shp",
         layer = "estimate")

# percent no health insurance coverage
no_health <- get_acs(geography = "tract",
                    state = "az",
                    county = "Pima",
                    variables = c("no_health" = "DP03_0099P"),
                    cache_table = TRUE,
                    year = 2019,
                    survey = "acs5",
                    geometry = TRUE)

# save to disk
st_write(obj = no_health,
         dsn = "gis/no_health/no_health.shp",
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
