# setup ----
library(here)
library(tidycensus)
library(tidyverse)
library(sf)

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

