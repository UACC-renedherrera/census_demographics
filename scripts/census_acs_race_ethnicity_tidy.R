# Race and Ethnicity Demographics
# René Dario Herrera 
# University of Arizona Cancer Center
# 12 August 2021 

# set up #### 
# packages 
library(here)
library(tidycensus)
library(tidyverse)
library(treemap)

# read data ####
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates
az_race <- get_acs(
  geography = "state",
  variables = c(
    "Total" = "DP05_0033",
    "White alone" = "DP05_0077P",
    "Black or African American alone" = "DP05_0078P",
    "American Indian and Alaska Native alone" = "DP05_0079P",
    "Asian alone" = "DP05_0080P",
    "Native Hawaiian and Other Pacific Islander alone" = "DP05_0081P",
    "Hispanic" = "DP05_0071P"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

# inspect 
glimpse(az_race)

# prepare for data viz
az_race_other <- az_race %>%
  filter(variable != "Total") %>%
  summarise(sum(estimate)) %>%
  transmute(other = 100 - `sum(estimate)`)

az_race %>%
  filter(variable != "Total" & variable != "Native Hawaiian and Other Pacific Islander alone") %>%
  add_row(
    GEOID = "04",
    NAME = "Arizona",
    variable = "Other",
    estimate = 2.40,
    moe = NA
  ) %>%
  treemap(index = "variable",
          vSize = "estimate",
          type = "index",
          title = "Race and Ethnicity in Arizona; 2014-2018
          Source: US Census American Community Survey")


