# setup ----
library(here)
library(tidycensus)
library(tidyverse)

# read data
# for reference
# load variable tables to find variables of interest
# 2015
var_acs_2015_acs5 <- load_variables(2015, "acs5", cache = TRUE)
var_acs_2015_acs5_profile <- load_variables(2015, "acs5/profile", cache = TRUE)
var_acs_2015_acs5_subject <- load_variables(2015, "acs5/subject", cache = TRUE)

#2019
var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)

# function that removes clutter from each county name
# for example change from Apache County, Arizona to Apache
trim_county_name <- function(x ){
  x %>%
    mutate(NAME = str_replace(x$NAME, " County, Arizona", ""))
}

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# female 2019 ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

female_sex_2019 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Female" = "S0101_C05_001"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# for catchment
female_sex_2019 %>%
  mutate(NAME = str_replace(female_sex_2019$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  summarise(Female = (sum(Female)))


# female 2015 ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2015
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2011-2015 American Community Survey 5-Year Estimates

female_sex_2015 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Female" = "S0101_C03_001"),
  year = 2015,
  cache_table = TRUE,
  survey = "acs5"
)

# for catchment
female_sex_2015 %>%
  mutate(NAME = str_replace(female_sex_2019$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  summarise(Female = (sum(Female)))


# race
# population catchment race White alone 
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az_county <- get_acs(
  geography = "county",
  state = "AZ",
  table = "B02001",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# acs5_population_race_az_county %>%
#   mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
#   filter(NAME %in% counties) %>%
#   filter(variable == "B02001_002") %>%
#   summarise(total = sum(estimate))

# # population catchment race American Indian and Alaska Native alone
# # RACE
# # Survey/Program: American Community Survey
# # Universe: Total population
# # Year: 2019
# # Estimates: 5-Year
# # Table ID: B02001
# #
# # Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates
# 
# acs5_population_race_az_county %>%
#   mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
#   filter(NAME %in% counties) %>%
#   filter(variable == "B02001_004") %>%
#   summarise(total = sum(estimate))

# AA 2019 ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_003") %>%
  summarise(total = sum(estimate))

# race
# population catchment race White alone 
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2015
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2015 American Community Survey 5-Year Estimates

acs5_population_race_az_county <- get_acs(
  geography = "county",
  state = "AZ",
  table = "B02001",
  year = 2015,
  cache_table = TRUE,
  survey = "acs5"
)

# acs5_population_race_az_county %>%
#   mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
#   filter(NAME %in% counties) %>%
#   filter(variable == "B02001_002") %>%
#   summarise(total = sum(estimate))

# # population catchment race American Indian and Alaska Native alone
# # RACE
# # Survey/Program: American Community Survey
# # Universe: Total population
# # Year: 2019
# # Estimates: 5-Year
# # Table ID: B02001
# #
# # Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates
# 
# acs5_population_race_az_county %>%
#   mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
#   filter(NAME %in% counties) %>%
#   filter(variable == "B02001_004") %>%
#   summarise(total = sum(estimate))

# AA 2015 ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2015
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2015 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_003") %>%
  summarise(total = sum(estimate))


# Hispanic Latino 2019 ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

acs5_hispanic_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)


val_catch_hisp <- acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "Total") %>%
  summarise(estimate = sum(estimate))

acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "Total") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = val_catch_hisp,
         prop = estimate / total)


# Hispanic Latino 2015 ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2015
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2015 American Community Survey 5-Year Estimates

acs5_hispanic_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2015,
  cache_table = TRUE,
  survey = "acs5"
)


val_catch_hisp <- acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "Total") %>%
  summarise(estimate = sum(estimate))

acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "Total") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = val_catch_hisp,
         prop = estimate / total)


# other 2015 ----
race_other_2015 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("total" = "DP05_0065",
                "white" = "DP05_0072",
                "ai" = "DP05_0074",
                "hispanic" = "DP05_0066"),
  year = 2015,
  cache_table = TRUE,
  survey = "acs5"
)

race_other_2015 <- race_other_2015 %>%
  mutate(NAME = str_replace(race_other_2015$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe))

race_other_2015 %>%
  spread(key = variable, value = estimate) %>%
  mutate(other = total - white - hispanic - ai) %>%
  summarise(ai = sum(ai),
            hispanic = sum(hispanic),
            other = sum(other))

# other 2019 ----
race_other_2019 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("total" = "DP05_0070",
                "white" = "DP05_0077",
                "ai" = "DP05_0079",
                "hispanic" = "DP05_0071"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

race_other_2019 <- race_other_2019 %>%
  mutate(NAME = str_replace(race_other_2019$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe))

race_other_2019 %>%
  spread(key = variable, value = estimate) %>%
  mutate(other = total - white - hispanic - ai) %>%
  summarise(ai = sum(ai),
            hispanic = sum(hispanic),
            other = sum(other))


# age 2015 ----

age_2015 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("total" = "DP05_0001",
                ">18" = "DP05_0018",
                ">65" = "DP05_0021"),
  year = 2015,
  cache_table = TRUE,
  survey = "acs5"
)

age_2015 %>%
    mutate(NAME = str_replace(age_2015$NAME, " County, Arizona", "")) %>%
    filter(NAME %in% counties) %>%
    select(!(moe)) %>%
  spread(key = variable, value = estimate) %>%
  mutate(`<18` = total - `>18`,
         `18-64` = `>18` - `>65`) %>%
  select(NAME, `<18`, `18-64`, `>65`) %>%
  summarise(`<18` = sum(`<18`), 
            `18-64` = sum(`18-64`), 
            `>65` = sum(`>65`))

# age 2019 ----

age_2019 <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("total" = "DP05_0001",
                ">18" = "DP05_0021",
                ">65" = "DP05_0024"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

age_2019 %>%
  mutate(NAME = str_replace(age_2019$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable, value = estimate) %>%
  mutate(`<18` = total - `>18`,
         `18-64` = `>18` - `>65`) %>%
  select(NAME, `<18`, `18-64`, `>65`) %>%
  summarise(`<18` = sum(`<18`), 
            `18-64` = sum(`18-64`), 
            `>65` = sum(`>65`))
