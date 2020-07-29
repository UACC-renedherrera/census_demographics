# setup ----
library(here)
library(tidycensus)
library(tidyverse)
library(stringr)
library(knitr)
library(purrr)
library(dataMaid)

# library(tigris)

# for reference
# load variable tables to find variables of interest
var_acs_2018_acs5 <- load_variables(2018, "acs5", cache = TRUE)
var_acs_2018_acs5_profile <- load_variables(2018, "acs5/profile", cache = TRUE)
var_acs_2018_acs5_subject <- load_variables(2018, "acs5/subject", cache = TRUE)

# save catchment counties to value
counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# load tables ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 1-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs5_age_sex <- get_acs(
  geography = "county",
  table = "S0101",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

# use stringr to clean up county name
# and save
acs5_age_sex <- acs5_age_sex %>%
  mutate(NAME = str_replace(acs5_age_sex$NAME, " County, Arizona", ""))

# save to file
write_rds(acs5_age_sex, "data/tidy/acs5_2018_age_sex.rds")

S0101_C01_030	

# population age USA ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_USA <- get_acs(
  geography = "us",
  variable = c("under_18" = "S0101_C01_022",
    "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_USA <- acs5_age_USA %>% 
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

acs5_age_USA %>%
  mutate(prop_65 = round(age_group_65 / Total, digits = 3),
         prop_18 = round(under_18 / Total, digits = 3))

# population age AZ ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_AZ <- get_acs(
  geography = "state",
  state = "az",
  variable = c("under_18" = "S0101_C01_022",
               "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_AZ <- acs5_age_AZ %>% 
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

acs5_age_AZ %>%
  mutate(prop_65 = round(age_group_65 / Total, digits = 3),
         prop_18 = round(under_18 / Total, digits =3))

# population age UAZCC Catchment ---- 
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_catch <- get_acs(
  geography = "county",
  state = "az",
  variable = c("under_18" = "S0101_C01_022",
               "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_catch <- acs5_age_catch %>%
  mutate(NAME = str_replace(acs5_age_catch$NAME, " County, Arizona", "")) %>% 
  select(!(moe)) %>%
  filter(NAME %in% counties) %>%
  spread(key = variable,
         value = estimate)

acs5_age_catch %>%
  summarise(under_18 = sum(under_18),
    age_group_65 = sum(age_group_65),
            total = sum(Total)) %>%
  mutate(prop_18 = round(under_18 / total, digits =3),
         prop_65 = round(age_group_65 / total, digits = 3))

# filter to southern az catchment
acs5_age_sex_catch <- acs5_age_sex %>%
  filter(NAME %in% counties)

# from AGE AND SEX
# total population for each county
# group by county
# filter to variable "S0101_C01_001" Estimate!!Total!!Total population
acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_001")

# from age and sex
# filter to age 60 and older
# S0101_C01_028
# Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!60 years and over
acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_028") # AGE CATEGORIES!!60 years and over

# show age 60 and older as proportion of total population
# first filter to age 60 and older only
# save
acs5_age_sex_catch_60 <- acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_028") %>%
  mutate("60+" = estimate)

# then filter to total
# save
acs5_age_sex_catch_total <- acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_001") %>%
  mutate(total = estimate)

# last combine
age_sex_profiles <- inner_join(age_sex_60, age_sex_total, by = c("GEOID", "NAME"))

# select and rename columns
age_sex_profiles <- age_sex_profiles %>%
  select(GEOID, NAME, "60+", total) %>%
  mutate("prop_60+" = round(`60+` / total, digits = 2))

# testing
# in progress
age_sex_subject <- get_acs(
  geography = "county",
  variables = c(
    median_age = "S0101_C01_032",
    total = "S0101_C01_001",
    "age_60+" = "S0101_C01_028",
    male_median_age = "S0101_C04_032",
    male_total = "S0101_C04_001",
    "male_age_60+" = "S0101_C04_028",
    female_median_age = "S0101_C05_032",
    female_total = "S0101_C05_001",
    "female_age_60+" = "S0101_C05_028"
  ),
  cache_table = TRUE,
  year = 2018,
  state = "AZ"
)
age_sex_subject %>%
  filter(variable == "male_total")

# end test

# for race ----
# B02001

# read data from acs
# RACE
race <- get_acs(
  geography = "county",
  table = "B02001",
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)


# total population by race for each county
# group by county then race
race

# use stringr to clean up county name
# and save
race <- race %>%
  mutate(NAME = str_replace(race$NAME, " County, Arizona", ""))

# filter to southern az catchment
race <- race %>%
  filter(NAME %in% counties)

# Census race has five options
# "White alone",
# "Black or African American alone",
# "Asian alone",
# "Native Hawaiian and Other Pacific Islander alone",
# "Some other race alone"

# save race variables to value
var_race <- c(
  "B02001_001", # total including all races
  "B02001_002", # "White alone"
  "B02001_003", # "Black or African American alone",
  "B02001_004", # "Asian alone"
  "B02001_005"
) # "Native Hawaiian and Other Pacific Islander alone",
# "B02001_006") # # "Some other race alone"

# use race variables to filter
race <- race %>%
  filter(variable %in% var_race)

# recode variables from code to label
race$variable <- recode(race$variable,
  "B02001_001" = "Total",
  "B02001_002" = "White",
  "B02001_003" = "Black",
  "B02001_004" = "Asian",
  "B02001_005" = "Native Hawaiian"
)
# "B02001_006" = "Other")

# calculate "other" and add to table
# sum named race as race_sub
race_subtotal <- race %>%
  group_by(NAME) %>%
  filter(variable != "Total") %>%
  summarize(race_sub = sum(estimate))

# generate total population for each county
race_total <- race %>%
  group_by(NAME) %>%
  filter(variable == "Total") %>%
  summarize(Total = sum(estimate))

# combine total and subtotal to calculate "other"
race_table <- full_join(race_subtotal, race_total)

# calculate "other"
race_other <- race_table %>%
  mutate(other = Total - race_sub) %>%
  select(NAME, "Other" = other) %>%
  gather(Other,
    key = "variable",
    value = "estimate"
  )

# add other variable back to original table
race <- full_join(race, race_other)

# convert race variable to factor
race$variable <- as_factor(race$variable)

# rearrange table in order by county then by race variable
race <- race %>%
  arrange(NAME, variable)

# save data
write_rds(race, "data/tidy/acs5_2018_race.rds")

# spread table to put each race on it's own column and county it's own row
race %>%
  select(NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  kable()

# table of proportions by race
race %>%
  select(NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  mutate(
    White_prop = round(White / Total, digits = 2),
    Black_prop = round(Black / Total, digits = 2),
    Asian_prop = round(Asian / Total, digits = 2),
    NH_prop = round(`Native Hawaiian` / Total, digits = 2),
    Other_prop = round(Other / Total, digits = 2)
  ) %>%
  select(NAME, White_prop, Black_prop, Asian_prop, NH_prop, Other_prop) %>%
  kable()

# collapse by summarizing
race %>%
  group_by(NAME) %>%
  summarize()


race_total <- race %>%
  group_by(NAME) %>%
  filter(variable == "Total")

race %>%
  group_by(NAME) %>%
  filter(variable != "Total") %>%
  summarize(total = sum(estimate))








# # get data from acs
# # Median Income, educational attainment, sex, health insurance
#
# az_acs_2018 <- get_acs(geography = "county",
#               variables = c(medincome = "B19013_001",
#                             edu_attain = "B15002_001",
#                             sex = "B01001_001",
#                             health_insurance = "B27001_001"),
#               state = "AZ",
#               year = 2018,
#               geometry = TRUE)
#
# write_rds(az_acs_2018, "data/az_acs_2018.rds")
#
# # Educational Attainment
#
# az_HS_ed_2018 <- get_acs(geography = "county",
#                               variables = c(edu_attain = "B15002_001",
#                                             sex = "B01001_001",
#                                             health_insurance = "B27001_001"),
#                               state = "AZ",
#                               year = 2018,
#                               geometry = TRUE)
#
# write_rds(az_HS_ed_2018, "data/az_HS_ed_2018.rds")
#
# # median income bar plot
#
# az_med_income_2018_plot <- az_acs_2018 %>%
#   filter(variable == "medincome") %>%
#   mutate(NAME = gsub(" County, Arizona", "", NAME)) %>%
#   ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#   geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
#   geom_point(color = "#AB0520", size = 3) +
#   labs(title = "Household income by county in Arizona",
#        subtitle = "2014-2018 American Community Survey",
#        y = "",
#        x = "ACS estimate (bars represent margin of error) in USD") +
#   theme_light()
#
# # median income choropleth
#
# az_acs_2018 %>%
#   filter(variable == "medincome") %>%
#   ggplot(aes(fill = estimate)) +
#   geom_sf(color = NA) +
#   coord_sf(crs = 26911) +
#   labs(title = "Household income by county in Arizona",
#        subtitle = "2014-2018 American Community Survey") +
#   theme_light()
#
# # high school  bar plot
#
# az_health_insurance_2018_plot <- az_acs_2018 %>%
#   filter(variable == "health_insurance") %>%
#   mutate(NAME = gsub(" County, Arizona", "", NAME)) %>%
#   ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#   geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
#   geom_point(color = "#AB0520", size = 3) +
#   labs(title = "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",
#        subtitle = "2014-2018 American Community Survey",
#        y = "",
#        x = "ACS estimate (bars represent margin of error) in USD") +
#   theme_light()
#
# # median income choropleth
#
# az_acs_2018 %>%
#   filter(variable == "medincome") %>%
#   ggplot(aes(fill = estimate)) +
#   geom_sf(color = NA) +
#   coord_sf(crs = 26911) +
#   labs(title = "Household income by county in Arizona",
#        subtitle = "2014-2018 American Community Survey") +
#   theme_light()

# Pima County ----

# Total Population ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_total <- get_acs(
  geography = "county",
  table = "B01003",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs1"
)

pima_population_total_acs5 <- az_population_total %>% filter(NAME == "Pima County, Arizona")
pima_population_total_acs5


# Race ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_race <- get_acs(
  geography = "county",
  variables = c(
    "White alone" = "B02001_002",
    "Black or African American alone" = "B02001_003",
    "American Indian and Alaska Native alone" = "B02001_004",
    "Asian alone" = "B02001_005",
    "Native Hawaiian and Other Pacific Islander alone" = "B02001_006",
    "Some other race alone" = "B02001_007",
    "Two or more races" = "B02001_008",
    "Two races including Some other race" = "B02001_009",
    "Two races excluding Some other race, and three or more races" = "B02001_010"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

write_rds(az_population_race, "data/tidy/county_by_race.rds")


az_population_race %>%
  filter(NAME == "Pima County, Arizona") %>%
  mutate(prop = round(estimate / sum(estimate), digits = 3))

az_population_race %>%
  filter(NAME == "Pima County, Arizona") %>%
  summarise(total = sum(estimate))


# Population Race Proportions ----
# Sex by Age
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B01001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_sex_age <- get_acs(
  geography = "county",
  variables = c(
    "WHITE ALONE" = "B01001A_001",
    "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
    "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
    "ASIAN ALONE" = "B01001D_001",
    "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
    "SOME OTHER RACE ALONE" = "B01001F_001",
    "TWO OR MORE RACES" = "B01001G_001",
    "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
    "HISPANIC OR LATINO" = "B01001I_001"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)

write_rds(az_population_sex_age, "data/tidy/county_by_sex_age.rds")

az_population_sex_age %>%
  filter(NAME == "Pima County, Arizona") %>%
  mutate(prop = round(estimate / sum(estimate), digits = 3)) %>%
  ggplot(mapping = aes(x = prop, y = reorder(variable, prop))) +
  geom_col() +
  geom_label(aes(label = prop), nudge_x = .075) +
  labs(
    title = "Population Proportion by Race",
    subtitle = "Pima County, AZ",
    x = "Proportion",
    y = "Race / Ethnicity",
    caption = "Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates"
  )

# Population Race by Age ----

az_population_race <- get_acs(
  geography = "county",
  variables = c(
    "WHITE ALONE" = "B01001A_001",
    "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
    "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
    "ASIAN ALONE" = "B01001D_001",
    "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
    "SOME OTHER RACE ALONE" = "B01001F_001",
    "TWO OR MORE RACES" = "B01001G_001",
    "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
    "HISPANIC OR LATINO" = "B01001I_001"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)


# attribute table ----
# total population usa ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_usa <- get_acs(
  geography = "us",
  table = "B01003",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_usa

# total population az ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B01003",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_az

# total population catchment ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_az_county <- get_acs(
  geography = "county",
  table = "B01003",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_az_county %>%
  mutate(NAME = str_replace(acs5_population_total_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  summarise(total = sum(estimate))

# population by sex usa ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex_usa <- get_acs(
  geography = "us",
  table = "S0101",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_sex_usa %>% filter(variable == "S0101_C05_001")

# population by sex az ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "S0101",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_sex_az %>%
  filter(variable == "S0101_C05_001")

# population by sex catchment ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex %>%
  filter(NAME %in% counties) %>%
  filter(variable == "S0101_C05_001") %>%
  summarise(total = sum(estimate))

# population USA hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_usa <- get_acs(
  geography = "us",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_usa

# population AZ hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_az

# population catchment hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2018,
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

# population USA race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa <- get_acs(
  geography = "us",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_usa %>%
  filter(variable == "B02001_002")

# population USA race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_004")

# population USA race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_003")

# population AZ race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az %>%
  filter(variable == "B02001_002")

# population AZ race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_004")

# population AZ race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_003")

# population catchment race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county <- get_acs(
  geography = "county",
  state = "AZ",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_002") %>%
  summarise(total = sum(estimate))

# population catchment race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_004") %>%
  summarise(total = sum(estimate))

# population catchment race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_003") %>%
  summarise(total = sum(estimate))

# population urban and rural usa ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_usa <- 27684999 / 131704730
population_rural_usa

# population urban and rural az ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_az <- 329022 / 2844526
population_rural_az

# population urban and rural catchment ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_catch <- tibble(
  area = c(
    "AZ",
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  ),
  estimate = c(
    329022,
    22851,
    33543,
    30511,
    6446,
    NA
  )
)

population_rural_catch %>%
  filter(area != "AZ") %>%
  drop_na() %>%
  summarise(total = sum(estimate))

# population uninsured usa ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance <- tibble(
  area = c("United States",
           "AZ",
           "Cochise",
           "Pima",
           "Pinal",
           "Santa Cruz",
           "Yuma"),
  prop = c(0.094,
           0.109,
           0.082,
           0.099,
           0.088,
           0.116,
           0.133),
  denominator = c(317941631,
                  6838270,
                  116140,
                  998748,
                  393843,
                  46251,
                  201230))

acs5_population_insurance <- acs5_population_insurance %>%
  mutate(estimate = prop * denominator) %>%
  select(area, estimate, denominator, prop)

acs5_population_insurance %>%
  filter(area == "United States")

# population uninsured az ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance %>%
  filter(area == "AZ")

# population uninsured catchment ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance %>%
  filter(!(area == "AZ" | area == "United States")) %>%
  summarise(estimate = sum(estimate),
            denominator = sum(denominator)) %>%
  mutate(prop = estimate / denominator)

# population high school graduate usa ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_usa <- get_acs(
  geography = "us",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_edu_usa %>%
  mutate(prop = estimate / 218446071)

# population high school graduate az ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_edu_az %>%
  mutate(prop = estimate / 4633932)

# population high school graduate catchment ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

val_catch_edu_den <- acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Population 25 years and over") %>%
  summarise(estimate = sum(estimate))

acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Population 25 years and over") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(denominator = val_catch_edu_den,
         prop = estimate / denominator)

# population unemployment usa ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_usa <- get_acs(
  geography = "us",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_unemployment_usa

# population unemployment az ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_unemployment_az

# population unemployment catchment ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_unemployment_catch %>%
  mutate(NAME = str_replace(acs5_unemployment_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  summarise(min(estimate),
            max(estimate))

# Poverty USA ----
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_usa <- get_acs(
  geography = "us",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_poverty_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(prop = 100*(number/total))

# Poverty AZ ----
# population unemployment az
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_poverty_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(prop = 100*(number/total))

# Poverty Catchment ----
# population unemployment catch
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_poverty_catch %>%
  mutate(NAME = str_replace(acs5_poverty_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  summarise(number = sum(number),
            total = sum(total)) %>%
  mutate(prop = 100*(number/total))


# Food Insecurity AZ ----
# https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/
# Household food insecurity
# 3 year average 2013-2015

food_insecurity <- tibble(
"area" = c("AZ"),
"rate" = c(.1920)
)

# Food Security in the United States: How Do States Compare?
# https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-us/interactive-charts-and-highlights/

# food insecurity 2016-2018
# 12.4%

# very low food insecurity 2016-2018
# 5.1%

# Population Foreign Born USA ---- 
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_USA <- get_acs(
  geography = "us",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_foreign_born_USA %>%
  select(!(moe)) %>%
  spread(key = variable, 
         value = estimate) %>%
  mutate(prop = foreign_born / total)

# Population Foreign Born AZ ---- 
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_AZ <- get_acs(
  geography = "state",
  state = "az",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_foreign_born_AZ %>%
  select(!(moe)) %>%
  spread(key = variable, 
         value = estimate) %>%
  mutate(prop = foreign_born / total)

# Population Foreign Born Catch ---- 
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
# 
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_foreign_born_catch %>%
  mutate(NAME = str_replace(acs5_foreign_born_catch$NAME, " County, Arizona", "")) %>% 
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable, 
         value = estimate) %>%
  summarise(foreign_born = sum(foreign_born),
            total = sum(total)) %>%
  mutate(prop = foreign_born / total)
