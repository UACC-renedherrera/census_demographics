# setup ----
library(here)
library(tidycensus)
library(tidyverse)
library(stringr)
library(knitr)

# library(tigris)

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
  "Yuma")

# load tables ----
# AGE AND SEX
age_sex <- get_acs(geography = "county",
                   table = "S0101",
                   state = "AZ",
                   year = 2018,
                   cache_table = TRUE)

# RACE
race <- get_acs(geography = "county",
                   table = "B02001",
                   state = "AZ",
                   year = 2018,
                   cache_table = TRUE)

# for age and sex ----
# S0101
# use stringr to clean up county name
# and save
age_sex <- age_sex %>% 
  mutate(NAME = str_replace(age_sex$NAME, " County, Arizona", ""))

# filter to southern az catchment 
age_sex <- age_sex %>%
  filter(NAME %in% counties)

# from AGE AND SEX
# total population for each county
# group by county
# filter to variable "S0101_C01_001" Estimate!!Total!!Total population
age_sex %>% group_by(NAME) %>%
  filter(variable == "S0101_C01_001")

# for race ----
# B02001
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

var_race <- c("B02001_001", # total including all races 
              "B02001_002", # "White alone"
              "B02001_003", # "Black or African American alone", 
              "B02001_004", # "Asian alone"
              "B02001_005", # "Native Hawaiian and Other Pacific Islander alone",
              "B02001_006") # # "Some other race alone"

race <- race %>%
  filter(variable %in% var_race)

race$variable <- recode(race$variable, "B02001_001" = "Total",
       "B02001_002" = "White",
       "B02001_003" = "Black",
       "B02001_004" = "Asian",
       "B02001_005" = "Native Hawaiian",
       "B02001_006" = "Other")

race %>% spread(variable, estimate)

race_total <- race %>% group_by(NAME) %>%
  filter(variable == "Total")

race %>% group_by(NAME) %>%
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