# setup ----
library(here)
library(tidycensus)
library(tidyverse)
library(stringr)
library(knitr)
library(dataMaid)

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

# for age and sex ----
# S0101
# use stringr to clean up county name
# and save
age_sex <- age_sex %>% 
  mutate(NAME = str_replace(age_sex$NAME, " County, Arizona", ""))

# filter to southern az catchment 
age_sex <- age_sex %>%
  filter(NAME %in% counties)

# save to file
write_rds(age_sex, "data/tidy/acs5_2018_age_sex.rds")

# from AGE AND SEX
# total population for each county
# group by county
# filter to variable "S0101_C01_001" Estimate!!Total!!Total population
age_sex %>% group_by(NAME) %>%
  filter(variable == "S0101_C01_001")

# from age and sex
# filter to age 60 and older
# S0101_C01_028	
# Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!60 years and over	
age_sex %>% group_by(NAME) %>%
  filter(variable == "S0101_C01_028") #AGE CATEGORIES!!60 years and over	

# show age 60 and older as proportion of total population
# first filter to age 60 and older only 
# save
age_sex_60 <- age_sex %>% group_by(NAME) %>%
  filter(variable == "S0101_C01_028") %>%
  mutate("60+" = estimate)

# then filter to total 
# save
age_sex_total <- age_sex %>% group_by(NAME) %>%
  filter(variable == "S0101_C01_001") %>%
  mutate(total = estimate)

# last combine
age_sex_profiles <- inner_join(age_sex_60, age_sex_total, by = c("GEOID", "NAME"))

# select and rename columns
age_sex_profiles <- age_sex_profiles %>%
  select(GEOID, NAME, "60+", total) %>%
  mutate("prop_60+" = round(`60+`/total, digits = 2))

# testing
# in progress
age_sex_subject <- get_acs(geography = "county",
                              variables = c(median_age = "S0101_C01_032",
                                            total = "S0101_C01_001",
                                            "age_60+" = "S0101_C01_028",
                                            male_median_age = "S0101_C04_032",
                                            male_total = "S0101_C04_001",
                                            "male_age_60+" = "S0101_C04_028",
                                            female_median_age = "S0101_C05_032",
                                            female_total = "S0101_C05_001",
                                            "female_age_60+" = "S0101_C05_028"),
                       cache_table = TRUE,
                       year = 2018,
                       state = "AZ")
age_sex_subject %>%
  filter(variable == "male_total")

# end test

# for race ----
# B02001

# read data from acs 
# RACE
race <- get_acs(geography = "county",
                table = "B02001",
                state = "AZ",
                year = 2018,
                cache_table = TRUE)


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
var_race <- c("B02001_001", # total including all races 
              "B02001_002", # "White alone"
              "B02001_003", # "Black or African American alone", 
              "B02001_004", # "Asian alone"
              "B02001_005") # "Native Hawaiian and Other Pacific Islander alone",
              #"B02001_006") # # "Some other race alone"

# use race variables to filter 
race <- race %>%
  filter(variable %in% var_race)

# recode variables from code to label 
race$variable <- recode(race$variable, "B02001_001" = "Total",
       "B02001_002" = "White",
       "B02001_003" = "Black",
       "B02001_004" = "Asian",
       "B02001_005" = "Native Hawaiian")
       #"B02001_006" = "Other")

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
         value = "estimate")
  
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
  mutate(White_prop = round(White / Total, digits = 2),
         Black_prop = round(Black / Total, digits = 2),
         Asian_prop = round(Asian / Total, digits = 2),
         NH_prop = round(`Native Hawaiian` / Total, digits = 2),
         Other_prop = round(Other / Total, digits = 2)) %>%
  select(NAME, White_prop, Black_prop, Asian_prop, NH_prop, Other_prop) %>%
  kable()

# collapse by summarizing
race %>% group_by(NAME) %>%
  summarize()
  

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

# Pima County ----

# Total Population ----

az_population_total <- get_acs(geography = "county",
                   table = "B01003",
                   state = "AZ",
                   year = 2018,
                   cache_table = TRUE)

pima_population_total_acs5 <- az_population_total %>% filter(NAME == "Pima County, Arizona")

# Population Race Proportions ----

az_population_race <- get_acs(geography = "county",
                               variables = c("WHITE ALONE" = "B01001A_001",
                                             "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
                                             "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
                                             "ASIAN ALONE" = "B01001D_001",
                                             "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
                                             "SOME OTHER RACE ALONE" = "B01001F_001",
                                             "TWO OR MORE RACES" = "B01001G_001",
                                             "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
                                             "HISPANIC OR LATINO" = "B01001I_001"),
                               state = "AZ",
                               year = 2018,
                               cache_table = TRUE)

write_rds(az_population_race, "data/tidy/county_by_race.rds")

az_population_race %>% 
  filter(NAME == "Pima County, Arizona") %>%
  mutate(prop = round(estimate / sum(estimate), digits = 3)) %>%
  ggplot(mapping = aes(x = prop, y = reorder(variable, prop))) +
  geom_col() +
  geom_label(aes(label = prop), nudge_x = .075) +
  labs(title = "Population Proportion by Race",
       subtitle = "Pima County, AZ",
       x = "Proportion",
       y = "Race / Ethnicity",
       caption = "Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates")

# Population Race by Age ---- 

az_population_race <- get_acs(geography = "county",
                              variables = c("WHITE ALONE" = "B01001A_001",
                                            "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
                                            "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
                                            "ASIAN ALONE" = "B01001D_001",
                                            "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
                                            "SOME OTHER RACE ALONE" = "B01001F_001",
                                            "TWO OR MORE RACES" = "B01001G_001",
                                            "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
                                            "HISPANIC OR LATINO" = "B01001I_001"),
                              state = "AZ",
                              year = 2018,
                              cache_table = TRUE)