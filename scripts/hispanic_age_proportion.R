# script will produce 
# 1. table of hispanic proportion in each catchment county
# 2. table of total hispanic proportion for catchment 
# 3. median age by sex of hispanic in each catchment county 

# set up ----
library(here)
library(tidyverse)
library(knitr)
library(ggthemes)
library(tidycensus)
library(stringr)

# read data ----

# SEX BY AGE (HISPANIC OR LATINO) ----
# Survey/Program: American Community Survey
# Universe: People who are Hispanic or Latino
# Year: 2018
# Estimates: 5-Year
# Table ID: B01001I

az_sex_by_age <- get_acs(
  geography = "county",
  table = "B01001I",
  cache_table = TRUE,
  year = 2018,
  state = "AZ"
)

# list county names
counties <- unique(az_sex_by_age$NAME)

# use stringr to clean up county name
# and save
az_sex_by_age <- az_sex_by_age %>% 
  mutate(NAME = str_replace(az_sex_by_age$NAME, " County, Arizona", ""))

# filter to southern az catchment 
az_catch_sex_by_age <- az_sex_by_age %>%
  filter(NAME %in% c(
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  ))

# load and list variables from acs 5 year 2018 dataset
acs_20128_var <- load_variables(2018, "acs5", cache = TRUE)
# view(acs_20128_var)

# SEX BY AGE (HISPANIC OR LATINO)
# save variables of interest to value 
age_sex_var <- c(
  "B01001I_001", # total estimate for each county
  "B01001I_013", # estimate for male 55-64 years for each county
  "B01001I_014", # estimate for male 65-74 years for each county
  "B01001I_015", # estimate for male 75-84 years for each county
  "B01001I_016", # estimate for male 85<= years for each county
  "B01001I_028", # estimate for female 55-64 years for each county
  "B01001I_029", # estimate for female 65-74 years for each county
  "B01001I_030", # estimate for female 75-84 years for each county
  "B01001I_031" # estimate for female 85<= years for each county
) 

# group by county and 
# apply filter to show desired age groups total and 55-85+
az_catch_sex_by_age <- az_catch_sex_by_age %>%
  group_by(NAME) %>%
  filter(variable %in% age_sex_var)

# filter to show total population (hispanic or latino) for each county 
age_sex_var_totals <- az_catch_sex_by_age %>%
  filter(variable == "B01001I_001")

# for each county, show the population of hispanic or latino age 55+
az_catch_sex_by_age <- az_catch_sex_by_age %>%
  filter(variable != "B01001I_001") %>%
  summarise(hispanic_population = sum(estimate))

# for each county, show both the age 55+ and total Hispanic or Latino
az_catch_sex_by_age <- inner_join(az_catch_sex_by_age, age_sex_var_totals)

# for each county, calculate the proportion of hispanic or latino age 55+
# select columns and display data grouped by county 
az_catch_sex_by_age %>%
  mutate(percentage_hispanic = hispanic_population / estimate) %>%
  select(NAME, hispanic_population, estimate, percentage_hispanic) %>%
  kable(col.names = c("County", "Age 55+", "Total", "Prop 55+"),
        caption = "ACS 5 Year (2014-2018) population estimates for Hispanic or Latino")

# for the catchment altogether; calculate the proportion of hispanic or latino age 55+
az_catch_sex_by_age %>%
  summarize(
    hispanic_total = sum(hispanic_population),
    catch_total = sum(estimate)
  ) %>%
  mutate(hispanic_percent = hispanic_total / catch_total) %>%
  kable(col.names = c("Age 55+", "Total", "Prop 55+"),
        caption = "ACS 5 Year (2014-2018) population estimates for Hispanic or Latino")

# MEDIAN AGE BY SEX (HISPANIC OR LATINO) ----
# Survey/Program: American Community Survey
# Universe: People who are Hispanic or Latino
# Year: 2018
# Estimates: 5-Year
# Table ID: B01002I
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

az_med_age <- get_acs(
  geography = "county",
  table = "B01002I",
  cache_table = TRUE,
  year = 2018,
  state = "AZ"
)

az_med_age <- az_med_age %>% 
  mutate(NAME = str_replace(az_med_age$NAME, " County, Arizona", ""))

az_catch_med_age <- az_med_age %>%
  filter(NAME %in% c(
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  ))

# for MEDIAN AGE BY SEX (HISPANIC OR LATINO)
# save variables of interest to value 
med_age_var <- c(
  "B01002I_001", #Estimate!!Median age --!!Total 
  "B01002I_002", #Estimate!!Median age --!!Male
  "B01002I_003" #Estimate!!Median age --!!Female
) 

# display median age for hispanic latino in each county
# group by county and 
# apply filter to show median age by sex of Hispanic in each county
az_catch_med_age %>%
  group_by(NAME) %>%
  filter(variable == "B01002I_001") %>%
  select(NAME, estimate) %>%
  kable(col.names = c("County", "Median Age"),
        caption = "ACS 5 Year (2014-2018) population estimates for Hispanic or Latino")

