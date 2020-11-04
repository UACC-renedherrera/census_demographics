# setup ----
# load packages
library(here)
library(tidyverse)
library(stringr)
library(knitr)


# population total, usa ----
# read data

acs5_age_USA <- read_rds("data/tidy/acs5_population_age_18_65.rds")

acs5_age_USA <- acs5_age_USA %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

acs5_age_USA %>%
  mutate(prop_65 = round(age_group_65 / Total, digits = 3),
         prop_18 = round(under_18 / Total, digits = 3))


# sex ----
# female
# male




# age ----
# age under 18
# age 18-39
# age 65 and over




# race and ethnicity ----
# american indian / alaska native
# asian pacific islander 
# black
# hispanic
# non hispanic white



# education ----
# less than hs
# hs
# college



# income ----
# median family income
# median household income


# insurance ----
# percent uninsured <65 year age


# language ----



# poverty ----



# unemployed ----
