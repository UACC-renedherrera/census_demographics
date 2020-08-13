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
