# setup ----
# load packages 
library(here)
library(tidyverse)
library(stringr)
library(knitr)

# load data race ----
# for catchment counties 
race <- read_rds("data/tidy/acs5_2018_race.rds")

# what is the total population
# for each county and the total altogether
race %>% 
  group_by(NAME) %>%
  filter(variable == "B02001_001") %>%
  summarize()
