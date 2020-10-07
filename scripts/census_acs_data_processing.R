# setup ----
# load packages
library(here)
library(tidyverse)
library(knitr)

# table format should be
# | variable | geography | race | sex | year | estimate | source |

# median age
# age 65 and over
# total population
# rural
# female
# race non hispanic white
# race hispanic
# race american indian
# race non hispanic black

#

# median age
median_age <- read_rds("data/tidy/acs5_median_age.rds")

# function to select areas to include on slides and visualizations
select_areas_for_slide <- function(x){
  areas <- c(
    "United States",
    "Arizona",
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  )
  x %>%
    filter(NAME %in% areas)
}

# function to build the values for the slide
build_table_for_slide <- function(x){
  x %>%
    filter(race == "all") %>%
    select(NAME, variable, estimate) %>%
    spread(key = NAME,
           value = estimate) %>%
    select(variable, `United States`, Arizona)
}

median_age %>%
  select_areas_for_slide %>%
  build_table_for_slide()  %>%
  mutate(Catchment = "34.3-40.6")

# age 65 and over
