# setup

library(tidycensus)
library(tidyverse)
library(tigris)

# load dataset

read_rds("data/az_med_income_2018.rds")

# median income bar plot

az_med_income_2018_plot <- az_med_income_2018 %>%
  mutate(NAME = gsub(" County, Arizona", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "#AB0520", size = 3) +
  labs(title = "Household income by county in Arizona",
       subtitle = "2014-2018 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error) in USD") +
  theme_light()

# median income choropleth

az_med_income_2018_map <- az_med_income_2018 %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26911) +
  labs(title = "Household income by county in Arizona",
       subtitle = "2014-2018 American Community Survey") +
  theme_light()

# trying with leaflet 
library(leaflet)


