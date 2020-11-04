# setup

library(tidycensus)
library(tidyverse)
library(tigris)

# get data from acs 
# Median Income, educational attainment, sex, health insurance

az_acs_2018 <- get_acs(geography = "county",
              variables = c(medincome = "B19013_001",
                            edu_attain = "B15002_001",
                            sex = "B01001_001",
                            health_insurance = "B27001_001"),
              state = "AZ",
              year = 2018,
              geometry = TRUE)

write_rds(az_acs_2018, "data/az_acs_2018.rds")

# Educational Attainment 

az_HS_ed_2018 <- get_acs(geography = "county",
                              variables = c(edu_attain = "B15002_001",
                                            sex = "B01001_001",
                                            health_insurance = "B27001_001"),
                              state = "AZ",
                              year = 2018,
                              geometry = TRUE)

write_rds(az_HS_ed_2018, "data/az_HS_ed_2018.rds")

# median income bar plot

az_med_income_2018_plot <- az_acs_2018 %>%
  filter(variable == "medincome") %>%
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

az_acs_2018 %>% 
  filter(variable == "medincome") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26911) +
  labs(title = "Household income by county in Arizona",
       subtitle = "2014-2018 American Community Survey") +
  theme_light()

# high school  bar plot

az_health_insurance_2018_plot <- az_acs_2018 %>%
  filter(variable == "health_insurance") %>%
  mutate(NAME = gsub(" County, Arizona", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "#AB0520", size = 3) +
  labs(title = "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",
       subtitle = "2014-2018 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error) in USD") +
  theme_light()

# median income choropleth

az_acs_2018 %>% 
  filter(variable == "medincome") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26911) +
  labs(title = "Household income by county in Arizona",
       subtitle = "2014-2018 American Community Survey") +
  theme_light()