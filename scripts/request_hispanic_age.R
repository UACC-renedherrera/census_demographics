# data request: average age of Hispanics in the five county catchment area
# percentage of hispanic over age 50 in catchment area

# set up ----
library(here)
library(tidyverse)
library(knitr)
library(ggthemes)
library(tidycensus)

# read data ----

data <- read_csv("data_raw/ACSDT5Y2018.B01002I_data_with_overlays_2020-06-01T150831.csv",
  skip = 2,
  col_names = c(
    "id",
    "County",
    "Estimate_Total",
    "MOE_Total",
    "Estimate_Male",
    "MOE_Male",
    "Estimate_Female",
    "MOE_Female"
  )
)

glimpse(data)

data %>%
  mutate(County = gsub(" County, Arizona", "", County)) %>%
  ggplot(aes(x = Estimate_Total, y = reorder(County, Estimate_Total))) +
  geom_errorbarh(aes(xmin = Estimate_Total - MOE_Total, xmax = Estimate_Total + MOE_Total)) +
  geom_point(color = "#AB0520", size = 3) +
  labs(
    title = "Median Age for Hispanic or Latino by County",
    subtitle = "2014-2018 American Community Survey",
    caption = "Source:  U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates",
    x = "ACS Estimate for Median Age (shown with margin of error)",
    y = ""
  ) +
  theme_solarized() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  )

data %>%
  mutate(County = gsub(" County, Arizona", "", County)) %>%
  select(County, Estimate_Total) %>%
  arrange(desc(Estimate_Total)) %>%
  kable()


# read data ----

# SEX BY AGE (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: People who are Hispanic or Latino
# Year: 2018
# Estimates: 5-Year
# Table ID: B01001I

az <- get_acs(
  geography = "county",
  table = "B01001I",
  cache_table = TRUE,
  year = 2018,
  state = "AZ"
)

county <- unique(az$NAME)

az_catch <- az %>%
  filter(NAME %in% c(
    "Cochise County, Arizona",
    "Pima County, Arizona",
    "Pinal County, Arizona",
    "Santa Cruz County, Arizona",
    "Yuma County, Arizona"
  ))

v18 <- load_variables(2018, "acs5", cache = TRUE)

# total for each county is = "B01001I_001"

var_age <- c(
  "B01001I_001", # total estimate for each county
  "B01001I_013", # estimate for male 55-64 years for each county
  "B01001I_014", # estimate for male 65-74 years for each county
  "B01001I_015", # estimate for male 75-84 years for each county
  "B01001I_016", # estimate for male 85<= years for each county
  "B01001I_028", # estimate for female 55-64 years for each county
  "B01001I_029", # estimate for female 65-74 years for each county
  "B01001I_030", # estimate for female 75-84 years for each county
  "B01001I_031"
) # estimate for female 85<= years for each county

az_catch <- az_catch %>%
  group_by(NAME) %>%
  filter(variable %in% var_age)

var_totals <- az_catch %>%
  filter(variable == "B01001I_001")

az_catch <- az_catch %>%
  filter(variable != "B01001I_001") %>%
  summarise(hispanic_population = sum(estimate))

az_catch <- inner_join(az_catch, var_totals)

az_catch %>%
  mutate(percentage_hispanic = hispanic_population / estimate) %>%
  select(NAME, hispanic_population, estimate, percentage_hispanic) %>%
  kable()

az_catch %>%
  summarize(
    hispanic_total = sum(hispanic_population),
    catch_total = sum(estimate)
  ) %>%
  mutate(hispanic_percent = hispanic_total / catch_total) %>%
  kable()
